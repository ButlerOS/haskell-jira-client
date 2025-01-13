module MD2Jira (
    Document (..),
    Epic (..),
    Story (..),
    Priority (..),
    Task (..),
    TaskStatus (..),
    CacheEntry (..),
    parse,
    printer,
    eval,
    showScore,

    -- * Test helpers
    toJira,
) where

import Control.Monad (forM_, void, when)
import Control.Monad.Catch (catch)
import Control.Monad.RWS.Strict qualified as RWS
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAsciiUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Read qualified as T
import Data.UnixTime (UnixTime (..), formatUnixTimeGMT, getUnixTime, parseUnixTimeGMT)
import Foreign.C (CTime)
import GHC.Generics (Generic)
import Jira (JiraID)
import Jira qualified
import Network.HTTP.Client (HttpException)
import Witch (from, into)

-- pandoc
import Text.Pandoc.Class qualified as P (runPure)
import Text.Pandoc.Definition qualified as P
import Text.Pandoc.Error qualified as P (renderError)
import Text.Pandoc.Extensions qualified as P
import Text.Pandoc.Options qualified as P
import Text.Pandoc.Readers qualified as P
import Text.Pandoc.Shared qualified as P (stringify)
import Text.Pandoc.Walk qualified as P (walk)
import Text.Pandoc.Writers qualified as P

data Document = Document
    { kind :: DocumentKind
    -- ^ From the Pandoc Meta
    , intro :: [P.Block]
    -- ^ Any content before the first heading
    , epics :: [Epic]
    }
    deriving (Show)

data DocumentKind
    = FullBacklog
    | AssignedStories Text
    deriving (Show)

data Epic = Epic
    { mJira :: Maybe JiraID
    , title :: Text
    , description :: [P.Block]
    , stories :: [Story]
    }
    deriving (Eq, Show, Generic)
instance ToJSON Epic
instance FromJSON Epic

data Story = Story
    { mJira :: Maybe JiraID
    , title :: Text
    , assigned :: [Text]
    , description :: [P.Block]
    , tasks :: [Task]
    , mScore :: Maybe Float
    , updated :: Maybe CTime
    , priority :: Maybe Priority
    }
    deriving (Eq, Show, Generic)
instance ToJSON Story
instance FromJSON Story

data Priority = Low | Medium | High
    deriving (Eq, Show, Generic, Ord)
instance ToJSON Priority
instance FromJSON Priority

data TaskStatus = Open | Closed
    deriving (Eq, Show, Generic)
instance ToJSON TaskStatus
instance FromJSON TaskStatus

data Task = Task
    { status :: TaskStatus
    , title :: Text
    , assigned :: [Text]
    , description :: [P.Block]
    }
    deriving (Eq, Show, Generic)
instance ToJSON Task
instance FromJSON Task

-- | Parse a markdown
parse :: Text -> Either Text Document
parse inp = case P.runPure (P.readCommonMark opt inp) of
    Left err -> Left $ P.renderError err
    Right (P.Pandoc meta blocks) -> do
        -- span consumes everything until the first top level heading
        let (intro, epicBlocks) = span (\case P.Header 1 _ _ -> False; _ -> True) blocks
        epics <- parseEpics [] epicBlocks
        let kind = case Map.lookup "assigned" meta.unMeta of
                Just (P.MetaInlines [P.Str s]) -> AssignedStories s
                Just (P.MetaInlines [P.Link _ [P.Str s] _]) -> AssignedStories s
                _ -> FullBacklog
        pure $ Document kind intro epics
  where
    opt = P.def{P.readerExtensions = pandocExts}

-- | Reformat the document.
printer :: Document -> Text
printer doc = case P.runPure (P.writeCommonMark writerOpt pdoc) of
    Left err -> P.renderError err
    Right txt -> addMeta txt
  where
    pdoc = P.Pandoc mempty (doc.intro <> P.walk inlineDanglingSpan body)
    addMeta = case doc.kind of
        FullBacklog -> id
        AssignedStories s -> mappend ("---\nassigned: " <> s <> "\n---\n\n")
    body = concatMap printerEpic doc.epics
    -- The writer ignores span attributes, so this convert them to a verbatim string.
    inlineDanglingSpan = \case
        P.Span ("", xs, []) [P.Space] -> P.Str $ " {" <> T.unwords (map (mappend ".") xs) <> "}"
        x -> x

pandocExts :: P.Extensions
pandocExts =
    P.extensionsFromList
        [ -- Decode attributes from '{...}'
          P.Ext_attributes
        , -- Decode task list from '- [ ] '
          P.Ext_task_lists
        , -- Decode front matter
          P.Ext_yaml_metadata_block
        , -- Decode code block
          P.Ext_backtick_code_blocks
        , -- Handle raw urls
          P.Ext_autolink_bare_uris
        ]

writerOpt :: P.WriterOptions
writerOpt = P.def{P.writerExtensions = pandocExts, P.writerWrapText = P.WrapPreserve}

-- | Parse a Jira-ID from the header id
jiraP :: Text -> Either Text (Maybe JiraID)
jiraP "" = pure Nothing
jiraP n =
    let name = T.takeWhile (\c -> c == '_' || isAsciiUpper c) n
     in case T.uncons (T.drop (T.length name) n) of
            Just ('-', rest) | Right (jid, "") <- T.decimal rest -> pure $ Just $ Jira.mkJiraID name jid
            _ -> Left $ "Bad JiraID: " <> n

-- | Parse the task's date from the header attrs
dateP :: [(Text, Text)] -> Maybe CTime
dateP xs = do
    dateTxt <- lookup "updated" xs
    case parseUnixTimeGMT "%Y-%m-%d" (T.encodeUtf8 dateTxt) of
        UnixTime 0 _ -> Nothing
        UnixTime ts _ -> pure ts

-- | Parse the task's score from the header attrs
scoreP :: [(Text, Text)] -> Maybe Float
scoreP xs = do
    scoreTxt <- lookup "score" xs
    case T.rational scoreTxt of
        Right (score, "") -> pure score
        _ -> Nothing

-- | Parse the story priority from the header attrs
priorityP :: [(Text, Text)] -> Maybe Priority
priorityP xs =
    lookup "prio" xs >>= \case
        "H" -> Just High
        "L" -> Just Low
        _ -> Nothing

-- | Parse the epics from the document body, consuming every top heading
parseEpics :: [Epic] -> [P.Block] -> Either Text [Epic]
parseEpics acc [] =
    -- The end of the document
    pure $ reverse acc
parseEpics acc (x : rest) = case x of
    -- A new epic appears
    P.Header 1 (jiraIdTxt, _, _attrs) htitle -> do
        mJira <- jiraP jiraIdTxt
        let title = P.stringify htitle
            -- span consumes everything until the first second heading
            (description, storyBlocks) = span (\case P.Header 2 _ _ -> False; _ -> True) rest
        (stories, remaining) <- parseStories [] storyBlocks
        -- continue to parse the epics
        parseEpics (Epic{mJira, title, description, stories} : acc) remaining
    _ -> Left $ "Invalid epic:" <> showT x <> showT rest

-- | Parse the stories from the epic body, returning the left over content
parseStories :: [Story] -> [P.Block] -> Either Text ([Story], [P.Block])
parseStories acc [] =
    -- The end of the document
    pure (reverse acc, [])
parseStories acc (x : rest) = case x of
    -- A new story appears
    P.Header 2 (jiraIdTxt, assigned, attrs) htitle -> do
        mJira <- jiraP jiraIdTxt
        let mScore = scoreP attrs
            updated = dateP attrs
            priority = priorityP attrs
            title = P.stringify htitle
            -- span consumes everything until the next story or epic
            (description, remaining) = span (\case P.Header n _ _ | n < 3 -> False; _ -> True) rest
            -- extract the tasks from the story's description
            tasks = foldMap parseStoryBody description
        parseStories (Story{mJira, title, assigned, description, tasks, mScore, updated, priority} : acc) remaining
    -- A new epic or something else, stop the stories parser
    _ -> pure (reverse acc, x : rest)

-- | Extract the tasks from bullet list
parseStoryBody :: P.Block -> [Task]
parseStoryBody = \case
    P.BulletList xs -> mapMaybe parseTask xs
    _ -> []
  where
    parseTask :: [P.Block] -> Maybe Task
    parseTask [] = Nothing
    parseTask (x : rest) = do
        (status, line) <- parseTaskTitle x
        -- span consumes everything until the end of line or the span attribute, e.g. {.TC}
        let (htitle, remaining) = span (\case P.SoftBreak -> False; P.Span{} -> False; _ -> True) line
        let title = P.stringify htitle
            (assigned, description) = case remaining of
                -- nothing is left after the title
                [] -> ([], [])
                -- a span attribute is present, extract the assigned class
                P.Span (_, as, _) [P.Space] : xs -> (as, P.Plain (trimInline xs) : rest)
                -- otherwise just append the rest
                _ -> ([], P.Plain (trimInline remaining) : rest)
        Just $ Task{title, status, assigned, description}

trimInline :: [P.Inline] -> [P.Inline]
trimInline = dropWhile \case
    P.SoftBreak -> True
    P.Space -> True
    _ -> False

-- | Extract the task status from a given buletted block
parseTaskTitle :: P.Block -> Maybe (TaskStatus, [P.Inline])
parseTaskTitle = \case
    P.Plain xs -> parseLine xs
    P.Para xs -> parseLine xs
    _ -> Nothing
  where
    parseLine = \case
        P.Str statusStr : P.Space : xs -> do
            status <- parseStatus statusStr
            pure (status, xs)
        _ -> Nothing

parseStatus :: Text -> Maybe TaskStatus
parseStatus = \case
    "☐" -> pure Open
    "☒" -> pure Closed
    _ -> Nothing

-- | Convert a pandoc definitions into a jira markup
toJira :: [P.Block] -> Text
toJira blocks = case P.runPure (P.writeJira writerOpt (P.Pandoc mempty body)) of
    Left err -> P.renderError err
    Right txt -> txt
  where
    body = P.walk removeSpan blocks
    -- Don't include the span attribute
    removeSpan = \case
        P.Span _ [P.Space] -> P.Str ""
        x -> x

type Cache = Map JiraID CacheEntry

data CacheEntry = CacheEntry
    { issue :: Jira.IssueData
    , transition :: Jira.Transition
    , completed :: Word
    }
    deriving (Eq, Show, Generic)
instance ToJSON CacheEntry
instance FromJSON CacheEntry

{- | The evaluation action, RWS stands for:
* Reader, the current time
* Writer, an error log
* State, the cache
-}
type EvalT a = RWS.RWST CTime [Text] Cache IO a

{- | This function is the core of md2jira:
* Create epics/story without ID
* Update issues when it differ from the cache

The function returns the updated issues, cache and a list of errors
-}
eval :: (Text -> IO ()) -> Jira.JiraClient -> Text -> Document -> Cache -> IO (Document, Cache, [Text])
eval logger client project doc cache' = do
    now <- getUnixTime
    RWS.runRWST (setEpics <$> mapM goEpic doc.epics) now.utSeconds cache'
  where
    docContainsEpic = case doc.kind of
        FullBacklog -> True
        AssignedStories{} -> False
    setEpics epics = doc{epics}
    goEpic :: Epic -> EvalT Epic
    goEpic epic
        | docContainsEpic = do
            let epicInfo = Jira.IssueData epic.title (toJira epic.description)
            mJira <- catchHttpError $ case epic.mJira of
                Nothing -> create [] Jira.Epic epicInfo
                Just jid -> update [] jid epicInfo
            case mJira of
                Nothing -> pure epic
                Just{} -> goEpicStory mJira epic
        | otherwise = goEpicStory Nothing epic

    goEpicStory :: Maybe JiraID -> Epic -> EvalT Epic
    goEpicStory mJira epic = do
        stories <- mapM (goStory mJira) epic.stories
        pure epic{stories, mJira}

    goStory :: Maybe JiraID -> Story -> EvalT Story
    goStory mEpicID story = do
        let info = Jira.IssueData story.title (toJira story.description)
        let issueType = maybe Jira.Story Jira.EpicStory mEpicID
        updated <- Just <$> storyUpdateDate story
        mJira <- catchHttpError $ case story.mJira of
            Nothing -> create story.tasks issueType info
            Just jid -> update story.tasks jid info
        pure $ case mJira of
            Nothing -> story
            Just{} -> Story mJira story.title story.assigned story.description story.tasks story.mScore updated story.priority

    storyUpdateDate :: Story -> EvalT CTime
    storyUpdateDate story = case story.mJira of
        -- This is a new story, use the current date
        Nothing -> RWS.ask
        Just jid -> do
            cache <- RWS.get
            case Map.lookup jid cache of
                Just entry
                    | -- If the number of completed task remained the same, then don't update the date
                      Just lastUpdate <- story.updated
                    , entry.completed == storyCompletion story.tasks ->
                        pure lastUpdate
                -- otherwise update to the current date
                _ -> RWS.ask

    -- TODO: add retry
    catchHttpError act = catch act \(e :: HttpException) -> do
        RWS.tell ["Network request failed: " <> T.pack (show e)]
        pure Nothing

    update tasks jid issueData = do
        cache <- RWS.get
        let transition = storyTransition tasks
        let entry = CacheEntry issueData transition (storyCompletion tasks)
        when (Map.lookup jid cache /= Just entry) do
            res <- lift do
                logger $ "Updating " <> from jid
                Jira.updateIssue client jid issueData
            case res of
                Nothing -> do
                    doTransition jid transition
                    RWS.modify (Map.insert jid entry)
                Just err -> RWS.tell ["Failed to update " <> from jid <> ": " <> err]

        pure (Just jid)

    doTransition :: JiraID -> Jira.Transition -> EvalT ()
    doTransition jid currentTransition = do
        cache <- RWS.get
        let mTransition = case Map.lookup jid cache of
                Just (CacheEntry _ prevTransition _)
                    | -- The transition changed since the last time
                      prevTransition /= currentTransition ->
                        Just currentTransition
                    | otherwise -> Nothing
                -- This is a new story, perform the transition
                _ -> Just currentTransition
        forM_ mTransition \transition ->
            void $ catchHttpError do
                res <- lift do
                    logger $ "Transitioning " <> from jid <> ", to: " <> T.pack (show transition)
                    Jira.doTransition client jid transition
                case res of
                    Nothing -> pure ()
                    Just err -> RWS.tell ["Failed to transition " <> from jid <> ": " <> err]
                pure Nothing

    create tasks issueType issueData = do
        res <- lift do
            logger $ "Creating " <> T.pack (show issueType) <> " " <> issueData.summary
            Jira.createIssue client project issueType issueData
        case res of
            Left err -> do
                RWS.tell ["Failed to create " <> issueData.summary <> ": " <> err]
                pure Nothing
            Right jid -> do
                let transition = storyTransition tasks
                    completed = storyCompletion tasks
                when (transition /= openT) do
                    -- A new story which already is in progress or completed
                    doTransition jid transition
                RWS.modify (Map.insert jid (CacheEntry issueData transition completed))
                pure (Just jid)

-- | Reformat the document.
printerEpic :: Epic -> [P.Block]
printerEpic epic =
    (P.Header 1 attr [P.Str epic.title] : epic.description)
        <> concatMap printerStory epic.stories
  where
    attr = (jiraAttr epic.mJira, [], [])

printerStory :: Story -> [P.Block]
printerStory story =
    P.Header 2 attr [P.Str story.title] : story.description
  where
    attr = (jiraAttr story.mJira, story.assigned, scoreAttr story.mScore <> dateAttr story.updated <> prioAttr story.priority)

scoreAttr :: Maybe Float -> [(Text, Text)]
scoreAttr = \case
    Just f -> [("score", T.pack (showScore f))]
    Nothing -> []

dateAttr :: Maybe CTime -> [(Text, Text)]
dateAttr = \case
    Nothing -> []
    Just d -> [("updated", T.decodeUtf8 $ formatUnixTimeGMT "%Y-%m-%d" (UnixTime d 0))]

prioAttr :: Maybe Priority -> [(Text, Text)]
prioAttr = \case
    Nothing -> []
    Just Medium -> []
    Just High -> [("prio", "H")]
    Just Low -> [("prio", "L")]

jiraAttr :: Maybe JiraID -> Text
jiraAttr = maybe "" into

showScore :: Float -> String
showScore score =
    let f = floor score
     in if f == ceiling score
            then show @Int f
            else show score

-- | TODO: fetch that from the project, it is in the `GET api/2/issue/NAME/transitions` endpoint
openT, wipT, doneT :: Jira.Transition
openT = Jira.Transition 11
wipT = Jira.Transition 21
doneT = Jira.Transition 51

-- | Return the number of completed task
storyCompletion :: [Task] -> Word
storyCompletion = fromIntegral . length . filter (\t -> t.status == Closed)

-- | Get the status of a story given a list of task
storyTransition :: [Task] -> Jira.Transition
storyTransition tasks
    | null tasks || allTasksAre Open = openT
    | allTasksAre Closed = doneT
    | otherwise = wipT
  where
    allTasksAre s = all (\task -> task.status == s) tasks

showT :: (Show a) => a -> Text
showT = T.pack . show
