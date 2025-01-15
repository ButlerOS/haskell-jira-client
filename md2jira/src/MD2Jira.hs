module MD2Jira (
    Document (..),
    Epic (..),
    Story (..),
    CacheEntry (..),
    parse,
    printer,
    eval,
    showScore,

    -- * Test helpers
    toJira,
) where

import Control.Monad (when)
import Control.Monad.Catch (catch)
import Control.Monad.RWS.Strict qualified as RWS
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAsciiUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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
    { config :: DocumentConfig
    -- ^ From the Pandoc Meta
    , intro :: [P.Block]
    -- ^ Any content before the first heading
    , epics :: [Epic]
    }
    deriving (Show)

data DocumentConfig = EmptyConfig
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
    , description :: [P.Block]
    , mScore :: Maybe Float
    , updated :: Maybe CTime
    }
    deriving (Eq, Show, Generic)
instance ToJSON Story
instance FromJSON Story

-- | Parse a markdown
parse :: Text -> Either Text Document
parse inp = case P.runPure (P.readCommonMark opt inp) of
    Left err -> Left $ P.renderError err
    Right (P.Pandoc _meta blocks) -> do
        -- span consumes everything until the first top level heading
        let (intro, epicBlocks) = span (\case P.Header 1 _ _ -> False; _ -> True) blocks
        epics <- parseEpics [] epicBlocks
        let config = EmptyConfig
        pure $ Document config intro epics
  where
    opt = P.def{P.readerExtensions = pandocExts}

-- | Reformat the document.
printer :: Document -> Text
printer doc = case P.runPure (P.writeCommonMark writerOpt pdoc) of
    Left err -> P.renderError err
    Right txt -> addMeta txt
  where
    pdoc = P.Pandoc mempty (doc.intro <> P.walk inlineDanglingSpan body)
    addMeta = case doc.config of
        EmptyConfig -> id
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
    P.Header 2 (jiraIdTxt, _, attrs) htitle -> do
        mJira <- jiraP jiraIdTxt
        let mScore = scoreP attrs
            updated = dateP attrs
            title = P.stringify htitle
            -- span consumes everything until the next story or epic
            (description, remaining) = span (\case P.Header n _ _ | n < 3 -> False; _ -> True) rest
        parseStories (Story{mJira, title, description, mScore, updated} : acc) remaining
    -- A new epic or something else, stop the stories parser
    _ -> pure (reverse acc, x : rest)

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
    setEpics epics = doc{epics}
    goEpic :: Epic -> EvalT Epic
    goEpic epic
        | "Without Epic" `T.isInfixOf` epic.title = goEpicStory Nothing epic
        | otherwise = do
            let epicInfo = Jira.IssueData epic.title (toJira epic.description) Nothing
            mJira <- catchHttpError $ case epic.mJira of
                Nothing -> create Jira.Epic epicInfo
                Just jid -> update jid epicInfo
            case mJira of
                Nothing -> pure epic
                Just{} -> goEpicStory mJira epic

    goEpicStory :: Maybe JiraID -> Epic -> EvalT Epic
    goEpicStory mJira epic = do
        stories <- mapM (goStory mJira) epic.stories
        pure epic{stories, mJira}

    goStory :: Maybe JiraID -> Story -> EvalT Story
    goStory mEpicID story = do
        let info = Jira.IssueData story.title (toJira story.description) Nothing
        let issueType = maybe Jira.Story Jira.EpicStory mEpicID
        mJira <- catchHttpError $ case story.mJira of
            Nothing -> create issueType info
            Just jid -> update jid info
        pure $ case mJira of
            Nothing -> story
            Just{} -> Story mJira story.title story.description story.mScore story.updated

    -- TODO: add retry
    catchHttpError act = catch act \(e :: HttpException) -> do
        RWS.tell ["Network request failed: " <> T.pack (show e)]
        pure Nothing

    update jid issueData = do
        cache <- RWS.get
        let entry = CacheEntry issueData
        when (Map.lookup jid cache /= Just entry) do
            res <- lift do
                logger $ "Check if cache is up-to-date for " <> from jid
                Jira.getIssue client jid >>= \case
                    Left e -> pure $ Just $ "Couldn't read: " <> e
                    Right issue
                        | issueToData issue == issueData -> pure Nothing
                        | otherwise -> do
                            logger $ "Updating " <> from jid
                            Jira.updateIssue client jid issueData
            case res of
                Nothing -> do
                    RWS.modify (Map.insert jid entry)
                Just err -> RWS.tell ["Failed to update " <> from jid <> ": " <> err]

        pure (Just jid)

    create issueType issueData = do
        res <- lift do
            logger $ "Creating " <> T.pack (show issueType) <> " " <> issueData.summary
            Jira.createIssue client project issueType issueData
        case res of
            Left err -> do
                RWS.tell ["Failed to create " <> issueData.summary <> ": " <> err]
                pure Nothing
            Right jid -> do
                RWS.modify (Map.insert jid (CacheEntry issueData))
                pure (Just jid)

issueToData :: Jira.JiraIssue -> Jira.IssueData
issueToData issue =
    Jira.IssueData
        { summary = issue.summary
        , description = fromMaybe "" issue.description
        , assignee = issue.assignee
        }

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
    attr = (jiraAttr story.mJira, [], scoreAttr story.mScore <> dateAttr story.updated)

scoreAttr :: Maybe Float -> [(Text, Text)]
scoreAttr = \case
    Just f -> [("score", T.pack (showScore f))]
    Nothing -> []

dateAttr :: Maybe CTime -> [(Text, Text)]
dateAttr = \case
    Nothing -> []
    Just d -> [("updated", T.decodeUtf8 $ formatUnixTimeGMT "%Y-%m-%d" (UnixTime d 0))]

jiraAttr :: Maybe JiraID -> Text
jiraAttr = maybe "" into

showScore :: Float -> String
showScore score =
    let f = floor score
     in if f == ceiling score
            then show @Int f
            else show score

showT :: (Show a) => a -> Text
showT = T.pack . show
