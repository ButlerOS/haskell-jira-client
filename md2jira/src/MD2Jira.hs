module MD2Jira (
    Document (..),
    Epic (..),
    Story (..),
    CacheEntry (..),
    parse,
    printer,
    eval,
    showPoints,

    -- * Test helpers
    toJira,
) where

import Control.Monad (unless, when)
import Control.Monad.Catch (catch)
import Control.Monad.RWS.Strict qualified as RWS
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first, second)
import Data.Char (isAsciiUpper)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Read qualified as T
import Data.UnixTime (UnixTime (..), formatUnixTimeGMT, getUnixTime, parseUnixTimeGMT)
import Data.Yaml qualified as YAML
import Foreign.C (CTime)
import GHC.Generics (Generic)
import Jira (JiraID, Sprint, SprintName (..))
import Jira qualified
import Network.HTTP.Client (HttpException)
import Witch (from, into)

-- pandoc

import Data.Foldable (forM_)
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

newtype DocumentConfig = DocumentConfig
    { users :: Maybe (Map Text Text)
    }
    deriving (Show, Generic)
instance FromJSON DocumentConfig
instance ToJSON DocumentConfig

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

data Story = Story
    { mJira :: Maybe JiraID
    , title :: Text
    , status :: Maybe StoryStatus
    , description :: [P.Block]
    , assignee :: Maybe Text
    , points :: Maybe Float
    , sprint :: Maybe SprintName
    , updated :: Maybe CTime
    }
    deriving (Eq, Show, Generic)

-- | Parse a markdown
parse :: Text -> Either Text Document
parse inp = do
    (inpBody, mConfig) <- parseFrontMatter inp
    let config = fromMaybe (DocumentConfig mempty) mConfig
    case P.runPure (P.readCommonMark opt inpBody) of
        Left err -> Left $ P.renderError err
        Right (P.Pandoc _ blocks) -> do
            -- span consumes everything until the first top level heading
            let (intro, epicBlocks) = span (\case P.Header 1 _ _ -> False; _ -> True) blocks
            epics <- parseEpics [] epicBlocks
            pure $ Document config intro epics
  where
    opt = P.def{P.readerExtensions = pandocExts}

-- | Consume the front matter and return the document body
parseFrontMatter :: (FromJSON a) => Text -> Either Text (Text, Maybe a)
parseFrontMatter inp
    | "---" `T.isPrefixOf` inp = case span (/= "---") (T.lines (T.drop 3 inp)) of
        (header, "---" : body) -> case YAML.decodeEither' (T.encodeUtf8 $ T.unlines header) of
            Left e -> Left $ "Header decoding error: " <> T.pack (show e)
            Right conf -> pure (T.unlines body, Just conf)
        v -> Left $ "Couldn't find header:" <> T.pack (show v)
    | otherwise = Right (inp, Nothing)

-- | Reformat the document.
printer :: Document -> Text
printer doc = case P.runPure (P.writeCommonMark writerOpt pdoc) of
    Left err -> P.renderError err
    Right txt -> addFrontMatter txt
  where
    pdoc = P.Pandoc mempty (doc.intro <> P.walk inlineDanglingSpan body)
    addFrontMatter
        | doc.config.users == mempty = id
        | otherwise = mappend ("---\n" <> T.decodeUtf8 (YAML.encode doc.config) <> "---\n\n")

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
        , -- Handle footnotes '[^1]'
          P.Ext_footnotes
        ]

writerOpt :: P.WriterOptions
writerOpt = P.def{P.writerExtensions = pandocExts, P.writerWrapText = P.WrapPreserve, P.writerReferenceLinks = True, P.writerReferenceLocation = P.EndOfSection}

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

-- | Parse the task's points from the header attrs
pointsP :: [(Text, Text)] -> Maybe Float
pointsP xs = do
    pointsTxt <- lookup "points" xs
    case T.rational pointsTxt of
        Right (points, "") -> pure points
        _ -> Nothing

-- | Parse the task's sprint from the header attrs
sprintP :: [(Text, Text)] -> Maybe SprintName
sprintP xs = Jira.SprintName <$> lookup "sprint" xs

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
        status <- case lookup "status" attrs of
            Nothing -> pure Nothing
            Just n -> Just <$> statusP n
        let points = pointsP attrs
            sprint = sprintP attrs
            updated = dateP attrs
            title = P.stringify htitle
            assignee = lookup "assignee" attrs
            -- span consumes everything until the next story or epic
            (description, remaining) = span (\case P.Header n _ _ | n < 3 -> False; _ -> True) rest
        parseStories (Story{mJira, title, status, description, assignee, points, sprint, updated} : acc) remaining
    -- A new epic or something else, stop the stories parser
    _ -> pure (reverse acc, x : rest)

-- | A header to inject at the top of the stories
infoHeader :: P.Inline
infoHeader = P.Emph [P.Str "This description is autogenerated by", P.Space, link, P.Space, P.Str "from source"]
  where
    link = P.Link ("", [], []) [P.Str "md2jira"] (url, "md2jira")
    url = "https://github.com/ButlerOS/haskell-jira-client/tree/main/md2jira#readme"

-- | Convert a pandoc definitions into a jira markup
toJira :: [P.Block] -> Text
toJira blocks = T.stripEnd $ case P.runPure (P.writeJira writerOpt (P.Pandoc mempty body)) of
    Left err -> P.renderError err
    Right txt -> txt
  where
    body = P.Para [infoHeader] : P.walk removeSpan blocks
    -- Don't include the span attribute
    removeSpan = \case
        P.Span _ [P.Space] -> P.Str ""
        x -> x

type Cache = Map JiraID CacheEntry

data CacheEntry = CacheEntry
    { issue :: Jira.IssueData
    , status :: Maybe Text
    , score :: Maybe Float
    , sprint :: Maybe SprintName
    , parent :: Maybe JiraID
    }
    deriving (Eq, Show, Generic)
instance ToJSON CacheEntry
instance FromJSON CacheEntry

{- | The evaluation action, RWS stands for:
* Reader, the current time
* Writer, an error log
* State, the cache
-}
type EvalT a = RWS.RWST CTime [Text] (Maybe [Sprint], Cache) IO a

{- | This function is the core of md2jira:
* Create epics/story without ID
* Update issues when it differ from the cache

The function returns the updated issues, cache and a list of errors
-}
eval :: (Text -> IO ()) -> Jira.JiraClient -> Text -> Maybe Jira.Board -> Document -> Cache -> IO (Document, Cache, [Text])
eval logger client project mBoard doc cache' = do
    now <- getUnixTime
    dropSprint <$> RWS.runRWST (setEpics <$> mapM goEpic doc.epics) now.utSeconds (Nothing, cache')
  where
    -- Sprints are only used during the internal eval, we don't give them back to the caller
    dropSprint (doc', (_, cache), err) = (doc', cache, err)
    setEpics epics = doc{epics}
    goEpic :: Epic -> EvalT Epic
    goEpic epic
        | "without epic" `T.isInfixOf` T.toLower epic.title = goEpicStory Nothing epic
        | otherwise = do
            let epicInfo = Jira.IssueData epic.title (toJira epic.description) Nothing
            mJira <- catchHttpError $ case epic.mJira of
                Nothing -> create Jira.Epic epicInfo
                Just jid -> update jid epicInfo Nothing Nothing Nothing Nothing
            case mJira of
                Nothing -> pure epic
                Just{} -> goEpicStory mJira epic

    goEpicStory :: Maybe JiraID -> Epic -> EvalT Epic
    goEpicStory mJira epic = do
        stories <- mapM (goStory mJira) epic.stories
        pure epic{stories, mJira}

    goStory :: Maybe JiraID -> Story -> EvalT Story
    goStory mEpicID story = withAssignee story \assignee -> do
        let info = Jira.IssueData story.title (toJira story.description) assignee
        let issueType = maybe Jira.Story Jira.EpicStory mEpicID
        mJira <- catchHttpError $ case story.mJira of
            Nothing -> create issueType info
            Just jid -> update jid info story.status story.points story.sprint mEpicID
        pure $ case mJira of
            Nothing -> story
            Just{} -> Story mJira story.title story.status story.description story.assignee story.points story.sprint story.updated

    withAssignee :: Story -> (Maybe Text -> EvalT Story) -> EvalT Story
    withAssignee story cb = case story.assignee of
        Just assignee -> case Map.lookup assignee (fromMaybe mempty doc.config.users) of
            Just username -> cb $ Just username
            Nothing -> do
                RWS.tell ["Unknown user: " <> assignee <> ", it needs to be added to the front-matter users dict"]
                pure story
        Nothing -> cb Nothing

    -- TODO: add retry
    catchHttpError :: EvalT (Maybe a) -> EvalT (Maybe a)
    catchHttpError act = catch act \(e :: HttpException) -> do
        RWS.tell ["Network request failed: " <> T.pack (show e)]
        pure Nothing

    update :: JiraID -> Jira.IssueData -> Maybe StoryStatus -> Maybe Float -> Maybe SprintName -> Maybe JiraID -> EvalT (Maybe JiraID)
    update jid issueData mStatus mPoints mSprint mParent = do
        cache <- snd <$> RWS.get
        let entry = CacheEntry issueData (statusName <$> mStatus) mPoints mSprint mParent
        when (Map.lookup jid cache /= Just entry) do
            res <- lift $ runExceptT do
                -- check cache
                lift $ logger $ from jid <> ": check if cache is up-to-date"
                issue <-
                    lift (Jira.getIssue client jid) >>= \case
                        Left e -> throwE $ "Couldn't read: " <> e
                        Right issue -> pure issue

                -- check status
                forM_ mStatus \status ->
                    when (statusName status /= issue.status) do
                        updateJira jid "status" $ Jira.doTransition client jid $ issueTransition status

                -- check points
                forM_ mPoints \points ->
                    when (Just points /= issue.score) do
                        updateJira jid "points" $ Jira.setIssueScore client jid points

                -- check description/assignee
                when (issueData /= issueToData issue) do
                    updateJira jid "description" $ Jira.updateIssue client jid issueData

                -- check parent
                forM_ mParent \parent ->
                    when (Just parent /= issue.parent) do
                        updateJira jid "parent" $ Jira.setIssueParent client jid parent

                pure issue

            case res of
                Right issue -> do
                    -- check sprints
                    forM_ mSprint \sprint ->
                        unless (sprint `elem` issue.sprints) $ trySprintUpdate jid sprint
                    RWS.modify $ second $ Map.insert jid entry
                Left err -> RWS.tell ["Failed to update " <> from jid <> ": " <> err]

        pure (Just jid)

    updateJira :: JiraID -> Text -> IO (Maybe Text) -> ExceptT Text IO ()
    updateJira jid name action = do
        lift $ logger $ from jid <> ": update " <> name
        lift action >>= \case
            Just err -> throwE $ name <> " update failed: " <> err
            Nothing -> pure ()

    -- Set the sprint attribute when found in the board, otherwise just print a warning.
    trySprintUpdate :: JiraID -> SprintName -> EvalT ()
    trySprintUpdate jid sprintName = forM_ mBoard \board -> do
        sprints <- getSprints board
        case find (\s -> sprintMatch sprintName s.name) sprints of
            Nothing -> lift $ logger $ from sprintName <> ": unknown sprint!"
            Just sprint -> do
                lift $ logger $ from jid <> ": updating sprint to " <> from sprintName
                lift (Jira.setIssueSprint client jid sprint.id) >>= \case
                    Nothing -> pure ()
                    Just err -> RWS.tell ["Failed to set sprint to " <> from (show sprint) <> ": " <> err]

    -- Get the list of sprint from the board, and keep them in the internal state
    getSprints :: Jira.Board -> EvalT [Jira.Sprint]
    getSprints board = do
        (mSprints, _) <- RWS.get
        case mSprints of
            -- Sprints were already looked up.
            Just xs -> pure xs
            Nothing -> do
                eSprints <- lift do
                    logger $ from (show board) <> ": fetching sprint names"
                    Jira.getSprints client board
                -- Report error if sprints can't be looked up.
                sprints <- case eSprints of
                    Left err -> RWS.tell ["Failed to get sprints " <> from (show board) <> ": " <> err] >> pure []
                    Right xs -> pure xs
                -- Store the result in the internal states
                RWS.modify $ first $ const (Just sprints)
                pure sprints

    create :: Jira.IssueType -> Jira.IssueData -> EvalT (Maybe JiraID)
    create issueType issueData = do
        res <- lift do
            logger $ "Creating " <> T.pack (show issueType) <> " " <> issueData.summary
            Jira.createIssue client project issueType issueData
        let mParent = case issueType of
                Jira.EpicStory jid -> Just jid
                _ -> Nothing
        case res of
            Left err -> do
                RWS.tell ["Failed to create " <> issueData.summary <> ": " <> err]
                pure Nothing
            Right jid -> do
                RWS.modify $ second $ Map.insert jid (CacheEntry issueData Nothing Nothing Nothing mParent)
                pure (Just jid)

data StoryStatus = Backlog | Todo | WIP | Review | Done
    deriving (Eq, Show, Generic)

sprintMatch :: SprintName -> SprintName -> Bool
sprintMatch (SprintName s1) (SprintName s2) = s1 == s2 || "Sprint " <> s1 == s2

-- | TODO: fetch that from the project, it is in the `GET /rest/api/2/issue/NAME/transitions` endpoint
issueTransition :: StoryStatus -> Jira.Transition
issueTransition = \case
    Backlog -> Jira.Transition 21
    Todo -> Jira.Transition 31
    WIP -> Jira.Transition 41
    Review -> Jira.Transition 51
    Done -> Jira.Transition 61

statusName :: StoryStatus -> Text
statusName = \case
    Backlog -> "Backlog"
    Todo -> "To Do"
    WIP -> "In Progress"
    Review -> "Review"
    Done -> "Closed"

-- Note: don't forget to update the 'statusP' parser if the StoryStatus data type changes!
statusShortName :: StoryStatus -> Text
statusShortName = \case
    Backlog -> "backlog"
    Todo -> "todo"
    WIP -> "wip"
    Review -> "review"
    Done -> "done"

-- | Parse the story's status from the header attrs
statusP :: Text -> Either Text StoryStatus
statusP = \case
    "backlog" -> Right Backlog
    "todo" -> Right Todo
    "wip" -> Right WIP
    "done" -> Right Done
    "review" -> Right Review
    e -> Left $ "Unknown status: " <> e

-- | Convert a remote issue to the local cache format to check if it needs to be updated.
issueToData :: Jira.JiraIssue -> Jira.IssueData
issueToData issue =
    Jira.IssueData
        { summary = issue.summary
        , description = maybe mempty T.stripEnd issue.description
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
    attr = (jiraAttr story.mJira, [], pointsAttr story.points <> sprintAttr story.sprint <> dateAttr story.updated <> statusAttr story.status <> assigneeAttr story.assignee)

statusAttr :: Maybe StoryStatus -> [(Text, Text)]
statusAttr = \case
    Just status -> [("status", statusShortName status)]
    Nothing -> []

assigneeAttr :: Maybe Text -> [(Text, Text)]
assigneeAttr = \case
    Just s -> [("assignee", s)]
    Nothing -> []

pointsAttr :: Maybe Float -> [(Text, Text)]
pointsAttr = \case
    Just f -> [("points", T.pack (showPoints f))]
    Nothing -> []

sprintAttr :: Maybe SprintName -> [(Text, Text)]
sprintAttr = \case
    Just (Jira.SprintName s) -> [("sprint", s)]
    Nothing -> []

dateAttr :: Maybe CTime -> [(Text, Text)]
dateAttr = \case
    Nothing -> []
    Just d -> [("updated", T.decodeUtf8 $ formatUnixTimeGMT "%Y-%m-%d" (UnixTime d 0))]

jiraAttr :: Maybe JiraID -> Text
jiraAttr = maybe "" into

showPoints :: Float -> String
showPoints points =
    let f = floor points
     in if f == ceiling points
            then show @Int f
            else show points

showT :: (Show a) => a -> Text
showT = T.pack . show
