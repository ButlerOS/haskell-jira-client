module MD2Jira (Epic (..), Story (..), Task (..), TaskStatus (..), parse, printer, eval) where

import Control.Applicative (many, optional, (<|>))
import Control.Monad (unless, void, when)
import Control.Monad.Catch (catch)
import Control.Monad.RWS.Strict qualified as RWS
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAsciiUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Jira (JiraID)
import Jira qualified
import Network.HTTP.Client (HttpException)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Witch (from)

data Epic = Epic
    { mJira :: Maybe JiraID
    , info :: Jira.IssueData
    , stories :: [Story]
    }
    deriving (Eq, Show, Generic)
instance ToJSON Epic
instance FromJSON Epic

data Story = Story
    { mJira :: Maybe JiraID
    , info :: Jira.IssueData
    , tasks :: [Task]
    }
    deriving (Eq, Show, Generic)
instance ToJSON Story
instance FromJSON Story

data TaskStatus = Todo | InProgress {assigned :: Text} | Done
    deriving (Eq, Show, Generic)
instance ToJSON TaskStatus
instance FromJSON TaskStatus

-- | TODO: fetch that from the project, it is in the `GET api/2/issue/NAME/transitions` endpoint
taskTransition :: TaskStatus -> Jira.Transition
taskTransition = \case
    Todo -> Jira.Transition 11
    InProgress{} -> Jira.Transition 21
    Done -> Jira.Transition 51

data Task = Task
    { status :: TaskStatus
    , info :: Jira.IssueData
    }
    deriving (Eq, Show, Generic)
instance ToJSON Task
instance FromJSON Task

type Parser = P.Parsec Void Text

-- | Parse a 'Epic'
epicP :: Parser Epic
epicP =
    "# " *> do
        Epic <$> optional jiraP <*> issueP (== '#') <*> many storyP

-- | Parse a 'Story'
storyP :: Parser Story
storyP =
    "## " *> do
        Story <$> optional jiraP <*> issueP (`elem` ['#', '-']) <*> many taskP

issueP :: (Char -> Bool) -> Parser Jira.IssueData
issueP stopChar = Jira.IssueData <$> titleP <*> bodyP stopChar

taskP :: Parser Task
taskP = Task <$> statusP <*> issueP (`elem` ['#', '-'])
  where
    statusP :: Parser TaskStatus
    statusP =
        (Done <$ "- [x]")
            <|> (Todo <$ ("- []" <|> "- [ ]"))
            <|> (InProgress <$> assignedP)

assignedP :: Parser Text
assignedP = "- [" *> P.takeWhile1P (Just "assigned") (/= ']') <* "]"

-- | Parse a title
titleP :: Parser Text
titleP = P.takeWhile1P (Just "title") (/= '\n')

-- | Parse a 'JiraID'
jiraP :: Parser JiraID
jiraP = do
    name <- P.takeWhile1P (Just "project") (\c -> c == '_' || isAsciiUpper c)
    _ <- "-"
    (Jira.mkJiraID name <$> P.decimal) <* " "

-- | Parse a body, until the next heading.
bodyP :: (Char -> Bool) -> Parser Text
bodyP stopChar = go []
  where
    go :: [Text] -> Parser Text
    go acc = do
        line <- P.takeWhileP Nothing (/= '\n')
        cr <- P.optional (P.satisfy (== '\n'))
        let linecr = case cr of
                Nothing -> line
                Just{} -> T.snoc line '\n'
        isEnd <- P.lookAhead ((True <$ P.satisfy stopChar) <|> P.atEnd)
        if isEnd
            then pure $ mconcat $ reverse $ linecr : acc
            else go (linecr : acc)

{- | Parse a markdown

>>> parse $ T.unlines ["# RHOS-45 toto", "body", "body2", "## test", "body story", "## testy 2"]
Right [Epic {mJira = Just "RHOS-45", info = IssueData {summary = "toto", description = "body\nbody2"}, stories = [Story {mJira = Nothing, info = IssueData {summary = "test", description = "body story"}},Story {mJira = Nothing, info = IssueData {summary = "testy 2", description = ""}}]}]
-}
parse :: FilePath -> Text -> Either String [Epic]
parse fp inp = case P.parse (P.many epicP <* P.eof) fp inp of
    Left err -> Left $ P.errorBundlePretty err
    Right epics -> pure epics

type Cache = Map JiraID (Jira.IssueData, [Task])

{- | The evaluation action, RWS stands for:
* Reader, a jira client
* Writer, an error log
* State, the cache
-}
type EvalT a = RWS.RWST Jira.JiraClient [Text] Cache IO a

{- | This function is the core of md2jira:
* Create epics/story without ID
* Update issues when it differ from the cache

The function returns the updated issues, cache and a list of errors
-}
eval :: (Text -> IO ()) -> Jira.JiraClient -> Text -> [Epic] -> Cache -> IO ([Epic], Cache, [Text])
eval logger client project epics = RWS.runRWST (mapM goEpic epics) client
  where
    goEpic :: Epic -> EvalT Epic
    goEpic epic = do
        mJira <- catchHttpError $ case epic.mJira of
            Nothing -> create [] Jira.Epic epic.info
            Just jid -> update [] jid epic.info
        case mJira of
            Nothing -> pure epic
            Just jid -> do
                stories <- mapM (goStory jid) epic.stories
                pure epic{stories, mJira}

    goStory :: JiraID -> Story -> EvalT Story
    goStory epicID story = do
        let info = storyData story
        mJira <- catchHttpError $ case story.mJira of
            Nothing -> create story.tasks (Jira.EpicStory epicID) info
            Just jid -> update story.tasks jid info
        pure $ case mJira of
            Nothing -> story
            Just{} -> Story mJira story.info story.tasks

    -- TODO: add retry
    catchHttpError act = catch act \(e :: HttpException) -> do
        RWS.tell ["Network request failed: " <> T.pack (show e)]
        pure Nothing

    update tasks jid issueData = do
        cache <- RWS.get
        when (Map.lookup jid cache /= Just (issueData, tasks)) do
            res <- lift do
                logger $ "Updating " <> from jid
                Jira.updateIssue client jid issueData
            case res of
                Nothing -> do
                    unless (null tasks) $ doTransition cache jid tasks
                    RWS.modify (Map.insert jid (issueData, tasks))
                Just err -> RWS.tell ["Failed to update " <> from jid <> ": " <> err]

        pure (Just jid)

    doTransition :: Cache -> JiraID -> [Task] -> EvalT ()
    doTransition cache jid tasks = do
        let currentTransition = storyTransition tasks
        let mTransition = case Map.lookup jid cache of
                Nothing -> Just currentTransition
                Just (_, prevTasks)
                    | storyTransition prevTasks /= currentTransition -> Just currentTransition
                    | otherwise -> Nothing
        void $ catchHttpError do
            case mTransition of
                Just transition -> do
                    res <- lift do
                        logger $ "Transitioning " <> from jid <> ", to: " <> T.pack (show transition)
                        Jira.doTransition client jid transition
                    case res of
                        Nothing -> pure ()
                        Just err -> RWS.tell ["Failed to transition " <> from jid <> ": " <> err]
                Nothing -> pure ()
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
                RWS.modify (Map.insert jid (issueData, tasks))
                pure (Just jid)

-- | Reformat the document.
printer :: [Epic] -> Text
printer = foldMap printEpic
  where
    printEpic :: Epic -> Text
    printEpic epic =
        mconcat
            [ "# " <> printTitle epic.mJira epic.info.summary
            , epic.info.description
            , foldMap printStory epic.stories
            ]

    printStory :: Story -> Text
    printStory story =
        mconcat
            [ "## " <> printTitle story.mJira issueData.summary
            , issueData.description
            ]
      where
        issueData = storyData story

    printTitle mJira title = printJira mJira <> title
    printJira Nothing = ""
    printJira (Just jid) = from jid <> " "

printTask :: Task -> Text
printTask task =
    mconcat
        [ "- " <> printTaskStatus task.status <> task.info.summary
        , task.info.description
        ]
  where
    printTaskStatus = \case
        Todo -> "[ ]"
        InProgress{assigned} -> "[" <> assigned <> "]"
        Done -> "[x]"

-- | Adds the task back into the description
storyData :: Story -> Jira.IssueData
storyData story = Jira.IssueData{summary = story.info.summary, description}
  where
    description = story.info.description <> foldMap printTask story.tasks

-- | Get the status of a story given a list of task
storyTransition :: [Task] -> Jira.Transition
storyTransition tasks = taskTransition status
  where
    status
        | allTasksAre Done = Done
        | allTasksAre Todo = Todo
        | -- For transition, the assignment is not relevant
          otherwise =
            InProgress "."
    allTasksAre s = all (\task -> task.status == s) tasks
