module MD2Jira (Epic (..), Story (..), Task (..), TaskStatus (..), parse, printer, eval) where

import Control.Applicative (many, optional, (<|>))
import Control.Monad (when)
import Control.Monad.Catch (catch)
import Control.Monad.RWS.Strict qualified as RWS
import Control.Monad.Trans.Class (lift)
import Data.Attoparsec.Text qualified as P
import Data.Char (isAsciiUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Jira (JiraID)
import Jira qualified
import Network.HTTP.Client (HttpException)
import Witch (from)

data Epic = Epic
    { mJira :: Maybe JiraID
    , info :: Jira.IssueData
    , stories :: [Story]
    }
    deriving (Eq, Show)

data Story = Story
    { mJira :: Maybe JiraID
    , info :: Jira.IssueData
    , tasks :: [Task]
    }
    deriving (Eq, Show)

data TaskStatus = Todo | InProgress | Done
    deriving (Eq, Show)

data Task = Task
    { status :: TaskStatus
    , info :: Jira.IssueData
    }
    deriving (Eq, Show)

-- | Parse a 'Epic'
epicP :: P.Parser Epic
epicP =
    P.string "# " *> do
        Epic <$> optional jiraP <*> issueP (== '#') <*> many storyP

-- | Parse a 'Story'
storyP :: P.Parser Story
storyP =
    P.string "## " *> do
        Story <$> optional jiraP <*> issueP (`elem` ['#', '-']) <*> many taskP

issueP :: (Char -> Bool) -> P.Parser Jira.IssueData
issueP stopChar = Jira.IssueData <$> titleP <*> bodyP stopChar

taskP :: P.Parser Task
taskP = Task <$> statusP <*> issueP (`elem` ['#', '-'])
  where
    statusP :: P.Parser TaskStatus
    statusP =
        (Done <$ P.string "- [x]")
            <|> (Todo <$ P.string "- [ ]")
            <|> (InProgress <$ P.string "- [.]")

-- | Parse a title
titleP :: P.Parser Text
titleP = P.takeWhile (/= '\n')

-- | Parse a 'JiraID'
jiraP :: P.Parser JiraID
jiraP = do
    name <- P.takeWhile (\c -> c == '_' || isAsciiUpper c)
    _ <- P.string "-"
    (Jira.mkJiraID name <$> P.decimal) <* P.string " "

-- | Parse a body, until the next heading.
bodyP :: (Char -> Bool) -> P.Parser Text
bodyP stopChar = P.scan False go
  where
    go True c | stopChar c = Nothing
    go _ '\n' = Just True
    go _ _ = Just False

{- | Parse a markdown

>>> parse $ T.unlines ["# RHOS-45 toto", "body", "body2", "## test", "body story", "## testy 2"]
Right [Epic {mJira = Just "RHOS-45", info = IssueData {summary = "toto", description = "body\nbody2"}, stories = [Story {mJira = Nothing, info = IssueData {summary = "test", description = "body story"}},Story {mJira = Nothing, info = IssueData {summary = "testy 2", description = ""}}]}]
-}
parse :: Text -> Either String [Epic]
parse = P.parseOnly (many epicP <* P.endOfInput)

type Cache = Map JiraID Jira.IssueData

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
            Nothing -> create Jira.Epic epic.info
            Just jid -> update jid epic.info
        case mJira of
            Nothing -> pure epic
            Just jid -> do
                stories <- mapM (goStory jid) epic.stories
                pure epic{stories, mJira}

    goStory :: JiraID -> Story -> EvalT Story
    goStory epicID story = do
        let info = storyData story
        mJira <- catchHttpError $ case story.mJira of
            Nothing -> create (Jira.EpicStory epicID) info
            Just jid -> update jid info
        pure $ Story mJira story.info story.tasks

    -- TODO: add retry
    catchHttpError act = catch act \(e :: HttpException) -> do
        RWS.tell ["Network request failed: " <> T.pack (show e)]
        pure Nothing

    update jid issueData = do
        cache <- RWS.get
        when (Map.lookup jid cache /= Just issueData) do
            res <- lift do
                logger $ "Updating " <> from jid
                Jira.updateIssue client jid issueData
            case res of
                Nothing -> RWS.modify (Map.insert jid issueData)
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
                RWS.modify (Map.insert jid issueData)
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
        InProgress -> "[.]"
        Done -> "[x]"

-- | Adds the task back into the description
storyData :: Story -> Jira.IssueData
storyData story = Jira.IssueData{summary = story.info.summary, description}
  where
    description = story.info.description <> foldMap printTask story.tasks
