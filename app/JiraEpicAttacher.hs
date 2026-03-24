{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module JiraEpicAttacher where

import Data.Aeson
import Data.Text (Text)
import Jira
import System.Environment
import Witch

main :: IO ()
main =
    getArgs >>= \case
        [from -> project_name, read -> story_nr, read -> epic_nr] -> do
            client <- Jira.newJiraClientFromEnv "https://issues.redhat.com"
            putStrLn "Epic Attacher Ready"
            setEpic client (mkJiraID project_name story_nr) (mkJiraID project_name epic_nr) >>= \case
                Just err -> putStrLn $ "Failed: " <> from err
                Nothing -> putStrLn "Done!"
        _ -> putStrLn "usage: story-id epic-id"

setEpic :: JiraClient -> JiraID -> JiraID -> IO (Maybe Text)
setEpic client jid parentId = Jira.updateIssueFields client jid ["customfield_12311140" .= into @Text parentId]
