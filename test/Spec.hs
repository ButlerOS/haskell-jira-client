module Main (main) where

import Data.ByteString qualified as BS
import Data.Either (partitionEithers)
import Network.HTTP.Mock (withMockedManager)
import Network.HTTP.Types.Status (status200)
import Network.Wai qualified as Wai
import Test.Tasty
import Test.Tasty.HUnit
import Witch (from)

import Jira qualified

jiraClientTests :: TestTree
jiraClientTests = testGroup "Lentille.Jira" [testGetIssues, testGetIssue]

withMockClient :: (Jira.JiraClient -> IO ()) -> IO ()
withMockClient cb = do
    searchResp <- BS.readFile "test/data/jirasearch.json"
    issueResp <- BS.readFile "test/data/jiraissue.json"
    let app req respond = respond . Wai.responseLBS status200 mempty . from $ case Wai.rawPathInfo req of
            "/rest/api/2/search" -> searchResp
            "/rest/api/2/issue/14825490" -> issueResp
            other -> error $ "Invalid path: " <> show other
    withMockedManager app (cb . Jira.newJiraClient "http://localhost" Nothing "test-token")

testGetIssues :: TestTree
testGetIssues = testCase "getIssues" go
  where
    go = withMockClient $ \client -> do
        Right searchResults <- Jira.searchIssuesInfo client (Jira.JiraSearchRequest 0 10 "")
        searchResults.total @?= 2
        let (err, infos) = partitionEithers searchResults.issues
        length err @?= 0
        length infos @?= 2

testGetIssue :: TestTree
testGetIssue = testCase "getIssue" go
  where
    go = withMockClient $ \client -> do
        Right issue <- Jira.getIssue client "14825490"
        issue.issueType @?= "Story"
        issue.project @?= "PROJECT"
        issue.name @?= "PROJECT-1058"
        issue.score @?= Just 5.0

main :: IO ()
main = defaultMain jiraClientTests
