module Jira (
    -- * The client
    newJiraClient,
    JiraClient,

    -- * Issue API
    getIssue,
    JiraID,
    JiraIssue (..),

    -- * Search API
    searchIssues,
    JQL (..),
    JiraIssueInfo (..),
    JiraSearchResult (..),
) where

import Control.Lens (toListOf, (^?))
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import GHC.Exts (fromList)
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP
import Witch (from)

data JiraClient = JiraClient
    { manager :: HTTP.Manager
    , baseUrl :: Text
    , token :: ByteString
    }

newJiraClient :: Text -> ByteString -> HTTP.Manager -> JiraClient
newJiraClient url token manager = JiraClient{..}
  where
    baseUrl = T.dropWhileEnd (== '/') url <> "/rest/api/2/"

httpJSONRequest :: HTTP.Manager -> HTTP.Request -> IO (Either Text Value)
httpJSONRequest manager request =
    either (Left . T.pack) Right . eitherDecode . HTTP.responseBody
        <$> HTTP.httpLbs request manager

jiraRequest :: JiraClient -> Text -> ByteString -> HTTP.RequestBody -> IO (Either Text Value)
jiraRequest client path verb body = do
    initRequest <- HTTP.parseUrlThrow (from $ client.baseUrl <> path)
    let request =
            initRequest
                { HTTP.requestHeaders =
                    [ ("Content-Type", "application/json")
                    , ("Authorization", "Bearer " <> client.token)
                    ]
                , HTTP.method = verb
                , HTTP.requestBody = body
                }
    httpJSONRequest client.manager request

newtype JiraID = JiraID Text deriving newtype (ToJSON, IsString)

{- | Drop the extra milli second and timezone

 >>> parseJiraTime "2022-09-13T14:37:36.000+0000"
 Just 2022-09-13 14:37:36 UTC
-}
parseJiraTime :: Text -> Maybe UTCTime
parseJiraTime t = parseTimeM False defaultTimeLocale "%FT%T" (from $ T.takeWhile (/= '.') t)

data JiraIssue = JiraIssue
    { project :: Text
    , name :: Text
    , issueType :: Text
    , updated :: UTCTime
    , description :: Maybe Text
    , summary :: Text
    }
    deriving (Generic, ToJSON)

decodeIssue :: Value -> Either Text JiraIssue
decodeIssue v = do
    name <- (v ^? key "key" . _String) `pDie` "Can't find kid"
    fields <- (v ^? key "fields") `pDie` "Can't find fields"
    project <- (fields ^? key "project" . key "key" . _String) `pDie` "Can't find project.key"
    issueType <- (fields ^? key "issuetype" . key "name" . _String) `pDie` "Can't find issuetype.name"
    updated <- (parseJiraTime =<< fields ^? key "updated" . _String) `pDie` "Can't find updated"
    let description = fields ^? key "description" . _String
    summary <- (fields ^? key "summary" . _String) `pDie` "Can't find summary"
    pure JiraIssue{..}
  where
    pDie :: Maybe a -> Text -> Either Text a
    pDie a n = a `orDie` (n <> ": " <> decodeUtf8 (from $ encode v))

getIssue :: JiraClient -> JiraID -> IO (Either Text JiraIssue)
getIssue client (JiraID jid) = do
    res <- jiraRequest client ("issue/" <> jid) "GET" mempty
    pure $ case res of
        Left e -> Left (jid <> ": " <> from e)
        Right x -> decodeIssue x

newtype JQL = JQL Text deriving newtype (IsString, Show)

data JiraIssueInfo = JiraIssueInfo
    { jid :: JiraID
    , name :: Text
    , updated :: UTCTime
    }
    deriving (Generic, ToJSON)

decodeIssueInfo :: Value -> Either Text JiraIssueInfo
decodeIssueInfo v = do
    jid <- (JiraID <$> v ^? key "id" . _String) `pDie` "Can't find id"
    name <- (v ^? key "key" . _String) `pDie` "Can't find key"
    updatedString <- (v ^? key "fields" . key "updated" . _String) `pDie` "Can't find fields.updated"
    updated <- parseJiraTime updatedString `pDie` ("Can't parse date: " <> updatedString)
    pure (JiraIssueInfo{..})
  where
    pDie :: Maybe a -> Text -> Either Text a
    pDie a n = a `orDie` (n <> ": " <> decodeUtf8 (from $ encode v))

data JiraSearchResult = JiraSearchResult
    { total :: Word
    , issues :: [Either Text JiraIssueInfo]
    }
    deriving (Generic)

searchIssues :: JiraClient -> Word -> JQL -> IO (Either Text JiraSearchResult)
searchIssues client start (JQL query) = do
    let body =
            object
                [ ("maxResults", Number 100)
                , ("startAt", Number (fromIntegral start))
                , ("fields", Array (fromList [String "updated"]))
                , ("jql", String $ query <> " order by updated")
                ]

    searchResult <- jiraRequest client "search" "POST" (HTTP.RequestBodyLBS (encode body))
    pure $ case searchResult of
        Left e -> Left $ "Invalid response: " <> e
        Right x -> case (x ^? key "total" . _JSON, toListOf (key "issues" . values) x) of
            (_, []) -> Left $ "Couldn't find issues: " <> from (show x)
            (Nothing, _) -> Left $ "Couldn't find total: " <> from (show x)
            (Just total, issues) -> Right (JiraSearchResult total $ decodeIssueInfo <$> issues)

-- | From https://www.haskellforall.com/2021/05/the-trick-to-avoid-deeply-nested-error.html
orDie :: Maybe a -> b -> Either b a
Just a `orDie` _ = Right a
Nothing `orDie` err = Left err
