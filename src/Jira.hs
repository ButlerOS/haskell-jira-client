module Jira (
    -- * The client
    newJiraClient,
    JiraClient,

    -- * Issue API
    getIssue,
    setIssueScore,
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
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import GHC.Exts (fromList)
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP
import Witch (From, from)

-- Check api doc at:
-- https://developer.atlassian.com/server/jira/platform/jira-rest-api-examples/
data JiraClient = JiraClient
    { manager :: HTTP.Manager
    , baseUrl :: Text
    , token :: ByteString
    , issueScoreKey :: Key
    }

newJiraClient :: Text -> Maybe Key -> ByteString -> HTTP.Manager -> JiraClient
newJiraClient url mIssueScoreKey token manager = JiraClient{..}
  where
    issueScoreKey = fromMaybe "customfield_12310243" mIssueScoreKey
    baseUrl = T.dropWhileEnd (== '/') url <> "/rest/api/2/"

httpJSONRequest :: HTTP.Manager -> HTTP.Request -> IO (Either Text Value)
httpJSONRequest manager request = do
    responseBody <- HTTP.responseBody <$> HTTP.httpLbs request manager
    pure $ case responseBody of
        "" -> Right Null
        _ -> case eitherDecode responseBody of
            Left e -> Left (T.pack e)
            Right v -> Right v

newtype HttpVerb = HttpVerb ByteString deriving newtype (IsString)

jiraRequest :: JiraClient -> Text -> HttpVerb -> HTTP.RequestBody -> IO (Either Text Value)
jiraRequest client path (HttpVerb verb) body = do
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

issueRequest :: JiraClient -> JiraID -> HttpVerb -> HTTP.RequestBody -> IO (Either Text Value)
issueRequest client (JiraID jid) = jiraRequest client ("issue/" <> jid)

newtype JiraID = JiraID Text deriving newtype (Show, Eq, Ord, ToJSON, IsString)
instance From JiraID Text where from (JiraID n) = n

{- | Drop the extra milli second and timezone

 >>> parseJiraTime "2022-09-13T14:37:36.000+0000"
 Just 2022-09-13 14:37:36 UTC
-}
parseJiraTime :: Text -> Maybe UTCTime
parseJiraTime t = parseTimeM False defaultTimeLocale "%FT%T" (from $ T.takeWhile (/= '.') t)

data JiraIssue = JiraIssue
    { project :: Text
    , name :: JiraID
    , issueType :: Text
    , updated :: UTCTime
    , description :: Maybe Text
    , summary :: Text
    , score :: Maybe Float
    }
    deriving (Show, Generic, ToJSON)

decodeIssue :: JiraClient -> Value -> Either Text JiraIssue
decodeIssue client v = do
    name <- JiraID <$> (v ^? key "key" . _String) `pDie` "Can't find kid"
    fields <- (v ^? key "fields") `pDie` "Can't find fields"
    project <- (fields ^? key "project" . key "key" . _String) `pDie` "Can't find project.key"
    issueType <- (fields ^? key "issuetype" . key "name" . _String) `pDie` "Can't find issuetype.name"
    updated <- (parseJiraTime =<< fields ^? key "updated" . _String) `pDie` "Can't find updated"
    let description = fields ^? key "description" . _String
    summary <- (fields ^? key "summary" . _String) `pDie` "Can't find summary"
    let score = fields ^? key client.issueScoreKey . _JSON
    pure JiraIssue{..}
  where
    pDie :: Maybe a -> Text -> Either Text a
    pDie a n = a `orDie` (n <> ": " <> decodeUtf8 (from $ encode v))

getIssue :: JiraClient -> JiraID -> IO (Either Text JiraIssue)
getIssue client jid = do
    res <- issueRequest client jid "GET" mempty
    pure $ case res of
        Left e -> Left (from jid <> ": " <> from e)
        Right x -> decodeIssue client x

setIssueScore :: JiraClient -> JiraID -> Float -> IO (Maybe Text)
setIssueScore client jid score = do
    res <- issueRequest client jid "PUT" (HTTP.RequestBodyLBS (encode body))
    case res of
        Left e -> pure $ Just e
        Right x -> do
            putStrLn $ "Got: " <> show x
            pure Nothing
  where
    body = object ["fields" .= object [client.issueScoreKey .= score]]

newtype JQL = JQL Text deriving newtype (IsString, Show)

data JiraIssueInfo = JiraIssueInfo
    { jid :: JiraID
    , name :: Text
    , updated :: UTCTime
    }
    deriving (Show, Generic, ToJSON)

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
    deriving (Show, Generic)

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
