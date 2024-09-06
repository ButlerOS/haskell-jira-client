module Jira (
    -- * The client
    newJiraClient,
    JiraClient,

    -- * Issue API
    getIssue,
    setIssueScore,
    JiraID,
    mkJiraID,
    JiraIssue (..),
    jiraUrl,

    -- * Search API
    searchIssues,
    searchIssuesInfo,
    JiraSearchRequest (..),
    JQL (..),
    JiraIssueInfo (..),
    JiraSearchResult (..),

    -- * Create API
    IssueType (..),
    IssueData (..),
    createIssue,
    updateIssue,

    -- * Update API
    Transition (..),
    doTransition,
) where

import Control.Lens (toListOf, (^?))
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Lens
import Data.Aeson.Types (Pair)
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
import Numeric.Natural (Natural)
import Witch (From, from, into)

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
    baseUrl = T.dropWhileEnd (== '/') url

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
    initRequest <- HTTP.parseUrlThrow (from $ client.baseUrl <> "/rest/api/2/" <> path)
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

newtype JiraID = JiraID Text
    deriving newtype (Show, Eq, Ord, FromJSON, FromJSONKey, ToJSON, ToJSONKey, IsString)
    deriving (Generic)
instance From JiraID Text where from (JiraID n) = n

mkJiraID :: Text -> Natural -> JiraID
mkJiraID name nr = JiraID $ name <> "-" <> from (show nr)

-- | Get the url of a 'JiraID'
jiraUrl :: JiraClient -> JiraID -> Text
jiraUrl client (JiraID jid) = client.baseUrl <> "/browse/" <> jid

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
    name <- (v ^? key "key" . _JSON) `pDie` "Can't find kid"
    fields <- (v ^? key "fields") `pDie` "Can't find fields"
    project <- (fields ^? key "project" . key "key" . _String) `pDie` "Can't find project.key"
    issueType <- (fields ^? key "issuetype" . key "name" . _String) `pDie` "Can't find issuetype.name"
    updated <- (parseJiraTime =<< fields ^? key "updated" . _String) `pDie` "Can't find updated"
    let description = fields ^? key "description" . _String
    summary <- (fields ^? key "summary" . _String) `pDie` "Can't find summary"
    let score = do
            scoreMaybeNan <- fields ^? key client.issueScoreKey . _JSON
            if isNaN scoreMaybeNan
                then Nothing
                else pure scoreMaybeNan
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
    pure $ case res of
        Left e -> Just e
        Right _ -> Nothing
  where
    body = object ["fields" .= object [client.issueScoreKey .= score]]

newtype JQL = JQL Text deriving newtype (IsString, Show)

data JiraIssueInfo = JiraIssueInfo
    { name :: JiraID
    , updated :: UTCTime
    }
    deriving (Show, Generic, ToJSON)

decodeIssueInfo :: Value -> Either Text JiraIssueInfo
decodeIssueInfo v = do
    name <- JiraID <$> (v ^? key "key" . _String) `pDie` "Can't find key"
    updatedString <- (v ^? key "fields" . key "updated" . _String) `pDie` "Can't find fields.updated"
    updated <- parseJiraTime updatedString `pDie` ("Can't parse date: " <> updatedString)
    pure (JiraIssueInfo{..})
  where
    pDie :: Maybe a -> Text -> Either Text a
    pDie a n = a `orDie` (n <> ": " <> decodeUtf8 (from $ encode v))

data JiraSearchResult a = JiraSearchResult
    { total :: Word
    , issues :: [Either Text a]
    }
    deriving (Show, Generic)

data JiraSearchRequest = JiraSearchRequest
    { start :: Word
    , maxResults :: Word
    , query :: JQL
    }

searchIssuesImpl :: (Value -> Either Text a) -> [Value] -> JiraClient -> JiraSearchRequest -> IO (Either Text (JiraSearchResult a))
searchIssuesImpl decodeElem fields client (JiraSearchRequest start maxResults (JQL query)) = do
    let body =
            object
                [ ("maxResults", Number (fromIntegral maxResults))
                , ("startAt", Number (fromIntegral start))
                , ("fields", Array (fromList $ String "updated" : fields))
                , ("jql", String $ query <> " order by updated")
                ]

    searchResult <- jiraRequest client "search" "POST" (HTTP.RequestBodyLBS (encode body))
    pure $ case searchResult of
        Left e -> Left $ "Invalid response: " <> e
        Right x -> case (x ^? key "total" . _JSON, toListOf (key "issues" . values) x) of
            (_, []) -> Left $ "Couldn't find issues: " <> from (show x)
            (Nothing, _) -> Left $ "Couldn't find total: " <> from (show x)
            (Just total, issues) -> Right (JiraSearchResult total $ decodeElem <$> issues)

searchIssuesInfo :: JiraClient -> JiraSearchRequest -> IO (Either Text (JiraSearchResult JiraIssueInfo))
searchIssuesInfo = searchIssuesImpl decodeIssueInfo []

searchIssues :: JiraClient -> JiraSearchRequest -> IO (Either Text (JiraSearchResult JiraIssue))
searchIssues client = searchIssuesImpl (decodeIssue client) [String "project", String "issuetype", String "description", String "summary", String $ Key.toText client.issueScoreKey] client

newtype Transition = Transition Word
    deriving (Generic)
    deriving newtype (Eq, Show, ToJSON, FromJSON)

data IssueType = Epic | EpicStory JiraID | Story | SubTask JiraID
    deriving (Show)

issueTypeName :: IssueType -> Text
issueTypeName = \case
    Epic -> "Epic"
    EpicStory{} -> "Story"
    Story -> "Story"
    SubTask{} -> "Sub-Task"

data IssueData = IssueData
    { summary :: Text
    , description :: Text
    }
    deriving (Eq, Show, Generic)
instance ToJSON IssueData
instance FromJSON IssueData

decodeJiraIDResp :: Either Text Value -> Either Text JiraID
decodeJiraIDResp = \case
    Left err -> Left err
    Right v -> JiraID <$> (v ^? key "key" . _String) `orDie` "Can't find key"

mkBody :: Key -> [Pair] -> HTTP.RequestBody
mkBody name attrs =
    HTTP.RequestBodyLBS $ encode $ object [name .= object attrs]

createIssue :: JiraClient -> Text -> IssueType -> IssueData -> IO (Either Text JiraID)
createIssue client project issueType issueData = decodeJiraIDResp <$> jiraRequest client "issue/" "POST" body
  where
    body = mkBody "fields" attrs
    attrs =
        [ "project" .= object ["key" .= project]
        , "summary" .= T.strip issueData.summary
        , "description" .= T.strip issueData.description
        , "issuetype" .= object ["name" .= issueTypeName issueType]
        ]
            <> case issueType of
                SubTask jid -> ["parent" .= object ["key" .= into @Text jid]]
                -- TODO: support custom config
                EpicStory jid -> ["customfield_12311140" .= into @Text jid]
                Epic -> ["customfield_12311141" .= issueData.summary]
                _ -> []

ensureNull :: Either Text Value -> Maybe Text
ensureNull = \case
    Left err -> Just err
    Right Null -> Nothing
    Right v -> Just $ "Expected null, got: " <> decodeUtf8 (from $ encode v)

updateIssue :: JiraClient -> JiraID -> IssueData -> IO (Maybe Text)
updateIssue client jid issueData = ensureNull <$> jiraRequest client ("issue/" <> into @Text jid) "PUT" body
  where
    body = mkBody "fields" attrs
    attrs =
        [ "summary" .= T.strip issueData.summary
        , "description" .= T.strip issueData.description
        ]

-- | From https://www.haskellforall.com/2021/05/the-trick-to-avoid-deeply-nested-error.html
orDie :: Maybe a -> b -> Either b a
Just a `orDie` _ = Right a
Nothing `orDie` err = Left err

doTransition :: JiraClient -> JiraID -> Transition -> IO (Maybe Text)
doTransition client jid transition = ensureNull <$> jiraRequest client path "POST" body
  where
    path = "issue/" <> into @Text jid <> "/transitions"
    body = mkBody "transition" ["id" .= transition]
