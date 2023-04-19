# jira-client

```haskell
-- * start REPL with
-- cabal repl --build-depends http-client-tls

-- * Create the client:
client <- newJiraClient "https://jira.example.com" Nothing <$> (maybe (error "needToken") (encodeUtf8 . from) <$> System.Environment.lookupEnv "JIRA_TOKEN") <*> Network.HTTP.Client.TLS.newTlsManager

-- * Query issue:
issue <- getIssue client "JIRA-4242"
print issue

-- * Update issue:
setIssueScore client issue.name 23
```
