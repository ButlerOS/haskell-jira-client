# jira-client

```haskell
-- * start REPL with
-- cabal repl

-- * Create the client:
client <- newJiraClientFromEnv "https://jira.example.com"

-- * Query issue:
issue <- getIssue client "JIRA-4242"
print issue

-- * Update issue:
setIssueScore client issue.name 23
```
