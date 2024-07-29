-- | A CLI interface for md2jira
module Main (main) where

import Control.Monad (void, when)
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.ByteString.Char8 qualified as BS
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Jira (newJiraClient)
import MD2Jira (eval, parse, printer)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Directory (doesPathExist)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)

main :: IO ()
main =
    getArgs >>= \case
        ["--dry"] -> T.putStrLn . printer . either error id . parse =<< T.getContents
        ["--help"] -> die "usage: md2jira FILE"
        [fp] ->
            fmap parse (T.readFile fp) >>= \case
                Right epics -> T.writeFile fp =<< go epics
                Left err -> die $ T.pack $ fp <> ": parse error: " <> err
        [] ->
            fmap parse T.getContents >>= \case
                Right epics -> void $ go epics
                Left err -> die $ "Could not parse input: " <> T.pack err
        _ -> die "usage: md2jira < FILE"
  where
    go epics = do
        project <- T.pack <$> getEnv "JIRA_PROJECT"
        client <- mkClient
        cache <- loadCache
        let logger _ = pure ()
        (newEpics, newCache, errors) <- eval logger client project epics cache
        T.putStrLn $ printer newEpics
        let updated = cache /= newCache
        when updated $ encodeFile ".cache.json" newCache
        case errors of
            []
                | updated -> pure $ printer newEpics
                | otherwise -> die "Already synced"
            xs -> die $ T.unlines xs

    mkClient = do
        url <- T.pack <$> getEnv "JIRA_URL"
        token <- BS.pack <$> getEnv "JIRA_TOKEN"
        Jira.newJiraClient url Nothing token <$> newTlsManager

    loadCache =
        doesPathExist ".cache.json" >>= \case
            False -> pure mempty
            True -> either error id <$> eitherDecodeFileStrict ".cache.json"

    die msg = T.putStrLn msg >> exitFailure
