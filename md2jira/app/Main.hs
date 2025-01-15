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
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, doesPathExist, getXdgDirectory)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)

main :: IO ()
main =
    getArgs >>= \case
        ["--dry"] -> T.putStrLn . printer . either (error . show) id . parse =<< T.getContents
        ["--help"] -> die "usage: md2jira FILE"
        [fp] ->
            fmap parse (T.readFile fp) >>= \case
                Right doc -> T.writeFile fp =<< go doc
                Left err -> die $ T.pack fp <> ": parse error: " <> err
        [] ->
            fmap parse T.getContents >>= \case
                Right doc -> void $ go doc
                Left err -> die $ "Could not parse input: " <> err
        _ -> die "usage: md2jira < FILE"
  where
    go doc = do
        project <- T.pack <$> getEnv "JIRA_PROJECT"
        cachePath <- getCachePath
        client <- mkClient
        cache <- loadCache cachePath
        let logger = T.putStrLn
        (newDoc, newCache, errors) <- eval logger client project doc cache
        let updated = cache /= newCache
        when updated $ encodeFile cachePath newCache
        case errors of
            []
                | updated -> pure $ printer newDoc
                | otherwise -> die "Already synced"
            xs -> die $ T.unlines xs

    mkClient = do
        url <- T.pack <$> getEnv "JIRA_URL"
        token <- BS.pack <$> getEnv "JIRA_TOKEN"
        Jira.newJiraClient url Nothing token <$> newTlsManager

    loadCache path =
        doesPathExist path >>= \case
            False -> pure mempty
            True -> either error id <$> eitherDecodeFileStrict path

    getCachePath = do
        xdgDir <- getXdgDirectory XdgCache ""
        createDirectoryIfMissing True xdgDir
        pure $ xdgDir <> "/md2jira.json"

    die msg = T.putStrLn msg >> exitFailure
