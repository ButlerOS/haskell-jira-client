module Main (main) where

import Data.List (isSuffixOf)
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Builder qualified as T
import Data.Text.Lazy.Encoding qualified as LText
import MD2Jira qualified
import System.Directory (listDirectory)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.Pretty.Simple (pShowNoColor)

main :: IO ()
main = do
    goldenFiles <- traverse parseMD =<< listGoldenFiles
    defaultMain $
        testGroup
            "Tests"
            [ testGroup "Golden test" $ map doGoldenTest goldenFiles
            ]

listGoldenFiles :: IO [FilePath]
listGoldenFiles = map (mappend dpath) . filter (not . isSuffixOf ".golden") <$> listDirectory dpath
  where
    dpath = "test/golden/"

parseMD :: FilePath -> IO (FilePath, MD2Jira.Document)
parseMD fp = do
    inp <- T.readFile fp
    case MD2Jira.parse inp of
        Left e -> error (show e)
        Right x -> pure (fp, x)

doGoldenTest :: (FilePath, MD2Jira.Document) -> TestTree
doGoldenTest (fp, doc) =
    testGroup
        fp
        [ go "ast" (pShowNoColor doc)
        , go "round" (LText.fromStrict $ MD2Jira.printer doc)
        , go "jira" (T.toLazyText $ foldMap jiraDoc doc.epics)
        ]
  where
    jiraDoc :: MD2Jira.Epic -> T.Builder
    jiraDoc epic =
        mconcat
            [ "EpicTitle: "
            , T.fromText epic.title
            , "\nEpicBody:\n"
            , T.fromText $ MD2Jira.toJira epic.description
            , foldMap jiraStoryDoc epic.stories
            , "\n"
            ]
    jiraStoryDoc story =
        mconcat
            [ "\n\nStoryTitle: "
            , T.fromText story.title
            , "\nStoryBody:\n"
            , T.fromText $ MD2Jira.toJira story.description
            , "\n"
            ]
    go name out = goldenVsString name (fp <> "-" <> name <> ".golden") (pure $ LText.encodeUtf8 out)
