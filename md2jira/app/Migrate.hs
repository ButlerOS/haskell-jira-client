-- | A script to migrate backlog file from megaparsec to pandoc
module Migrate where

import Data.List
import GHC.Read

main :: IO ()
main = interact (unlines . go [] . lines)

go :: [String] -> [String] -> [String]
go acc [] = reverse acc
go acc (x : xs)
    | -- An epic line: move the jid to a {#} attribute
      "# " `isPrefixOf` x
    , Just (jid, line) <- parseJID (drop 2 x) =
        go (("# " <> line <> " {#" <> jid <> "}") : acc) xs
    | -- A story line: move the jid and add the score from the next line
      "## " `isPrefixOf` x
    , Just (jid, line) <- parseJID (drop 3 x) =
        let (score, rest) = case xs of
                (next : xs') | "> Score: " `isPrefixOf` next -> (" score=" <> drop 9 next, xs')
                _ -> ("", xs)
         in go (("## " <> line <> " {#" <> jid <> score <> "}") : acc) rest
    | -- A task line: move the '.' and assigned to a {.} attribute
      "- [" `isPrefixOf` x =
        let line = drop 3 x
            status = takeWhile (/= ']') line
            task = dropWhile (== ' ') $ drop (1 + length status) line
            (newStatus, tag) = case status of
                "." -> (" ", " {.n}")
                s | s `elem` [" ", "x"] -> (s, "")
                name -> (" ", " {." <> name <> "}")
         in go (("- [" <> newStatus <> "] " <> task <> tag) : acc) xs
    | otherwise = go (x : acc) xs

parseJID :: String -> Maybe (String, String)
parseJID s = case lex s of
    [(proj, '-' : num)] | [(n, ' ' : rest)] <- lexDigits num -> Just (proj <> "-" <> n, dropWhile (== ' ') rest)
    _ -> Nothing
