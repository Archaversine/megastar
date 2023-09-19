module Main (main) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

import Data.Void

import Megastar

import Text.Megaparsec
import Text.Megaparsec.Char

parseProg :: String -> String -> Either (ParseErrorBundle String Void) [MegaToken]
parseProg source xs = parse prog source (trim xs)
    where prog = parseMegaToken `sepBy` space
          trim   = dropWhile isSpace . dropWhileEnd isSpace

parseFile :: FilePath -> IO (Either (ParseErrorBundle String Void) [MegaToken])
parseFile filename = do 
    contents <- readFile filename
    return $ parseProg filename contents

main :: IO ()
main = do 
    parsed <- parseFile "syntax-test.ms"

    case parsed of
        Left err -> putStrLn $ errorBundlePretty err
        Right xs -> mapM_ print xs
