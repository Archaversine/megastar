module Main (main) where

import Control.Monad.State

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

import Data.Void

import Megastar (MegaToken, parseMegaToken)
import Megastar.Interpreter (evalProg, newProgState)

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
    parsed <- parseFile "prog-test.ms"

    case parsed of
        Left err -> putStrLn $ errorBundlePretty err
        Right xs -> newProgState >>= evalStateT (evalProg xs)
