module Main (main) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data MegaToken = MoveLeft 
               | MoveRight
               deriving Show

moveLeft :: Parser MegaToken
moveLeft = string "<-" >> return MoveLeft

moveRight :: Parser MegaToken 
moveRight = string "->" >> return MoveRight

parseMegaToken :: Parser MegaToken 
parseMegaToken = try moveLeft 
             <|> moveRight

parseProg :: String -> String -> Either (ParseErrorBundle String Void) [MegaToken]
parseProg source xs = parse prog source (trim xs)
    where prog = parseMegaToken `sepBy` space
          trim   = dropWhile isSpace . dropWhileEnd isSpace

main :: IO ()
main = do 
    let test1 = " <- -> <- -> "

    case parseProg "<test>" test1 of
        Left err -> putStrLn $ errorBundlePretty err
        Right xs -> print xs
