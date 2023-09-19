module Main (main) where

import Control.Monad (void)

import Data.Char (isSpace, ord)
import Data.List (dropWhileEnd)
import Data.Word (Word8)

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data MegaToken = MoveLeft 
               | MoveRight
               | TapeAlloc Word8 Word8 
               | TapeString [Word8] 
               | TapeFile FilePath
               | TapeManual [Word8]
               deriving Show

moveLeft :: Parser MegaToken
moveLeft = string "<-" >> return MoveLeft

moveRight :: Parser MegaToken 
moveRight = string "->" >> return MoveRight

parseMegaToken :: Parser MegaToken 
parseMegaToken = moveLeft 
             <|> moveRight
             <|> parseTape

tapeAlloc :: Parser MegaToken 
tapeAlloc = do 
    void $ string "##" <* space

    tapeLength <- some digitChar <* space
    tapeValue  <- some digitChar <* eol

    return (TapeAlloc (read tapeLength) (read tapeValue))

tapeString :: Parser MegaToken 
tapeString = do 
    void $ string "#string" <* some spaceChar <* char '"'

    tapeValue <- someTill (noneOf "\"") (char '"')

    return $ TapeString $ map (fromIntegral . ord) tapeValue

tapeFile :: Parser MegaToken 
tapeFile = do 
    void $ string "#file" <* some spaceChar <* char '<'

    tapeValue <- someTill asciiChar (char '>')

    return (TapeFile tapeValue)

parseTape :: Parser MegaToken 
parseTape = tapeAlloc <|> tapeString <|> tapeFile

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
