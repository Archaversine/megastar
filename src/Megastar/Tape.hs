module Megastar.Tape (parseTape) where 

import Control.Monad (void)

import Data.Char (ord)

import Megastar.Types

import Text.Megaparsec
import Text.Megaparsec.Char

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
