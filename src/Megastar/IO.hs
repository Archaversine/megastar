module Megastar.IO (parseIOToken) where

import Megastar.Types

import Text.Megaparsec
import Text.Megaparsec.Char

printCell :: Parser MegaToken 
printCell = string "<=" >> return PrintCell

printCellValue :: Parser MegaToken 
printCellValue = string "<%" >> return PrintCellValue

printAndMove :: Parser MegaToken 
printAndMove = string "<>" >> return (TokenSequence [PrintCell, MoveRight])

printValueAndMove :: Parser MegaToken 
printValueAndMove = string "<%>" >> return (TokenSequence [PrintCellValue, MoveRight])

readChar :: Parser MegaToken 
readChar = string "=>" >> return ReadChar

readCharValue :: Parser MegaToken 
readCharValue = string "%>" >> return ReadCellValue

parseIOToken :: Parser MegaToken 
parseIOToken = printCell 
           <|> printValueAndMove
           <|> printCellValue 
           <|> printAndMove 
           <|> readChar 
           <|> readCharValue
