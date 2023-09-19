module Megastar ( parseMegaToken 
                , parseLoop
                , module Megastar.Movement 
                , module Megastar.Modification
                , module Megastar.Tape 
                , module Megastar.Types 
                , module Megastar.IO
                ) where 

import Megastar.Movement
import Megastar.Modification
import Megastar.Tape
import Megastar.Types
import Megastar.IO

import Text.Megaparsec
import Text.Megaparsec.Char

parseMegaToken :: Parser MegaToken 
parseMegaToken = parseMovement 
             <|> parseTape
             <|> parseModification
             <|> parseIOToken
             <|> parseLoop

-- Control Flow

codeBlock :: Parser [MegaToken]
codeBlock = singleToken <|> multiToken 
    where singleToken = (:[]) <$> parseMegaToken 
          multiToken  = char '(' *> (parseMegaToken `sepBy` space) <* char ')'

emptyLoop :: Parser MegaToken 
emptyLoop = EmptyLoop <$> (string "[]" *> codeBlock)

positiveLoop :: Parser MegaToken 
positiveLoop = do 
    iterations <- char '[' *> (read <$> some digitChar) <* char ']'
    PositiveLoop iterations <$> codeBlock

negativeLoop :: Parser MegaToken 
negativeLoop = do
    iterations <- string "[-" *> (read <$> some digitChar) <* char ']'
    NegativeLoop iterations <$> codeBlock

parseLoop :: Parser MegaToken 
parseLoop = emptyLoop <|> negativeLoop <|> positiveLoop

