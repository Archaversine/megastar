module Megastar ( parseMegaToken 
                , parseLoop
                , parseConditional
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
import Megastar.Variable

import Text.Megaparsec
import Text.Megaparsec.Char

parseMegaToken :: Parser MegaToken 
parseMegaToken = parseMovement 
             <|> parseTape
             <|> parseModification
             <|> parseIOToken
             <|> parseLoop
             <|> parseConditional
             <|> parseFunction

-- Control Flow (Loops)

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

-- Control Flow (Conditionals)

whileLoop :: Parser MegaToken
whileLoop = char '?' *> (WhileLoop <$> codeBlock)

whileNotLoop :: Parser MegaToken 
whileNotLoop = string "!?" *> (WhileNotLoop <$> codeBlock)

ifConditional :: Parser MegaToken 
ifConditional = string "??" *> (If <$> codeBlock)

unlessConditional :: Parser MegaToken 
unlessConditional = string "!??" *> (Unless <$> codeBlock)

parseConditional :: Parser MegaToken 
parseConditional = ifConditional 
               <|> unlessConditional 
               <|> whileLoop 
               <|> whileNotLoop

-- Control Flow (Conditionals)

funcDeclaration :: Parser MegaToken
funcDeclaration = char '&' *> (FuncDecl <$> identifier <*> codeBlock)

funcCall :: Parser MegaToken 
funcCall = char '*' *> (FuncCall <$> identifier)

parseFunction :: Parser MegaToken 
parseFunction = funcDeclaration <|> funcCall
