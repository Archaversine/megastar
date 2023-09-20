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
parseMegaToken = parseHalt
             <|> parseMovement 
             <|> parseTape
             <|> parseModification
             <|> parseIOToken
             <|> parseLoop
             <|> parseConditional
             <|> parseFunction
             <|> parseVar

parseHalt :: Parser MegaToken 
parseHalt = string "[H]" >> return Halt

-- Control Flow (Loops)

codeBlock :: Parser [MegaToken]
codeBlock = singleToken <|> multiToken 
    where singleToken = (:[]) <$> parseMegaToken 
          multiToken  = char '(' *> (parseMegaToken `sepBy` space) <* char ')'

emptyLoop :: Parser MegaToken 
emptyLoop = EmptyLoop <$> (string "[]" *> codeBlock)

positiveLoop :: Parser MegaToken 
positiveLoop = do 
    iterations <- char '[' *> number <* char ']'
    PositiveLoop iterations <$> codeBlock

negativeLoop :: Parser MegaToken 
negativeLoop = do
    iterations <- string "[-" *> number <* char ']'
    NegativeLoop iterations <$> codeBlock

emptyRollingLoop :: Parser MegaToken 
emptyRollingLoop = string "{}" *> (EmptyRollingLoop <$> codeBlock)

positiveRollingLoop :: Parser MegaToken 
positiveRollingLoop = do 
    iterations <- char '{' *> number <* char '}'
    PositiveRollingLoop iterations <$> codeBlock

negativeRollingLoop :: Parser MegaToken 
negativeRollingLoop = do 
    iterations <- string "{-" *> number <* char '}'
    NegativeRollingLoop iterations <$> codeBlock

parseLoop :: Parser MegaToken 
parseLoop = emptyLoop  
        <|> negativeLoop 
        <|> positiveLoop
        <|> emptyRollingLoop
        <|> negativeRollingLoop
        <|> positiveRollingLoop

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

funcArgDeclaration :: Parser [String]
funcArgDeclaration = char '<' *> (identifier `sepBy` separator) <* char '>'
    where separator = space >> char ',' >> space

funcArgExpr :: Parser [MegaExpr]
funcArgExpr = char '<' *> (number `sepBy` separator) <* char '>'
    where separator = space >> char ',' >> space

funcDeclaration :: Parser MegaToken
funcDeclaration = do 
    name   <- char '&' *> identifier
    params <- optional funcArgDeclaration
    block  <- codeBlock

    return $ case params of 
        Nothing -> FuncDecl name [] block
        Just ps -> FuncDecl name ps block

funcCall :: Parser MegaToken 
funcCall = do 
    name   <- char '*' *> identifier
    params <- optional funcArgExpr

    return $ case params of 
        Nothing -> FuncCall name []
        Just ps -> FuncCall name ps

parseFunction :: Parser MegaToken 
parseFunction = funcDeclaration <|> funcCall
