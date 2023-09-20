module Megastar.Variable ( identifier 
                         , number
                         , parseVar
                         ) where 

import Megastar.Types

import Text.Megaparsec
import Text.Megaparsec.Char

identifier :: Parser String
identifier = do 
    x  <- letterChar
    xs <- many (letterChar <|> numberChar)

    return (x:xs)

number :: Parser MegaExpr
number = variable <|> numLiteral <|> cellValue

variable :: Parser MegaExpr
variable = char '$' *> (MegaVar <$> identifier)

cellValue :: Parser MegaExpr
cellValue = string "^^" *> (MegaBookmark <$> identifier)

numLiteral :: Parser MegaExpr
numLiteral = MegaConst . read <$> some digitChar

varAssign :: Parser MegaToken
varAssign = do 
    name  <- identifier <* (space >> string ":=" >> space)
    VarAssign name <$> number

varIncrease :: Parser MegaToken 
varIncrease = do 
    name <- identifier <* (space >> string "+=" >> space)
    VarIncrease name <$> number

varDecrease :: Parser MegaToken 
varDecrease = do 
    name <- identifier <* (space >> string "-=" >> space)
    VarDecrease name <$> number

parseVar :: Parser MegaToken 
parseVar = try varAssign <|> try varIncrease <|> varDecrease
