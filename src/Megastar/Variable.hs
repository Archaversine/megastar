module Megastar.Variable ( identifier 
                         , number
                         , parseVarAssign
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

parseVarAssign :: Parser MegaToken
parseVarAssign = do 
    name  <- identifier <* (space >> string ":=" >> space)
    VarAssign name <$> number
