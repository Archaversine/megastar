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

variable :: Parser MegaExpr
variable = char '$' *> (Variable <$> identifier)

character :: Parser MegaExpr 
character = char '\'' *> (FromChar <$> anySingle) <* char '\''

cellValue :: Parser MegaExpr
cellValue = string "^^" *> (BookmarkExpr <$> identifier)

normalize :: Parser MegaExpr 
normalize = char '|' *> (Normalize <$> number) <* char '|'

number :: Parser MegaExpr
number = variable 
     <|> numLiteral 
     <|> cellValue 
     <|> normalize 
     <|> character

numLiteral :: Parser MegaExpr
numLiteral = Const . read <$> some digitChar

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
