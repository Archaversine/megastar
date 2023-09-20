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

negateExpr :: Parser MegaExpr
negateExpr = char '/' *> (Negate <$> number)

cellValue :: Parser MegaExpr
cellValue = string "^^" *> (BookmarkExpr <$> identifier)

normalize :: Parser MegaExpr 
normalize = char '|' *> (Normalize <$> number) <* char '|'

addExpr :: Parser MegaExpr 
addExpr = string "{+|" *> (Add <$> number `sepBy1` separator) <* char '}'
    where separator = space >> char ',' >> space

subExpr :: Parser MegaExpr 
subExpr = string "{-|" *> (Sub <$> number `sepBy1` separator) <* char '}'
    where separator = space >> char ',' >> space

foldOr :: Parser MegaExpr
foldOr = string "{o|" *> (FoldOr <$> number `sepBy1` separator) <* char '}'
    where separator = space >> char ',' >> space

foldAnd :: Parser MegaExpr 
foldAnd = string "{&|" *> (FoldAnd <$> number `sepBy1` separator) <* char '}'
    where separator = space >> char ',' >> space

foldXor :: Parser MegaExpr 
foldXor = string "{x|" *> (FoldXor <$> number `sepBy1` separator) <* char '}'
    where separator = space >> char ',' >> space

number :: Parser MegaExpr
number = variable 
     <|> numLiteral 
     <|> cellValue 
     <|> normalize 
     <|> character
     <|> negateExpr
     <|> addExpr
     <|> subExpr
     <|> foldOr 
     <|> foldAnd 
     <|> foldXor

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
