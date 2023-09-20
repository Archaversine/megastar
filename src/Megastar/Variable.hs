module Megastar.Variable ( identifier 
                         , number
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
number = variable <|> numLiteral

variable :: Parser MegaExpr
variable = char '$' *> (MegaVar <$> identifier)

numLiteral :: Parser MegaExpr
numLiteral = MegaConst . read <$> some digitChar
