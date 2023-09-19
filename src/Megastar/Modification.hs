module Megastar.Modification (parseModification) where 

import Megastar.Types

import Text.Megaparsec
import Text.Megaparsec.Char

increment :: Parser MegaToken
increment = do 
    num <- char '+' *> some digitChar
    return $ Increment (read num)

decrement :: Parser MegaToken 
decrement = do 
    num <- char '-' *> some digitChar
    return $ Decrement (read num)

setValue :: Parser MegaToken
setValue = do 
    num <- char '~' *> some digitChar 
    return $ SetValue (read num)

parseModification :: Parser MegaToken
parseModification = increment <|> decrement <|> setValue
