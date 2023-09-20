module Megastar.Modification (parseModification) where 

import Megastar.Types
import Megastar.Variable (number)

import Text.Megaparsec
import Text.Megaparsec.Char

increment :: Parser MegaToken
increment = char '+' *> (Increment <$> number)

decrement :: Parser MegaToken 
decrement = char '-' *> (Decrement <$> number)

setValue :: Parser MegaToken
setValue = char '~' *> (SetValue <$> number)

parseModification :: Parser MegaToken
parseModification = increment <|> decrement <|> setValue
