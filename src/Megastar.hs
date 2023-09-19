module Megastar ( parseMegaToken 
                , module Megastar.Movement 
                , module Megastar.Modification
                , module Megastar.Tape 
                , module Megastar.Types 
                ) where 

import Megastar.Movement
import Megastar.Modification
import Megastar.Tape
import Megastar.Types

import Text.Megaparsec

parseMegaToken :: Parser MegaToken 
parseMegaToken = moveLeft 
             <|> moveRight
             <|> parseTape
             <|> parseModification
