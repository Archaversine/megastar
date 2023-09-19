module Megastar ( parseMegaToken 
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

import Text.Megaparsec

parseMegaToken :: Parser MegaToken 
parseMegaToken = parseMovement 
             <|> parseTape
             <|> parseModification
             <|> parseIOToken
