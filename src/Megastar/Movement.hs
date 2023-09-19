module Megastar.Movement where 

import Megastar.Types

import Text.Megaparsec.Char

moveLeft :: Parser MegaToken
moveLeft = string "<-" >> return MoveLeft

moveRight :: Parser MegaToken 
moveRight = string "->" >> return MoveRight
