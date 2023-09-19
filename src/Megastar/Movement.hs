module Megastar.Movement (parseMovement) where 

import Megastar.Types

import Text.Megaparsec
import Text.Megaparsec.Char

moveLeft :: Parser MegaToken
moveLeft = string "<-" >> return MoveLeft

moveRight :: Parser MegaToken 
moveRight = string "->" >> return MoveRight

moveStart :: Parser MegaToken 
moveStart = string "<<" >> return MoveStart

moveEnd :: Parser MegaToken 
moveEnd = string ">>" >> return MoveEnd 

parseMovement :: Parser MegaToken 
parseMovement = moveLeft <|> moveRight <|> moveStart <|> moveEnd
