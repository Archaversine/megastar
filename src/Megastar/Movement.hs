module Megastar.Movement (parseMovement) where 

import Megastar.Types
import Megastar.Variable (identifier)

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

circularLeft :: Parser MegaToken 
circularLeft = string "<*" >> return CircularLeft

circularRight :: Parser MegaToken 
circularRight = string "*>" >> return CircularRight

bookmark :: Parser MegaToken 
bookmark = char '@' *> (Bookmark <$> identifier)

jumpTo :: Parser MegaToken 
jumpTo = char '^' *> (JumpTo <$> identifier)

parseMovement :: Parser MegaToken 
parseMovement = moveLeft 
            <|> moveRight 
            <|> moveStart 
            <|> moveEnd 
            <|> circularLeft 
            <|> circularRight
            <|> bookmark
            <|> jumpTo
