module Megastar.Types (Parser, MegaToken(..)) where

import Data.Word (Word8)
import Data.Void (Void)

import Text.Megaparsec

type Parser = Parsec Void String

data MegaToken = MoveLeft 
               | MoveRight
               | MoveStart
               | MoveEnd
               | TapeAlloc Word8 Word8 
               | TapeString [Word8] 
               | TapeFile FilePath
               | TapeManual [Word8]
               | Increment Word8 
               | Decrement Word8
               | SetValue Word8
               | PrintCell
               | PrintCellValue
               | ReadChar
               | ReadCellValue
               | TokenSequence [MegaToken]
               | EmptyLoop [MegaToken]
               | PositiveLoop Word8 [MegaToken]
               | NegativeLoop Word8 [MegaToken]
               deriving Show
