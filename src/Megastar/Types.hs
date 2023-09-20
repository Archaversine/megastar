module Megastar.Types (Parser, MegaToken(..), MegaExpr(..)) where

import Data.Word (Word8)
import Data.Void (Void)

import Text.Megaparsec

type Parser = Parsec Void String

data MegaExpr = MegaVar String
              | MegaConst Word8

instance Show MegaExpr where 
    show (MegaVar name) = '$' : name
    show (MegaConst x) = '#' : show x

data MegaToken = MoveLeft 
               | MoveRight
               | MoveStart
               | MoveEnd
               | TapeAlloc Word8 Word8 
               | TapeString [Word8] 
               | TapeFile FilePath
               | TapeManual [Word8]
               | Increment MegaExpr 
               | Decrement MegaExpr
               | SetValue MegaExpr
               | PrintCell
               | PrintCellValue
               | ReadChar
               | ReadCellValue
               | TokenSequence [MegaToken]
               | EmptyLoop [MegaToken]
               | PositiveLoop MegaExpr [MegaToken]
               | NegativeLoop MegaExpr [MegaToken]
               | WhileLoop [MegaToken]
               | WhileNotLoop [MegaToken]
               | If [MegaToken]
               | Unless [MegaToken]
               | Bookmark String
               | JumpTo String
               | FuncDecl String [String] [MegaToken]
               | FuncCall String [MegaExpr]
               deriving Show
