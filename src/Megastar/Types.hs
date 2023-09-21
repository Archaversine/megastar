module Megastar.Types (Parser, MegaToken(..), MegaExpr(..)) where

import Data.Word (Word8)
import Data.Void (Void)

import Text.Megaparsec

type Parser = Parsec Void String

data MegaExpr = Variable String
              | Const Word8
              | BookmarkExpr String
              | Normalize MegaExpr
              | FromChar Char
              | Negate MegaExpr
              | Add [MegaExpr]
              | Sub [MegaExpr]
              | FoldOr [MegaExpr] 
              | FoldAnd [MegaExpr] 
              | FoldXor [MegaExpr]

instance Show MegaExpr where 
    show (Variable name)     = '$' : name
    show (Const x)           = '#' : show x
    show (BookmarkExpr name) = "^^" <> name
    show (Normalize num)     = "|" <> show num <> "|"
    show (FromChar c)        = '\'' : c : "'"
    show (Negate x)          = '/' : show x
    show (Add xs)            = "sum(" <> show xs <> ")"
    show (Sub xs)            = "sub(" <> show xs <> ")" 
    show (FoldOr xs)         = "or(" <> show xs <> ")" 
    show (FoldAnd xs)        = "and(" <> show xs <> ")" 
    show (FoldXor xs)        = "xor(" <> show xs <> ")" 

data MegaToken = MoveLeft 
               | MoveRight
               | MoveStart
               | MoveEnd
               | TapeAlloc Int Word8 
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
               | OutString String
               | TokenSequence [MegaToken]
               | EmptyLoop [MegaToken]
               | PositiveLoop MegaExpr [MegaToken]
               | NegativeLoop MegaExpr [MegaToken]
               | EmptyRollingLoop [MegaToken]
               | PositiveRollingLoop MegaExpr [MegaToken]
               | NegativeRollingLoop MegaExpr [MegaToken]
               | WhileLoop [MegaToken]
               | WhileNotLoop [MegaToken]
               | If [MegaToken]
               | Unless [MegaToken]
               | Bookmark String
               | JumpTo String
               | FuncDecl String [String] [MegaToken]
               | FuncCall String [MegaExpr]
               | VarAssign String MegaExpr
               | VarIncrease String MegaExpr
               | VarDecrease String MegaExpr
               | Halt
               deriving Show
