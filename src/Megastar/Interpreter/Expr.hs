module Megastar.Interpreter.Expr (evalExpr) where

import Control.Monad.State

import Data.Bits
import Data.Bool (bool)
import Data.List (foldl')
import Data.Word (Word8)

import Megastar.Types
import Megastar.Interpreter.Core
import Megastar.Interpreter.Variable

import qualified Data.Vector.Unboxed.Mutable as UM

evalExpr :: MegaExpr -> Interpreter Word8
evalExpr (Variable varname)    = varLookup varname
evalExpr (Const x)             = return x
evalExpr (BookmarkExpr x)      = bookmarkLookup x
evalExpr (Normalize x)         = bool 0 1 . (>0) <$> evalExpr x
evalExpr (FromChar c)          = return (fromIntegral $ fromEnum c)
evalExpr (Negate x)            = bool 1 0 . (>0) <$> evalExpr x
evalExpr (Add xs)              = sum <$> mapM evalExpr xs
evalExpr (Sub xs)              = foldl' (-)   0 <$> mapM evalExpr xs
evalExpr (FoldOr xs)           = foldl' (.|.) 0 <$> mapM (evalExpr . Normalize) xs
evalExpr (FoldAnd xs)          = foldl' (.&.) 1 <$> mapM (evalExpr . Normalize) xs 
evalExpr (FoldXor xs)          = foldl' xor   0 <$> mapM (evalExpr . Normalize) xs
evalExpr (OffsetLookupLeft x)  = get >>= \ps -> UM.read (tape ps) (pos ps - fromIntegral x)
evalExpr (OffsetLookupRight x) = get >>= \ps -> UM.read (tape ps) (pos ps + fromIntegral x)
