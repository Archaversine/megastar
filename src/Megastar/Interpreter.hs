module Megastar.Interpreter ( module Megastar.Interpreter.Tape
                            , module Megastar.Interpreter.Core
                            , module Megastar.Interpreter.Movement 
                            , module Megastar.Interpreter.Modification
                            , module Megastar.Interpreter.IO 
                            , module Megastar.Interpreter.Variable
                            , module Megastar.Interpreter.Expr
                            , evalProg
                            ) where

import Control.Monad
import Control.Monad.State

import Data.Word (Word8)

import Megastar.Interpreter.Tape
import Megastar.Interpreter.Core
import Megastar.Interpreter.Movement 
import Megastar.Interpreter.Modification
import Megastar.Interpreter.IO 
import Megastar.Interpreter.Variable
import Megastar.Interpreter.Expr

import Megastar.Types

import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed.Mutable as UM

evalProg :: [MegaToken] -> Interpreter ()
evalProg = mapM_ evalToken

evalToken :: MegaToken -> Interpreter ()
evalToken MoveLeft               = moveLeft
evalToken MoveRight              = moveRight
evalToken MoveStart              = moveStart
evalToken MoveEnd                = moveEnd 
evalToken (TapeAlloc size value) = bulkAllocTape size value
evalToken (TapeString xs)        = allocTapeString xs
evalToken (TapeFile path)        = allocTapeFile path 
evalToken (TapeManual xs)        = allocTapeString xs
evalToken (Increment expr)       = evalExpr expr >>= increaseCell
evalToken (Decrement expr)       = evalExpr expr >>= decreaseCell
evalToken (SetValue expr)        = evalExpr expr >>= setCell
evalToken PrintCell              = printCell 
evalToken PrintCellValue         = printCellValue 
evalToken ReadChar               = readCell
evalToken ReadCellValue          = readCellValue 
evalToken (OutString str)        = outString str
evalToken (TokenSequence xs)     = mapM_ evalToken xs
evalToken (EmptyLoop xs)         = gets len >>= flip evalLoop xs . fromIntegral
evalToken (PositiveLoop expr xs) = evalExpr expr >>= flip evalLoop xs
evalToken (NegativeLoop expr xs) = do 
    val <- evalExpr expr
    n   <- gets $ fromIntegral . len

    evalLoop (n - val) xs
evalToken (EmptyRollingLoop xs)         = gets len >>= flip evalRollingLoop xs . fromIntegral 
evalToken (PositiveRollingLoop expr xs) = evalExpr expr >>= flip evalRollingLoop xs
evalToken (NegativeRollingLoop expr xs) = do 
    val <- evalExpr expr 
    n   <- gets $ fromIntegral . len 

    evalRollingLoop (n - val) xs
evalToken (WhileLoop xs) = do 
    t <- gets tape
    p <- gets pos

    value <- UM.read t p

    when (value /= 0) $ mapM_ evalToken xs >> evalToken (WhileLoop xs)
evalToken (WhileNotLoop xs) = do 
    t <- gets tape
    p <- gets pos

    value <- UM.read t p

    when (value == 0) $ mapM_ evalToken xs >> evalToken (WhileNotLoop xs)
evalToken (If xs) = do 
    t <- gets tape
    p <- gets pos 

    value <- UM.read t p

    when (value /= 0) $ mapM_ evalToken xs
evalToken (Unless xs) = do 
    t <- gets tape
    p <- gets pos 

    value <- UM.read t p

    when (value == 0) $ mapM_ evalToken xs
evalToken (Bookmark bookname) = do 
    p <- gets pos
    b <- gets books

    let books' = Map.insert bookname p b

    modify $ \s -> s { books = books' }
evalToken (JumpTo bookname) = do 
    b <- gets books

    case Map.lookup bookname b of 
        Nothing -> error $ "Imaginary Book: " <> bookname
        Just p  -> modify $ \s -> s { pos = p }
evalToken (FuncDecl funcName funcArgs funcBody) = do 
    f <- gets fnmap

    let f' = Map.insert funcName (ProgFunction funcArgs funcBody) f

    modify $ \s -> s { fnmap = f' }
evalToken (FuncCall _ _) = error "Unimplemented Feature"
evalToken (VarAssign varname expr) = evalExpr expr >>= varAssign varname
evalToken (VarIncrease varname expr) = evalExpr expr >>= varModify varname . (+)
evalToken (VarDecrease varname expr) = evalExpr expr >>= varModify varname . subtract
evalToken Halt = errorWithoutStackTrace "Halted"

evalLoop :: Word8 -> [MegaToken] -> Interpreter ()
evalLoop n xs = replicateM_ (fromIntegral n) (mapM_ evalToken xs)

evalRollingLoop :: Word8 -> [MegaToken] -> Interpreter ()
evalRollingLoop n xs = do 
    replicateM_ (fromIntegral n - 1) (mapM_ evalToken xs >> moveRight)
    mapM_ evalToken xs
