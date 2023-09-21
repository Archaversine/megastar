module Megastar.Interpreter.Core ( ProgFunction(..) 
                                 , ProgState(..)
                                 , TapePtr
                                 , VarMap 
                                 , FuncMap
                                 , Interpreter
                                 ) where

import Control.Monad.State

import Data.Map.Strict (Map)

import Data.Vector.Unboxed.Mutable (IOVector)
import Data.Word (Word8)

import Megastar.Types

type TapePtr = Int
type VarMap  = Map String Word8
type FuncMap = Map String ProgFunction

data ProgFunction = ProgFunction { args :: [String]
                                 , body :: [MegaToken]
                                 }

data ProgState = ProgState { tape  :: IOVector Word8 
                           , pos   :: TapePtr
                           , len   :: TapePtr 
                           , books :: Map String TapePtr
                           , vmap  :: [VarMap]
                           , fnmap :: FuncMap
                           }

type Interpreter = StateT ProgState IO

