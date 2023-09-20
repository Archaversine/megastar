module Megastar.Interpreter.Core ( ProgFunction(..) 
                                 , ProgState(..)
                                 , TapePtr
                                 , Interpreter
                                 ) where

import Control.Monad.State

import Data.Map.Strict (Map)

import Data.Vector.Unboxed.Mutable (IOVector)
import Data.Word (Word8)

import Megastar.Types

type TapePtr = Int

data ProgFunction = ProgFunction { name :: String
                                 , args :: [String]
                                 , body :: [MegaToken]
                                 }

data ProgState = ProgState { tape  :: IOVector Word8 
                           , pos   :: TapePtr
                           , len   :: TapePtr 
                           , books :: Map String TapePtr
                           , vmap  :: [Map String Word8]
                           , fnmap :: [Map String ProgFunction]
                           }

type Interpreter = StateT ProgState IO

