module Megastar.Interpreter.Core ( ProgFunction(..) 
                                 , ProgState(..)
                                 , TapePtr
                                 , VarMap 
                                 , FuncMap
                                 , Interpreter
                                 , newProgState
                                 ) where

import Control.Monad.State

import Data.Map.Strict (Map)

import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as UM 

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

newProgState :: IO ProgState 
newProgState = do 
    newTape <- UM.replicate 1 0

    return $ ProgState { tape  = newTape
                       , pos   = 0
                       , len   = 0
                       , books = mempty
                       , vmap  = mempty
                       , fnmap = mempty
                       }

type Interpreter = StateT ProgState IO

