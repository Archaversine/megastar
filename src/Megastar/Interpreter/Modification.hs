module Megastar.Interpreter.Modification ( increaseCell 
                                         , decreaseCell 
                                         , setCell 
                                         ) where

import Control.Monad.State

import Data.Word (Word8)

import Megastar.Interpreter.Core

import qualified Data.Vector.Unboxed.Mutable as UM

increaseCell :: TapePtr -> Word8 -> Interpreter () 
increaseCell cell value = do 
    t <- gets tape
    liftIO $ UM.modify t (+ value) cell

decreaseCell :: TapePtr -> Word8 -> Interpreter () 
decreaseCell cell value = do 
    t <- gets tape
    liftIO $ UM.modify t (flip (-) value) cell

setCell :: TapePtr -> Word8 -> Interpreter () 
setCell cell value = do 
    t <- gets tape
    liftIO $ UM.write t cell value
