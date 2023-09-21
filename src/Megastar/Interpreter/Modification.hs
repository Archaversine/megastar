module Megastar.Interpreter.Modification ( increaseCell 
                                         , decreaseCell 
                                         , setCell 
                                         ) where

import Control.Monad.State

import Data.Word (Word8)

import Megastar.Interpreter.Core

import qualified Data.Vector.Unboxed.Mutable as UM

increaseCell :: Word8 -> Interpreter () 
increaseCell value = do 
    pstate <- get
    liftIO $ UM.modify (tape pstate) (+ value) (pos pstate)

decreaseCell :: Word8 -> Interpreter () 
decreaseCell value = do 
    pstate <- get
    liftIO $ UM.modify (tape pstate) (flip (-) value) (pos pstate)

setCell :: Word8 -> Interpreter () 
setCell value = do 
    pstate <- get
    liftIO $ UM.write (tape pstate) (pos pstate) value
