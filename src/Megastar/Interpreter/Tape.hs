module Megastar.Interpreter.Tape ( bulkAllocTape 
                                 , allocTapeString 
                                 , allocTapeFile
                                 ) where

import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Word (Word8)

import Control.Monad.State

import Megastar.Interpreter.Core

resetTapeData :: ProgState -> ProgState 
resetTapeData pstate = pstate { pos = 0 
                              , books = mempty
                              , vmap  = mempty 
                              , fnmap = mempty
                              }

bulkAllocTape :: Int -> Word8 -> Interpreter ()
bulkAllocTape size value = do 
    pstate  <- gets resetTapeData
    newTape <- liftIO $ UM.replicate size value

    put $ pstate { tape = newTape 
                 , len  = size 
                 }


allocTapeString :: [Word8] -> Interpreter ()
allocTapeString xs = do 
    pstate  <- gets resetTapeData 

    -- Convert the list of bytes to a mutable vector 
    -- Because apparently that's the only way to do it
    newTape <- liftIO $ do 
        t <- UM.replicate (length xs) 0
        mapM_ (uncurry $ UM.write t) $ zip [0..] xs
        return t

    put $ pstate { tape = newTape 
                 , len  = length xs 
                 }

allocTapeFile :: FilePath -> Interpreter () 
allocTapeFile path = do 
    contents <- liftIO $ readFile path
    allocTapeString $ map (fromIntegral . fromEnum) contents
