module Megastar.Interpreter.IO ( printCell 
                               , printCellValue
                               , readCell 
                               , readCellValue 
                               , outString
                               ) where

import Control.Monad.State

import Megastar.Interpreter.Core

import qualified Data.Vector.Unboxed.Mutable as UM

printCell :: Interpreter ()
printCell = do 
    s <- get
    x <- liftIO $ UM.read (tape s) (pos s) 

    liftIO $ putStr [toEnum $ fromIntegral x]

printCellValue :: Interpreter () 
printCellValue = do 
    s <- get 
    x <- liftIO $ UM.read (tape s) (pos s)

    liftIO $ putStr (show x)

readCell :: Interpreter () 
readCell = do
    pstate <- get
    value  <- fromIntegral . fromEnum <$> liftIO getChar

    liftIO $ UM.write (tape pstate) (pos pstate) value

readCellValue :: Interpreter () 
readCellValue = do 
    pstate <- get 
    value  <- read <$> liftIO getLine

    liftIO $ UM.write (tape pstate) (pos pstate) value

outString :: String -> Interpreter () 
outString = liftIO . putStr
