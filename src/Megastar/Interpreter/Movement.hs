module Megastar.Interpreter.Movement ( moveLeft 
                                     , moveRight 
                                     , moveStart 
                                     , moveEnd 
                                     , circularLeft 
                                     , circularRight
                                     , bookmark 
                                     , jumpTo
                                     ) where 

import Control.Monad.State

import Megastar.Interpreter.Core 

import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed.Mutable as UM

moveLeft :: Interpreter () 
moveLeft = do 
    pstate <- get 

    let index' = pos pstate - 1
        pos'   = if index' < 0 then error "Negative Index" else index'

    put $ pstate { pos = pos' }

moveRight :: Interpreter () 
moveRight = do 
    pstate <- get 

    let index' = pos pstate + 1
        pos'   = if index' >= len pstate then error "Too Large Index" else index'

    put $ pstate { pos = pos' }

moveStart :: Interpreter () 
moveStart = modify $ \pstate -> pstate { pos = 0 }

moveEnd :: Interpreter () 
moveEnd = modify $ \pstate -> pstate { pos = len pstate - 1 }

circularLeft :: Interpreter () 
circularLeft = do 
    pstate <- get
    
    let p  = pos pstate
        t  = tape pstate
        p' = if p == 0 then UM.length t - 1 else p - 1

    put $ pstate { pos = p' }

circularRight :: Interpreter () 
circularRight = do 
    pstate <- get 

    let p  = pos pstate 
        t  = tape pstate
        p' = if p == UM.length t - 1 then 0 else p + 1

    put $ pstate { pos = p' }


bookmark :: String -> Interpreter ()
bookmark bookName = modify $ \pstate -> pstate { books = M.insert bookName (pos pstate) $ books pstate }

jumpTo :: String -> Interpreter () 
jumpTo bookName = do 
    pstate <- get

    let books' = books pstate
        pos'   = M.lookup bookName books'

    case pos' of 
        Nothing -> error $ "Bookmark " <> bookName <> " doesn't exist."
        Just p  -> put $ pstate { pos = p }
