module Megastar.Interpreter.Movement ( moveLeft 
                                     , moveRight 
                                     , moveStart 
                                     , moveEnd 
                                     , bookmark 
                                     , jumpTo
                                     ) where 

import Control.Monad.State

import Megastar.Interpreter.Core 

import qualified Data.Map.Strict as M

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
