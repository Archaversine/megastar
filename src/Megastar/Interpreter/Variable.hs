module Megastar.Interpreter.Variable (varAssign 
                                     , varModify 
                                     ) where 

import Control.Monad.State

import Data.Maybe (fromJust)
import Data.List (findIndex)
import Data.Word (Word8)

import Megastar.Interpreter.Core 

import qualified Data.Map.Strict as Map

varAssign :: String -> Word8 -> Interpreter () 
varAssign varname value = do 
    pstate <- get

    let (current, rest) = case vmap pstate of 
            []     -> (Map.empty, [])
            (x:xs) -> (x, xs)
        vmap' = Map.insert varname value current : rest

    put $ pstate { vmap = vmap' }

varModify :: String -> (Word8 -> Word8) -> Interpreter () 
varModify varname f = do 
    pstate <- get
    (before, current, rest) <- findVarMap varname

    let current' = Map.adjust f varname current
        vmap'    = before <> [current'] <> rest

    put $ pstate { vmap = vmap' }

findVarMap :: String -> Interpreter ([VarMap], VarMap, [VarMap])
findVarMap varname = do
    index          <- gets $ fromJust . findIndex (Map.member varname) . vmap
    (before, x:xs) <- gets $ splitAt index . vmap

    return (before, x, xs)

