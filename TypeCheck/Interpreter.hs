module Interpreter where

import AbsCPP
import PrintCPP
import ErrM

import Monad
import qualified Data.Map as M

interpret :: Program -> IO ()
interpret p = putStrLn "no interpreter yet"
