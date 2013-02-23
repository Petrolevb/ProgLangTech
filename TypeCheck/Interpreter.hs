module Interpreter where

import AbsCPP
import PrintCPP
import ErrM

import BuildEnv
import Context

--import Data.Monad
import qualified Data.Map as M


data Value = VDouble Double | VInt Integer | VBool Bool
    deriving Eq

interpret :: Program -> IO ()
interpret p = evalDefs (getSignatures p) (fromProg p)
    where fromProg (PDefs defs) = defs
          getSignatures (PDefs defs) = 
            [ sig | (sig, _) <- map funToSign defs ]

evalDefs :: [Signature] -> [Def] -> IO ()
evalDefs signs [def] | checkMain def = evalDef (buildEnvOnDef def signs) def
                     | otherwise     = putStrLn "No Main function found"
            where checkMain (DFun _ name _ _) = name == Id "main"
evalDefs signs (def:defs) | checkMain def = evalDef (buildEnvOnDef def signs) def
                          | otherwise = evalDefs signs defs
            where checkMain (DFun _ name _ _) = name == Id "main"


evalDef :: Env -> Def -> IO ()
evalDef env (DFun _ _ _ stms) = evalStatements env stms



evalStatements :: Env -> [Stm] -> IO ()
evalStatements env [stm] = evalStatement env stm
evalStatements env (stm:stms) = do
    evalStatement env stm
    evalStatements env stms

evalStatement :: Env -> Stm -> IO ()
--evalStatements (SExp exp) = evalExp exp
evalStatement env (SDecls ty ids) = undefined
evalStatement env (SInit ty id ex) = undefined
evalStatement env (SReturn exp) = undefined
evalStatement env (SWhile exp stm) = undefined
evalStatement env (SBlock stms)    = undefined
evalStatement env (SIfElse e stm stm2) = do
    ifExp <- evalExp env e
    if ifExp == (VBool True)
        then evalStatement env stm 
        else evalStatement env stm2


-- 0 for false, 1 for true, Nothing for void
evalExp :: Env -> Exp -> IO Value
evalExp = undefined
