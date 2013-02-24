module Interpreter where

import AbsCPP
import PrintCPP
import ErrM

import InterpreterContext

--import Data.Monad
import qualified Data.Map as M



interpret :: Program -> IO ()
interpret p = evalFun (Id "main") [] (createEnv p)

evalFun :: Id -> [Var] -> Env -> IO ()
evalFun id args env = case getBody id env of
    Right err -> putStrLn err
    Left (fun, funArg)  -> evalDef (addArgs env funArg args) fun


getBody :: Id -> Env -> Either ([Stm], [Var])  String
getBody id (_, []) = Right ("Function " ++ show id ++ " unknown")
getBody id (context, (idFun, body, args):funContext) 
    | idFun == id = Left (body, args)
    | otherwise   = getBody id (context, funContext)


{-
evalDefs :: [Signature] -> [Def] -> IO ()
evalDefs signs [def] | checkMain def = evalDef (buildEnvOnDef def signs) def
                     | otherwise     = putStrLn "No Main function found"
            where checkMain (DFun _ name _ _) = name == Id "main"
evalDefs signs (def:defs) | checkMain def = evalDef (buildEnvOnDef def signs) def
                          | otherwise = evalDefs signs defs
            where checkMain (DFun _ name _ _) = name == Id "main"


-}

evalDef :: Env -> [Stm] -> IO ()
evalDef env stms = do
    evalStatements env stms
    putStr ""



evalStatements :: Env -> [Stm] -> IO Value
evalStatements env [stm] = evalStatement env stm
evalStatements env (stm:stms) = do
    evalStatement env stm
    evalStatements env stms

evalStatement :: Env -> Stm -> IO Value
evalStatement env (SExp exp) = evalExp env exp
evalStatement env (SDecls ty ids) = undefined
evalStatement env (SInit ty id ex) = undefined
evalStatement env (SReturn exp) = undefined
evalStatement env (SWhile exp stm) = undefined
evalStatement env (SBlock stms)    = undefined
evalStatement env (SIfElse e stm stm2) = do
    ifExp <- evalExp env e
    if ifExp == VBool True
        then evalStatement env stm 
        else evalStatement env stm2


-- 0 for false, 1 for true, Nothing for void
evalExp :: Env -> Exp -> IO Value
evalExp = undefined
