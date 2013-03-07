module Interpretter (interpret)  where

import AbsFunc

import ErrM

type Env = [(FIndent, Func)] -- List of all functions

--interpret :: (Print a, Show a) => Funcs -> Err ()
interpret :: Funcs -> IO ()
interpret prog = case searchFunc "main" (buildEnv prog) of
    Bad err -> putStrLn err
    Ok  _   -> putStrLn "Main found"

buildEnv :: Funcs -> Env
buildEnv (Program fs) = map 
    (\f -> case f of 
        DefFuncArg fi _ _ -> (fi, f)
        BasFunc fi _      -> (fi, f)
    ) fs

searchFunc :: String -> Env -> Err Func
searchFunc fu [] = Bad (fu ++ " not found")
searchFunc fu ((ss, f):env) | ss == FIndent fu = Ok f
                            | otherwise            = 
                              searchFunc fu env

evalFunc :: Env -> Func -> Err Integer
evalFunc env (DefFuncArg _ args exp) = undefined
evalFunc env (BasFunc _ exp) = evalExp env exp

evalExp :: Env -> Exp -> Err Integer
evalExp env (Identif a) = undefined
evalExp env (Integ i) = Ok i
evalExp env (Application fi e) = undefined
evalExp env (Addition e1 e2) = do
    r1 <- evalExp env e1
    r2 <- evalExp env e2
    Ok (r1 + r2)
evalExp env (Substrac e1 e2) = do
    r1 <- evalExp env e1
    r2 <- evalExp env e2
    Ok (r1 + r2)
evalExp env (Multipli e1 e2) = do
    r1 <- evalExp env e1
    r2 <- evalExp env e2
    Ok (r1 + r2)
evalExp env (Division e1 e2) = do
    r1 <- evalExp env e1
    r2 <- evalExp env e2
    Ok (r1 `div` r2)
evalExp env (Lt e1 e2) = do
    r1 <- evalExp env e1 
    r2 <- evalExp env e2
    if r1 < r2 
        then Ok 1
        else Ok 0
evalExp env (Gt e1 e2) = do
    r1 <- evalExp env e1 
    r2 <- evalExp env e2
    if r1 > r2 
        then Ok 1
        else Ok 0
evalExp env (LEt e1 e2) = do
    r1 <- evalExp env e1 
    r2 <- evalExp env e2
    if r1 <= r2 
        then Ok 1
        else Ok 0
evalExp env (GEt e1 e2) = do
    r1 <- evalExp env e1 
    r2 <- evalExp env e2
    if r1 <= r2 
        then Ok 1
        else Ok 0
evalExp env (Eq e1 e2) = do
    r1 <- evalExp env e1 
    r2 <- evalExp env e2
    if r1 ==  r2 
        then Ok 1
        else Ok 0
evalExp env (NEq e1 e2) = do
    r1 <- evalExp env e1 
    r2 <- evalExp env e2
    if r1 /= r2 
        then Ok 1
        else Ok 0
evalExp env (Condition e1 e2 e3) = do
    rif <- evalExp env e1
    if rif == 1
        then evalExp env e2
        else evalExp env e3
evalExp env (And  e1 e2) = do
    r1 <- evalExp env e1
    if r1 == 0
        then Ok 0
        else evalExp env e2
evalExp env (Or e1 e2 ) = do
    r1 <- evalExp env e1
    if r1 == 1
        then Ok 1
        else evalExp env e2
evalExp env (Abstraction fi e) = undefined
