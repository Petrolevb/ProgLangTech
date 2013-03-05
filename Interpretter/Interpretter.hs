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

evalExp :: Env -> Exp -> Err Integer
evalExp env (Identif a) = undefined
evalExp env (Integ i) = Ok i
evalExp env (Application fi e) = undefined
evalExp env (Addition e1 e2) = undefined
evalExp env (Substrac e1 e2) = undefined
evalExp env (Multipli e1 e2) = undefined
evalExp env (Division e1 e2) = undefined
evalExp env (Lt e1 e2) = undefined
evalExp env (Gt e1 e2) = undefined
evalExp env (LEt e1 e2) = undefined
evalExp env (GEt e1 e2) = undefined
evalExp env (Eq e1 e2) = undefined
evalExp env (NEq e1 e2) = undefined
evalExp env (Condition e1 e2 e3) = undefined
evalExp env (Abstraction fi e) = undefined
