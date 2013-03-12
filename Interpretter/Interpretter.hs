module Interpretter (interpret)  where

import System.Exit (exitFailure)

import AbsFunc

import ErrM

type Env = [(FIndent, Func)] -- List of all functions
type FuncEnv = ([(FIndent, Integer)], Env)
--interpret :: (Print a, Show a) => Funcs -> Err ()
interpret :: Funcs -> IO ()
interpret prog = case searchFunc "main" envProg of
    Bad err  -> do
        putStrLn "INTERPRETTER ERROR"
        putStrLn err
        exitFailure
    Ok  main -> case evalFunc envProg main of
        Bad err -> do
            putStrLn "INTERPRETTER ERROR"
            putStrLn err
            exitFailure
        Ok i    -> putStrLn $ show i
  where envProg = buildEnv prog

buildEnv :: Funcs -> FuncEnv
buildEnv (Program fs) = ([], map 
    (\f -> case f of 
        DefFuncArg fi _ _ -> (fi, f)
        BasFunc fi _      -> (fi, f)
    ) fs)

searchFunc :: String -> FuncEnv -> Err Func
searchFunc fu (_, []) = Bad (fu ++ " not found")
searchFunc fu (e, ((ss, f):env))
                      | ss == FIndent fu = Ok f
                      | otherwise        = 
                              searchFunc fu (e, env)


addArgs :: [Arg] -> FuncEnv -> FuncEnv
addArgs args (funcEnv, env) = (add args funcEnv, env)
    where add [] f = f
          add ((Argument arg):args) ((_, value):vars) 
                    = (arg, value):add args vars

-- Values added before the call of a function
addValues :: [Integer] -> FuncEnv -> Err FuncEnv
addValues values (vals, env) = Ok (add values vals, env)
    where add [] vals = vals
          add (i, is) vals = (FIndent "", i) : add is vals

evalFunc :: FuncEnv -> Func -> Err Integer
evalFunc env (DefFuncArg _ args exp) = evalExp (addArgs args env) exp
evalFunc env (BasFunc _ exp) = evalExp env exp

evalExp :: FuncEnv -> Exp -> Err Integer
evalExp env (Identif a) = searchFunc (fromFI a) env >>= evalFunc env
    where fromFI (FIndent s) = s
evalExp env (Integ i) = Ok i
evalExp env (Application (FIndent fi) exp) = do
    is <- mapM (evalExp env) exp
    newEnv <- addValues is env
    func <- searchFunc fi newEnv
    evalFunc newEnv func

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
