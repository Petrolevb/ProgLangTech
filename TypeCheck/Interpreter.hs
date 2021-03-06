{-# OPTIONS  -XTypeSynonymInstances -XFlexibleInstances #-}
module Interpreter where

import System.Exit (exitFailure)

import AbsCPP
import PrintCPP
import ErrM

import InterpreterContext

--import Data.Monad
import qualified Data.Map as M

import Control.Monad.State


type PrintProg a = StateT Env IO a

interpret :: Program -> IO ()
interpret p = 
    case searchMain p of
        Nothing -> do
            execStateT (evalFun (Id "main") [] (createEnv p)) emptyEnv
            putStr ""
        Just err -> do 
            putStr "SYNTAX ERROR"
            fail err
            exitFailure

searchMain :: Program -> Maybe String
searchMain (PDefs defs) = searchInDefs defs
    where searchInDefs [] = Just "No main function"
          searchInDefs (DFun _ id _ _ :defs) 
            | id == Id "main" = Nothing
            | otherwise         = searchInDefs defs
                            

evalFun :: Id -> [Value] -> Env -> PrintProg Value
evalFun id args env = do
    put env
    case getBody id env of
        Right err -> fail err
        Left (fun, funArg)  -> do 
            defEnv <- get 
            put $ addArgs defEnv funArg args
            evalDef fun


getBody :: Id -> Env -> Either ([Stm], [Var])  String
getBody id (_, []) = Right ("Function " ++ show id ++ " unknown")
getBody id (context, (idFun, body, args):funContext) 
   | idFun == id = Left (body, args)
   | otherwise   = getBody id (context, funContext)


evalDef :: [Stm] -> PrintProg Value
evalDef = evalStatements 


evalStatements :: [Stm] -> PrintProg Value
evalStatements [] = return VNul
evalStatements [stm] = evalStatement stm
evalStatements (stm:stms) = do
    evalStatement stm
    evalStatements stms

evalStatement :: Stm -> PrintProg Value
evalStatement (SExp exp) = evalExp exp
evalStatement (SDecls ty ids) = do
    env <- get
    put $ addIds ids env
    return VNul
        where 
            addIds [id] env = addVar env (id, VNul)
            addIds (id:ids) env = addVar (addIds ids env) (id, VNul)
evalStatement (SInit ty id exp) = do
    value <- evalExp exp
    env <- get
    put $ addVar env (id, value)
    return VNul
evalStatement (SReturn exp) = evalExp exp
evalStatement (SWhile exp stm) = do
    (VBool val) <- evalExp exp
    if val
        then do 
            evalStatement stm
            evalStatement (SWhile exp stm)
        else return VNul
evalStatement (SBlock stms)    = do
            env <- get
            put $ addNewBlock env
            evalStatements stms
            aft <- get
            put $ removeBlock aft
            return VNul
evalStatement (SIfElse e stm stm2) = do
    ifExp <- evalExp e
    if ifExp == VBool True
        then evalStatement stm 
        else evalStatement stm2


-- Special case for evalExp Application
evalExpA :: Exp -> PrintProg Value
evalExpA (EApp id exp) = do
    vars <- mapM evalExp exp
    (save, funcont) <- get
    returnVal <- evalFun id vars ([], funcont)
    put (save, funcont)
    return returnVal

-- 0 for false, 1 for true, Nothing for void
evalExp :: Exp -> PrintProg Value
evalExp (ETrue) = return $ VBool True
evalExp (EFalse) = return $ VBool False
evalExp (EInt i) = return $ VInt i
evalExp (EDouble d) = return $ VDouble d
evalExp (EId id) = do
    thenEnv <- get
    return $ getVal thenEnv id

evalExp (EApp id exp) = 
    case id of
        (Id "printInt") -> evalExp expS >>= appPrintInt 
        (Id "printDouble") -> evalExp expS >>= appPrintDouble
        (Id "readInt" ) -> appReadInt
        (Id "readDouble" ) -> appReadDouble
        (Id oth) -> evalExpA (EApp id exp)
    where expS = getTop exp
          getTop (e:_) = e

evalExp (EPIncr (EId id)) = do
    env <- get
    let value = getVal env id
    let newEnv = updateVal env id (value `vPlusv` VInt 1)
    put newEnv
    return value
evalExp (EPDecr (EId id)) = do
    env <- get
    let value = getVal env id
    let newEnv = updateVal env id (value `vMinusv` VInt 1)
    put newEnv
    return value
evalExp (EIncr (EId id)) = do
    value <- evalExp (EPIncr (EId id))
    return $ value `vPlusv` VInt 1
evalExp (EDecr (EId id)) = do
    value <- evalExp (EPDecr (EId id))
    return $ value `vMinusv` VInt 1

evalExp (ETimes e1 e2  ) = do
    val1 <- evalExp e1
    val2 <- evalExp e2
    return $ val1 `vTimesv` val2
evalExp (EDiv e1 e2) = do
    val1 <- evalExp e1
    val2 <- evalExp e2
    return $ val1 `vDivv` val2
evalExp (EPlus e1 e2) = do
    val1 <- evalExp e1
    val2 <- evalExp e2
    return $ val1 `vPlusv` val2
evalExp (EMinus e1 e2) = do
    val1 <- evalExp e1
    val2 <- evalExp e2
    return $ val1 `vMinusv` val2


evalExp (ELt e1 e2) = do
    val1 <- evalExp e1
    val2 <- evalExp e2
    return $ VBool (val1 < val2)
evalExp (EGt e1 e2) = do
    val1 <- evalExp e1
    val2 <- evalExp e2
    return $ VBool (val1 > val2)
evalExp (ELtEq e1 e2) = do
    val1 <- evalExp e1
    val2 <- evalExp e2
    return $ VBool (val1 <= val2)
evalExp (EGtWq e1 e2) = do
    val1 <- evalExp e1
    val2 <- evalExp e2
    return $ VBool (val1 >= val2)

evalExp (EEq e1 e2) = do
    val1 <- evalExp e1
    val2 <- evalExp e2
    return $ VBool (val1 == val2)
evalExp (ENEq e1 e2) = do
    val1 <- evalExp e1
    val2 <- evalExp e2
    return $ VBool (val1 /= val2)
evalExp (EAnd e1 e2) = do
    (VBool val1) <- evalExp e1
    if val1 
        then do
            (VBool val2) <- evalExp e2
            return $ VBool val2
        else return (VBool False)
evalExp (EOr e1 e2) = do
    (VBool val1) <- evalExp e1
    if val1 
        then return (VBool True)
        else do
            (VBool val2) <- evalExp e2
            return $ VBool val2

evalExp (EAss (EId id) e2) = do
    vallToAss <- evalExp e2
    env <- get
    put $ updateVal env id vallToAss
    return vallToAss

applyFunc :: Value -> (Value -> Value -> Value) -> Value -> Value
applyFunc v1 f v2 = v1 `f` v2


appPrintInt :: Value -> PrintProg Value
appPrintInt (VInt int) = do
    liftIO $ print int
    return VNul
appPrintDouble :: Value -> PrintProg Value
appPrintDouble (VDouble double) = do
    liftIO $ print double
    return VNul
appReadInt :: PrintProg Value
appReadInt = do
    val <- liftIO readInt
    return $ VInt val
appReadDouble :: PrintProg Value
appReadDouble = do
    val <- liftIO readDouble
    return $ VDouble val

readInt :: IO Integer
readInt = do
    val <- getLine
    return (read val :: Integer)
readDouble :: IO Double
readDouble = do
    val <- getLine
    return (read val :: Double)
