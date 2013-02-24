module Interpreter where

import AbsCPP
import PrintCPP
import ErrM

import InterpreterContext

--import Data.Monad
import qualified Data.Map as M

import Control.Monad.State


type PrintProg a = StateT Env IO a

--instance Show a => Show (PrintProg a) where 
--    show (StateT f) = show.f


interpret :: Program -> PrintProg ()
interpret p = evalFun (Id "main") [] (createEnv p)

evalFun :: Id -> [Var] -> Env -> PrintProg ()
evalFun id args env = do
    put env
    case getBody id env of
        Right err -> liftIO $ putStrLn err
        Left (fun, funArg)  -> evalDef (addArgs env funArg args) fun


getBody :: Id -> Env -> Either ([Stm], [Var])  String
getBody id (_, []) = Right ("Function " ++ show id ++ " unknown")
getBody id (context, (idFun, body, args):funContext) 
   | idFun == id = Left (body, args)
   | otherwise   = getBody id (context, funContext)


evalDef :: Env -> [Stm] -> PrintProg ()
evalDef env stms = do
    evalStatements env stms
    return ()


evalStatements :: Env -> [Stm] -> PrintProg ()
evalStatements env [stm] = evalStatement env stm
evalStatements env (stm:stms) = do
    evalStatement env stm
    evalStatements env stms

evalStatement :: Env -> Stm -> PrintProg ()
evalStatement env (SExp exp) = do
    evalExp exp
    return ()
evalStatement env (SDecls ty ids) = undefined
evalStatement env (SInit ty id exp) = do
    value <- evalExp exp
    let newEnv = addVar env (id, value)
    return ()
evalStatement env (SReturn exp) = undefined
evalStatement env (SWhile exp stm) = undefined
evalStatement env (SBlock stms)    = undefined
evalStatement env (SIfElse e stm stm2) = do
    ifExp <- evalExp e
    if ifExp == VBool True
        then evalStatement env stm 
        else evalStatement env stm2


-- 0 for false, 1 for true, Nothing for void
evalExp :: Exp -> PrintProg Value
evalExp (ETrue) = return $ VBool True
evalExp (EFalse) = return $ VBool False
evalExp (EInt i) = return $ VInt i
evalExp (EDouble d) = return $ VDouble d
evalExp (EId id) = do
    thenEnv <- get
    return $ getVal thenEnv id

evalExp (EApp id [exp]) = undefined

evalExp (EPIncr exp) = do
    (VInt val) <- evalExp exp
    return $ (VInt (val + 1))
evalExp (EPDecr exp) = do
    (VInt val) <- evalExp exp
    return $ (VInt (val - 1))
evalExp (EIncr exp) = do
    (VInt val) <- evalExp exp
    return $ (VInt (val + 1))
evalExp (EDecr exp) = do
    (VInt val) <- evalExp exp
    return $ (VInt (val - 1))

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
    (VBool val1) <- evalExp e1
    (VBool val2) <- evalExp e2
    return $ VBool (val1 < val2)
evalExp (EGt e1 e2) = do
    (VBool val1) <- evalExp e1
    (VBool val2) <- evalExp e2
    return $ VBool (val1 > val2)
evalExp (ELtEq e1 e2) = do
    (VBool val1) <- evalExp e1
    (VBool val2) <- evalExp e2
    return $ VBool (val1 <= val2)
evalExp (EGtWq e1 e2) = do
    (VBool val1) <- evalExp e1
    (VBool val2) <- evalExp e2
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
    (VBool val2) <- evalExp e2
    return $ VBool (val1 && val2)
evalExp (EOr e1 e2) = do
    (VBool val1) <- evalExp e1
    if val1 
        then return (VBool True)
        else do
            (VBool val2) <- evalExp e2
            return $ VBool val2

evalExp (EAss e1 e2) = undefined

applyFunc :: Value -> (Value -> Value -> Value) -> Value -> Value
applyFunc v1 f v2 = v1 `f` v2
