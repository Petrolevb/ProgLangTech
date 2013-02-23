module TypeChecker where

import BuildEnv

import Context

import AbsCPP
import PrintCPP
import ErrM

typecheck :: Program -> Err ()
typecheck p = checkDefs (getSignatures p) (fromProg p)
    where fromProg (PDefs defs)      = defs
          getSignatures (PDefs defs) =
            [ sig | (sig, _) <- map funToSign defs ]

-- Take the list of all signatures of all functions
-- Take the list of definitions
-- Typecheck all definitions
checkDefs ::  [Signature] -> [Def] -> Err () 
checkDefs signs (def:[]) = 
    checkDef (buildEnvOnDef def signs) def
checkDefs signs (def:ds) = do
    checkDef (buildEnvOnDef def signs) def 
    checkDefs signs ds

typeResult :: Bool -> Err ()
typeResult False = Bad "Error on type checking"
typeResult True  = Ok ()


-- empty env with the function signature
checkDef :: Env -> Def -> Err ()
checkDef env (DFun _ _ _ body) = checkStms env body

checkStms :: Env -> [Stm] -> Err ()
checkStms gamma (s:[]) = do
    newEnv <- buildEnvFromStatement gamma s
    checkStm newEnv s
checkStms gamma (s:ss) = do
    newGamma <- buildEnvFromStatement gamma s
    checkStm newGamma s
    checkStms newGamma ss

-- check sequence of statetments
checkStm :: Env -> Stm -> Err ()
checkStm gamma (SExp e) = do
    infer gamma e -- if we can get the type, it's ok
    Ok ()
checkStm gamma (SDecls t ids) = undefined
    -- add t ids in gamma ?
checkStm gamma (SInit t i e) = do
    checkExp gamma e t
    -- add t i in gamma ?
checkStm gamma (SReturn e) =  do
    treturn <- infer gamma e
    tFun <- getFunType gamma
    typeResult (tFun == treturn)
checkStm gamma (SWhile e s) = do
    checkExp gamma e Type_bool
    checkStm gamma s
checkStm gamma (SBlock stms) = do
    -- add a new context on the stack
    let newGamma = newBlock gamma
    checkStms newGamma stms
checkStm gamma (SIfElse e s1 s2) = do
    checkExp gamma e Type_bool
    checkStm gamma s1
    checkStm gamma s2
--checkStm _ _ = typeResult False


-- Check type of exp
checkExp :: Env -> Exp -> Type -> Err ()
checkExp gamma  ETrue           Type_bool   = Ok ()
checkExp gamma  EFalse          Type_bool   = Ok ()
checkExp gamma (EInt    i     ) Type_int    = Ok ()
checkExp gamma (EDouble d     ) Type_double = Ok ()
checkExp gamma (EId    id     ) t = do
    tId <- infer gamma (EId id)
    typeResult (tId == t)

checkExp gamma (EApp   id exp) t = do
    (tysids, typeFun) <- lookupFun id gamma
    -- Function return same type as requested
    typeResult (t == typeFun)
    -- Same number of argument as requested
    typeResult ((length exp) == (length tysids))
    checkAllArgs tysids exp
        where 
            checkAllArgs [(ty, _)] [exp] = 
                checkExp gamma exp ty
            checkAllArgs ((ty,_):tysids) (exp:exps) = do
                checkExp gamma exp ty
                checkAllArgs tysids exps
 
checkExp gamma (EPIncr e      ) t = checkExp gamma e t
checkExp gamma (EPDecr e      ) t = checkExp gamma e t

checkExp gamma (ETimes e1  e2)  t = do
    te1 <- infer gamma e1
    te2 <- infer gamma e2 
    tfin <- (maxType te1 te2)
    typeResult (tfin == t)
checkExp gamma (EDiv   e1  e2)  t = checkExp gamma (ETimes e1 e2) t
checkExp gamma (EPlus  e1  e2)  t = checkExp gamma (ETimes e1 e2) t
checkExp gamma (EMinus e1  e2)  t = checkExp gamma (ETimes e2 e2) t

checkExp gamma (ELt    e1  e2)  Type_bool = 
    checkExp gamma (EGtWq e1 e2) Type_bool
checkExp gamma (EGt    e1  e2)  Type_bool = 
    checkExp gamma (EGtWq e1 e2) Type_bool
checkExp gamma (ELtEq  e1  e2)  Type_bool = 
    checkExp gamma (EGtWq e1 e2) Type_bool
checkExp gamma (EGtWq  e1  e2)  Type_bool = do
    te1 <- infer gamma e1
    typeResult (te1 `elem` [Type_int, Type_double])
    checkExp gamma e2 te1

checkExp gamma (EEq    e1  e2)  Type_bool = 
    checkExp gamma (ENEq e1 e2) Type_bool
checkExp gamma (ENEq   e1  e2)  Type_bool = do 
    t1 <- infer gamma e1
    t2 <- infer gamma e2
    typeResult (t1 == t2)

checkExp gamma (EAnd   e1  e2)  Type_bool = 
    checkExp gamma (EOr e1 e2) Type_bool
checkExp gamma (EOr    e1  e2)  Type_bool = do
    checkExp gamma e1 Type_bool
    checkExp gamma e2 Type_bool

checkExp gamma (EAss   e1  e2)  t =  do
    te1 <- infer gamma e1
    checkExp gamma e2 te1 -- e2 has to be the type of e1
    typeResult (te1 == t)

checkExp _ _ _ = typeResult False


-- Return the maxType between two type
-- void < bool < int < double < string
maxType :: Type -> Type -> Err Type
maxType Type_void _ = Bad "bad type comparison"
maxType Type_bool _ = Bad "bad type comparison"
maxType _ Type_void = Bad "bad type comparison"
maxType _ Type_bool = Bad "bad type comparison"
maxType Type_int a = Ok a
maxType _        Type_double = Ok Type_double


newFunc :: Stm -> Def
newFunc s = DFun Type_int (Id "main") [] [s]

-- infer type of exp
infer :: Env -> Exp -> Err Type
infer gamma (ETrue)        = Ok Type_bool
infer gamma (EFalse)       = Ok Type_bool
infer gamma (EInt _)       = Ok Type_int
infer gamma (EDouble _)    = Ok Type_double
infer gamma (EId id)       = lookupVar id gamma
infer gamma (EApp id exps) = Bad "EApp not implemented"

infer gamma (EIncr e)      = do
    te <- infer gamma e
    maxType Type_double te
infer gamma (EPIncr e)     = infer gamma (EIncr e)
infer gamma (EPDecr e)     = infer gamma (EIncr e)
infer gamma (EDecr e)      = infer gamma (EIncr e)

infer gamma (EPlus e1 e2)  = do
    te1 <- infer gamma e1
    te2 <- infer gamma e2
    maxType te1 te2
infer gamma (ETimes e1 e2) = infer gamma (EPlus e1 e2)
infer gamma (EDiv e1 e2)   = infer gamma (EPlus e1 e2)
infer gamma (EMinus e1 e2) = infer gamma (EPlus e1 e2)

infer gamma (ELt e1 e2)    = do
    te1 <- infer gamma e1
    checkExp gamma e2 te1
    return Type_bool 
infer gamma (EGt e1 e2)    = infer gamma (ELt e1 e2)
infer gamma (ELtEq e1 e2)  = infer gamma (ELt e1 e2)
infer gamma (EGtWq e1 e2)  = infer gamma (ELt e1 e2)
infer gamma (ENEq e1 e2)   = infer gamma (ELt e1 e2)
infer gamma (EEq e1 e2)    = infer gamma (ELt e1 e2)

infer gamma (EAnd e1 e2)   = do
    checkExp gamma e1 Type_bool
    checkExp gamma e2 Type_bool
    return Type_bool
infer gamma (EOr e1 e2)    = infer gamma (EAnd e1 e2)
infer gamma (EAss e1 e2)   = Bad "EAss not implemented"
