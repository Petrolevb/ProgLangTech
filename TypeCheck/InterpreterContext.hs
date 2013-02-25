module InterpreterContext (
    Value (VDouble,VInt,VBool,VNul),
    Env,
    ValContext,
    FunContext,
    Var,

    vTimesv,
    vPlusv,
    vMinusv,
    vDivv,
    
    emptyEnv,
    createEnv,
    addVar,
    addArgs,
    getVal,
    updateVal,

    addNewBlock,
    removeBlock

) where

import AbsCPP
import LexCPP
import ParCPP
import ErrM

import Data.Maybe (fromMaybe)

data Value = VDouble Double 
           | VInt Integer 
           | VBool Bool
           | VNul 
    deriving Eq 


-- Disallow all comparison between types which doesn't match
-- since it's considered as an error in the typechecker
instance Ord Value where
    (VInt v1)    > (VInt v2)    = v1 > v2
    (VDouble v1) > (VDouble v2) = v1 > v2
    (VInt v1)    < (VInt v2)    = v1 < v2
    (VDouble v1) < (VDouble v2) = v1 < v2
    (VInt v1)    >= (VInt v2)    = v1 >= v2
    (VDouble v1) >= (VDouble v2) = v1 >= v2
    (VInt v1)    <= (VInt v2)    = v1 <= v2
    (VDouble v1) <= (VDouble v2) = v1 <= v2

vTimesv :: Value -> Value -> Value
(VInt i)    `vTimesv` (VInt i2)    = VInt (i*i2)
(VDouble i) `vTimesv` (VDouble i2) = VDouble (i*i2)
(VDouble d) `vTimesv` (VInt i)     = VDouble (d  * fromInteger i)
(VInt i) `vTimesv` (VDouble d)     = vTimesv (VDouble d) (VInt i)
vPlusv :: Value -> Value -> Value
(VInt i)    `vPlusv` (VInt i2)    = VInt (i+i2)
(VDouble i) `vPlusv` (VDouble i2) = VDouble (i+i2)
(VDouble d) `vPlusv` (VInt i)     = VDouble (d + fromInteger i)
(VInt i) `vPlusv` (VDouble d)     = vPlusv (VDouble d) (VInt i)
vMinusv :: Value -> Value -> Value
(VInt i)    `vMinusv` (VInt i2)    = VInt (i-i2)
(VDouble i) `vMinusv` (VDouble i2) = VDouble (i-i2)
(VDouble d) `vMinusv` (VInt i)     = VDouble (d - fromInteger i)
(VInt i) `vMinusv` (VDouble d)     = VDouble (fromInteger i - d)
vDivv :: Value -> Value -> Value
vDivv (VInt i) (VInt i2) = VInt (i `div`i2)


type Env = ([ValContext], [FunContext])

type ValContext = [Var]
type FunContext = (Id, [Stm], [Var])

type Var = (Id, Value)

emptyEnv :: Env
emptyEnv = ([], [])

createEnv :: Program -> Env
createEnv (PDefs defs) = ([], funCon)
    where 
        funCon = map (\(DFun _ name arg body) 
                        -> (name, body, vars arg)) (addIO defs)
        vars args = [ (id, VNul) | (ADecl _ id) <- args ]

addIO :: [Def] -> [Def]
addIO defs = readInt:readDouble:printInt:printDouble:defs

readInt :: Def
readInt  = DFun Type_int  (Id "readInt")  [] []
printInt :: Def
printInt = DFun Type_void (Id "printInt") [ADecl Type_int (Id "arg")] []
readDouble :: Def
readDouble  = DFun Type_double  (Id "readDouble")  [] []
printDouble :: Def
printDouble = DFun Type_void (Id "printDouble") [ADecl Type_double (Id "arg")] []

-- Function which take an environement
-- Variables name from function
-- Variables value from the call
-- Return the environement
addArgs :: Env -> [Var] -> [Value] -> Env
addArgs env [] [] = env
addArgs env ((id, _):argN) (val:argV) 
    = addArgs (addVar env (id, val)) argN argV

addVar :: Env -> Var -> Env
addVar ([], funContext) var = ([[var]], funContext)
addVar (context:stack, funContext) var 
    = ((context ++ [var]):stack, funContext)

getTopContext :: [ValContext] -> ValContext
getTopContext (c:_) = c

addNewBlock :: Env -> Env
addNewBlock (context, funContext) = ([]:context, funContext)
removeBlock :: Env -> Env
removeBlock (c:context, funContext) = (context, funContext)

getVal :: Env -> Id -> Value
getVal (context, _) = getInContexts context

getInContexts ::  [ValContext]-> Id -> Value
getInContexts [vars] id = fromMaybe undefined (getInContext vars id)
getInContexts (vars:context) id = fromMaybe (getInContexts context id) (getInContext vars id)

getInContext :: ValContext -> Id -> Maybe Value
getInContext [] _ = Nothing
getInContext ((idv, valv):vars) id | idv == id = Just valv
                                   | otherwise = getInContext vars id



updateVal :: Env -> Id -> Value -> Env
updateVal (context, f) id val = (updateInContexts context id val, f)

updateInContexts :: [ValContext] -> Id -> Value -> [ValContext]
updateInContexts [vars] id val = 
    case updateInContext vars id val of
        Nothing -> undefined -- Theorically impossible
        Just valcon -> [valcon]
updateInContexts (vars:ss) id val = 
    case updateInContext vars id val of
        Nothing -> vars : updateInContexts ss id val
        Just valcon -> valcon:ss

updateInContext :: ValContext -> Id -> Value -> Maybe [Var]
updateInContext [] _ _ = Nothing
updateInContext ((idv, valv):vars) id val 
    | idv == id = Just ((idv, val):vars)
    | otherwise = case updateInContext vars id val of
                Nothing -> Nothing
                Just newVC -> Just ((idv, valv):newVC)
