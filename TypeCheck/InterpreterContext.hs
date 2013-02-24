module InterpreterContext where

import AbsCPP
import LexCPP
import ParCPP
import ErrM

data Value = VDouble Double 
           | VInt Integer 
           | VBool Bool
           | VNul 
    deriving Eq

vTimesv :: Value -> Value -> Value
(VInt i)    `vTimesv` (VInt i2) = VInt (i*i2)
(VDouble i) `vTimesv` (VDouble i2) = VDouble (i*i2)
vPlusv :: Value -> Value -> Value
(VInt i)    `vPlusv` (VInt i2) = VInt (i+i2)
(VDouble i) `vPlusv` (VDouble i2) = VDouble (i+i2)
vMinusv :: Value -> Value -> Value
(VInt i)    `vMinusv` (VInt i2) = VInt (i-i2)
(VDouble i) `vMinusv` (VDouble i2) = VDouble (i-i2)
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
                        -> (name, body, vars arg)) defs
        vars args = [ (id, VNul) | (ADecl _ id) <- args ]

-- Function which take an environement
-- Variables name from function
-- Variables value from the call
-- Return the environement
addArgs :: Env -> [Var] -> [Var] -> Env
addArgs env [] [] = env
addArgs env ((id, _):argN) ((_, val):argV) 
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
getInContexts [vars] id = 
    case getInContext vars id of
        Nothing -> undefined -- Theorically impossible
        Just val -> val
getInContexts (vars:context) id = 
    case getInContext vars id of
        Nothing -> getInContexts context id
        Just val -> val

getInContext :: ValContext -> Id -> Maybe Value
getInContext [] _ = Nothing
getInContext ((idv, valv):vars) id | idv == id = Just valv
                                   | otherwise = getInContext vars id



updateVal :: Env -> Id -> Value -> Env
updateVal = undefined
