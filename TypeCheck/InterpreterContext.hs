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

getVal :: Env -> Id -> Value
getVal = undefined

updateVal :: Env -> Id -> Value -> Env
updateVal = undefined
