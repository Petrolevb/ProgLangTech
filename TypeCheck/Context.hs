module Context where

import AbsCPP
import LexCPP
import ParCPP
import ErrM

-- Signature is the name of the function and list of types
type Signature = (Id, FunType)
type FunType   = ([Type], Type)

-- Env is a stack of context and the function initialisation
type Env     = (Signature, [Context])
type Context = [(Type, Id)]

-- Building the context
-- Has to check if already exist
extendVar :: Env -> Id -> Type -> Env
extendVar (s, gamma:stack) id ty = (s, ((ty, id):gamma):stack)
extendVar (s, [])            id ty = ( s, [[(ty, id)]] )
emptyEnv :: Signature -> Env
emptyEnv s = (s, [])

newBlock :: Env -> Env
newBlock (s, c) = (s, []:c)


funToSign :: Def -> (Signature, [Stm])
funToSign (DFun typeFun name args stms) = ((name, (fromArgs args, typeFun)), stms)
    where fromArgs = map (\(ADecl ty _) -> ty) 


-- get informations from the context

lookupVar :: Id -> Env -> Err Type
lookupVar id (_, [])                 = Bad "Variable not found"
lookupVar id (s, []:stack)           = lookupVar id (s, stack)
lookupVar id (s, ((t, i):env):stack) | i == id   = Ok t
                                     | otherwise = lookupVar id (s, env:stack)

lookupFun :: Id -> Env -> Err FunType
lookupFun = undefined
