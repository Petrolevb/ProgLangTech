module Context where

import AbsCPP
import LexCPP
import ParCPP
import ErrM

-- Signature is the name of the function and list of types
type Signature = (Id, FunType)
type FunType   = ([(Type,Id)], Type)

-- Env is a stack of context and the function initialisation
type Env     = (Signature, [Context], [Signature])
type Context = [(Type, Id)]


extendVar :: Env -> Id -> Type -> Err Env
extendVar (s, gamma:stack, sigs)   id ty = 
    case (lookupVar id (s, gamma:stack, sigs)) of
        Bad _ -> Ok (s, ((ty, id):gamma):stack, sigs) -- not found, so ok
        Ok  _ -> Bad "Variable already exist"
extendVar (s, [], sigs) id ty = 
    case (lookupVar id (s, [], sigs)) of
        Bad _ -> Ok (s, [[(ty, id)]], sigs)
        Ok  _ -> Bad "Variable already exist"

emptyEnv :: Signature -> Env
emptyEnv s = (s, [], [])

newBlock :: Env -> Env
newBlock (s, c, ss) = (s, []:c, ss)


funToSign :: Def -> (Signature, [Stm])
funToSign (DFun typeFun name args stms) = ((name, (fromArgs args, typeFun)), stms)
    where fromArgs = map (\(ADecl ty id) -> (ty, id)) 


-- get informations from the context

lookupVar :: Id -> Env -> Err Type
lookupVar id (s, [], ss)                 = lookupInFun id (s, [], ss)
lookupVar id (s, []:stack, ss)           = lookupVar id (s, stack, ss)
lookupVar id (s, ((t, i):env):stack, ss) | i == id   = Ok t
                                         | otherwise = lookupVar id (s, env:stack, ss)


-- check in the signature of the function
lookupInFun :: Id -> Env -> Err Type
lookupInFun id ((_, (args, _)), _, _) = lookInArgs id args
    where   lookInArgs id []  = Bad "Variable not found"
            lookInArgs id ((tyArg,idArg):args) 
                | id == idArg = Ok tyArg
                | otherwise   = lookInArgs id args

lookupFun :: Id -> Env -> Err FunType
lookupFun id (_, _, sigs)      = lookInSigs id sigs 
    where lookInSigs id [] = Bad "Fun not found"
          lookInSigs id ((idFun, tyFun):sigs) 
            | id == idFun = Ok tyFun
            | otherwise   = lookInSigs id sigs

getFunType :: Env -> Err Type
getFunType (sig, _, _) = Ok $ getFromSig sig
    where getFromSig (_, (_, typeFun) ) = typeFun
