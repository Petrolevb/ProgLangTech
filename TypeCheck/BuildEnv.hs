module BuildEnv where

import Context

import AbsCPP
import PrintCPP
import ErrM

-- Create an env with a function
buildEnvOnDef :: Def -> [Signature] -> Env
buildEnvOnDef fun signs = addListSigns (emptyEnv signature) signs
    where 
          (signature, _) = funToSign fun
          addListSigns (s, c, ss) ss2 = (s, c, ss++ss2)

-- Update an env from a block of statement
buildEnvFromStatement :: Env -> Stm -> Err Env
buildEnvFromStatement e (SExp    exp)           = buildEnvFromExp e exp
buildEnvFromStatement e (SDecls  typ ids)       = newenv 
    where newenv = extendVars e ids typ
          extendVars e (id:[]) typ  = extendVar e id typ
          extendVars e (id:ids) typ = 
                (extendVar e id typ) >>= (\a -> extendVars a ids typ)
buildEnvFromStatement e (SInit   typ id expVal) = newenv
    where newenv = extendVar e id typ
buildEnvFromStatement e (SReturn exp)           = buildEnvFromExp e exp
buildEnvFromStatement e (SWhile  exp stm)       = do
    newEnv <- buildEnvFromExp e exp
    buildEnvFromStatement newEnv stm
-- With a block of statements, a new env is built later, 
-- when the typechecker reach this block
buildEnvFromStatement e (SBlock  stms)          = return $ newBlock e
buildEnvFromStatement e (SIfElse exp stmI stmE) = return e

-- Update an env from an expression
buildEnvFromExp :: Env -> Exp -> Err Env
buildEnvFromExp env exp = Ok env

-- Update an env with a new function
extendFun :: Env -> Def -> Env 
extendFun env def = addSig signature env
    where
        (signature, _) = funToSign def
        addSig sig (s, c, sigs) = (s, c, sigs ++ [sig])
