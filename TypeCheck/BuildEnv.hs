module BuildEnv where

import Context

import AbsCPP
import PrintCPP
import ErrM

-- Create an env with a function
buildEnvOnDef :: Def -> Env
buildEnvOnDef fun = emptyEnv signature
    where (signature, _) = funToSign fun

-- Update an env from a block of statement
buildEnvFromStatement :: Env -> Stm -> Env
buildEnvFromStatement e (SExp    exp)           = buildEnvFromExp e exp
buildEnvFromStatement e (SDecls  typ ids)       = newenv 
    where newenv = extendVars e ids typ
          extendVars e (id:[]) typ = extendVar e id typ
          extendVars e (id:ids) typ = extendVars (extendVar e id typ) ids typ
buildEnvFromStatement e (SInit   typ id expVal) = newenv
    where newenv = extendVar e id typ
buildEnvFromStatement e (SReturn exp)           = e
buildEnvFromStatement e (SWhile  exp stm)       = e
-- With a block of statements, a new env is built later, when the typechecker reach this block
buildEnvFromStatement e (SBlock  stms)          = e
buildEnvFromStatement e (SIfElse exp stmI stmE) = e

-- Update an env from an expression
buildEnvFromExp :: Env -> Exp -> Env
buildEnvFromExp env exp = env

-- Update an env with a new function
extendFun :: Env -> Def -> Env 
extendFun = undefined
