module BuildEnv where

import Context

import AbsCPP
import PrintCPP
import ErrM

-- Create an env with a function
buildEnvOnDef :: Def -> Env
buildEnvOnDef fun = buildEnvFromStatements (emptyEnv funSig) stms
    where  (funSig, stms) = funToSign fun

-- Update an env from a block of statement
buildEnvFromStatements :: Env -> [Stm] -> Env
buildEnvFromStatements e ((SExp    exp)          :s) 
    = buildEnvFromStatements (buildEnvFromExp e exp) s
buildEnvFromStatements e ((SDecls  typ ids)      :s) = buildEnvFromStatements newenv s
    where newenv = extendVars e ids typ
          extendVars e (id:[]) typ = extendVar e id typ
          extendVars e (id:ids) typ = extendVars (extendVar e id typ) ids typ
buildEnvFromStatements e ((SInit   typ id expVal):s) = buildEnvFromStatements newenv s
    where newenv = extendVar e id typ
buildEnvFromStatements e ((SReturn exp)          :s) = buildEnvFromStatements e s
buildEnvFromStatements e ((SWhile  exp stm)      :s) = buildEnvFromStatements e s
-- With a block of statements, a new env is built later, when the typechecker reach this block
buildEnvFromStatements e ((SBlock  stms)         :s) = buildEnvFromStatements e s
buildEnvFromStatements e ((SIfElse exp stmI stmE):s) = buildEnvFromStatements e s
buildEnvFromStatements e []      = e

-- Update an env from an expression
buildEnvFromExp :: Env -> Exp -> Env
buildEnvFromExp env exp = env

-- Update an env with a new function
extendFun :: Env -> Def -> Env 
extendFun = undefined
