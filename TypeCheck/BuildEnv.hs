module BuildEnv where

import Context

import AbsCPP
import PrintCPP
import ErrM

-- Create an env with a function
buildEnvOnDef :: Def -> [Signature] -> Env
buildEnvOnDef fun signs = addIOFun $ addListSigns (emptyEnv signature) signs
    where 
          (signature, _) = funToSign fun
          addListSigns (s, c, ss) ss2 = (s, c, ss++ss2)
addIOFun :: Env -> Env
addIOFun env = extendFun 
                (extendFun 
                    (extendFun 
                        (extendFun env readInt) 
                        printInt)
                    readDouble)
                printDouble

readInt :: Def
readInt  = DFun Type_int  (Id "readInt")  [] []
printInt :: Def
printInt = DFun Type_void (Id "printInt") [ADecl Type_int (Id "arg")] []
readDouble :: Def
readDouble  = DFun Type_double  (Id "readDouble")  [] []
printDouble :: Def
printDouble = DFun Type_void (Id "printDouble") [ADecl Type_double (Id "arg")] []


-- Update an env from a block of statement
buildEnvFromStatement :: Env -> Stm -> Err Env
buildEnvFromStatement e (SExp    exp)           = buildEnvFromExp e exp
buildEnvFromStatement e (SDecls  typ ids)       = newenv 
    where newenv = extendVars e ids typ
          extendVars e (id:[]) typ  = extendVar e id typ
          extendVars e (id:ids) typ = 
                extendVar e id typ >>= (\a -> extendVars a ids typ)
buildEnvFromStatement e (SInit   typ id expVal) = newenv
    where newenv = extendVar e id typ
buildEnvFromStatement e (SReturn exp)           = buildEnvFromExp e exp
buildEnvFromStatement e (SWhile  exp stm)       = do
    newEnv <- buildEnvFromExp e exp
    buildEnvFromStatement newEnv stm
-- With a block of statements, a new env is built later, 
-- when the typechecker reach this block
buildEnvFromStatement e (SBlock  stms)          = return e
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
