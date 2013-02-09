module TypeChecker where

import Context

import AbsCPP
import PrintCPP
import ErrM

{-
	- Has to work in two passes
	- the first "build" the database of context
	- the second check and anotate the code
-}
typecheck :: Program -> Err ()
typecheck p = typeResult $ and $ 
                [ checkDef env def | 
                    (env, def) <- zip 
                                (map buildEnvOnDef (fromProg p)) 
                                (fromProg p) 
                ]
    where fromProg (PDefs defs) = defs

typeResult :: Bool -> Err ()
typeResult False = Bad "Error on type checking"
typeResult True  = Ok ()

buildEnvOnDef :: Def -> Env
buildEnvOnDef fun = buildEnvFromStatements (emptyEnv funSig) stms
    where  (funSig, stms) = funToSign fun

buildEnvFromStatements :: Env -> [Stm] -> Env
buildEnvFromStatements e ((SExp    exp)          :s) = buildEnvFromStatements e s
buildEnvFromStatements e ((SDecls  typ ids)      :s) = buildEnvFromStatements e s
buildEnvFromStatements e ((SInit   typ id expVal):s) = buildEnvFromStatements e s
buildEnvFromStatements e ((SReturn exp)          :s) = buildEnvFromStatements e s
buildEnvFromStatements e ((SWhile  exp stm)      :s) = buildEnvFromStatements e s
buildEnvFromStatements e ((SBlock  stms)         :s) = buildEnvFromStatements e s
buildEnvFromStatements e ((SIfElse exp stmI stmE):s) = buildEnvFromStatements e s
buildEnvFromStatements e []      = e

checkDef :: Env -> Def -> Bool
checkDef = undefined

-- infer type of exp
infer :: Env -> Exp -> Err Type
infer = undefined

-- Check type of exp
checkExp :: Env -> Exp -> Type -> Err ()
checkExp gamma e t = Ok ()

-- check sequence of statetments
checkStm :: Env -> Stm -> Err ()
checkStm gamma stm = Ok ()

-- check function definition
checkFun :: Env -> Def -> Err ()
checkFun gamma fun = Ok ()

-- Check a whole program
check :: Program -> Err ()
check prog = Ok ()


