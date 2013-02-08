module TypeChecker where

import Context

import AbsCPP
import PrintCPP
import ErrM

-- Type from CPP.cf
--type Type  = "int" | "double" | "bool" | "void" 
type Ident = String

-- Return the good thing or an error by a string
type Error a = Error String | Ok a

-- Type Program from CPP.cf
-- Type Exp     from CPP.cf


typecheck :: Program -> Err ()
typecheck p = typeResult $ all $ map checkDef (map buildEnvOnDef p)

typeResult :: Bool -> Err ()
typeResult = undefined
{-
	- Has to work in two passes
	- the first "build" the database of context
	- the second check and anotate the code
-}

-- Program = [Def] 
-- check the Def just after building the environment
buildEnvOnDef :: Def -> Env
buildEnvOnDef def = undefined

checkDef :: Env -> Def -> Bool
checkDef = undefined

-- infer type of exp
infer :: Env -> Exp -> Error Type
infer = undefined

-- Check type of exp
checkExp :: Env -> Exp -> Type -> Error ()
checkExp gamma e t = Ok ()

-- check sequence of statetments
checkStm :: Env -> Statement -> Error ()
checkStm gamma stm = Ok ()

-- check function definition
checkFun :: Env -> Def -> Error ()
checkFun gamma fun = Ok ()

-- Check a whole program
check :: Program -> Error ()
check prog = Ok ()


