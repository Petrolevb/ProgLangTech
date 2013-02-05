module TypeCheck where

type Type  = String
type Ident = String
-- Env is a stack of context and the function initialisation
type Env     = (Signature, [Context])
type Context = [(Type, Ident)]

-- Has to wait an import
type Exp = String
type Statement = [String]
-- Definition is list of functions signatures
type Def = [Signature]
-- Signature is the name of the function and list of types
type Signature = (Ident, FunType)
type FunType   = ([Type], Type)

type Program = [Statement]

data Error a = Error String | Ok a

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

lookupVar :: Ident -> Env -> Error Type
lookupVar id (_, [])                   = Error "Variable not found"
lookupVar id (s, ([]:stack))           = lookupVar id (s, stack)
lookupVar id (s, (((t, i):env):stack)) | i == id   = Ok t
                                       | otherwise = lookupVar id (s, (env:stack))

lookupFun :: Ident -> Env -> Error FunType
lookupFun = undefined

extendVar :: Env -> Ident -> Type -> Env
extendVar (s, (gamma:stack)) id ty = (s, ((ty, id):gamma):stack)
extendVar (s, [])            id ty = (s, ((ty, id):[]):[])

extendFun :: Env -> Def -> Env 
extendFun = undefined

emptyEnv :: Env
emptyEnv = (("", ([], "")), [])

newblock :: Env -> Env
newblock (s, c) = (s, []:c)
 
gamma = extendVar (newblock $ extendVar (extendVar (emptyEnv) "x" "Int") "y" "Double") "z" "String"
