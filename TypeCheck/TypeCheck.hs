module TypeCheck where

type Type  = String
type Ident = String
-- Env is a stack of context
type Env = [[(Type, Ident)]]

data Error a = Error String | Ok a

-- Has to wait an import
type Exp = String
type Statement = [String]
type Def = Bool
type FunType = Bool

type Program = [Statement]

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
lookupVar id []                   = Error "Error"
lookupVar id ([]:stack)           = lookupVar id stack
lookupVar id (((t, i):env):stack) | i == id   = Ok t
                                  | otherwise = lookupVar id (env:stack)

lookupFun :: Ident -> Env -> Error FunType
lookupFun = undefined

extendVar :: Env -> Ident -> Type -> Env
extendVar (gamma:stack) id ty = ((ty, id):gamma):stack
extendVar []            id ty = ((ty, id):[]):[]

extendFun :: Env -> Def -> Env 
extendFun = undefined

emptyEnv :: Env
emptyEnv = []

newblock :: Env -> Env
newblock e = []:e
 
gamma = extendVar (newblock $ extendVar (extendVar (emptyEnv) "x" "Int") "y" "Double") "z" "String"
