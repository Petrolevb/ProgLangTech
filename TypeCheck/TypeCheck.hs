module TypeCheck where

type Type  = String
type Ident = String
-- Env is a stack of context
type Env = [[(Type, Ident)]]


-- Has to wait an import
type Exp = String
type Statement = [String]
type Def = Bool
type FunType = Bool

type Program = [Statement]

-- infer type of exp
infer :: Env -> Exp -> Type
infer = undefined

-- Check type of exp
checkExp :: Env -> Exp -> Type -> Maybe String 
checkExp gamma e t = Nothing

-- check sequence of statetments
checkStm :: Env -> Statement -> Maybe String
checkStm gamma stm = Nothing

-- check function definition
checkFun :: Env -> Def -> Maybe String
checkFun gamma fun = Nothing

-- Check a whole program
check :: Program -> Maybe String
check prog = Nothing

lookupVar :: Ident -> Env -> Type
lookupVar = undefined
lookupFun :: Ident -> Env -> FunType
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

