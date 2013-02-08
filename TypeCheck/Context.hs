module Context where

-- Signature is the name of the function and list of types
type Signature = (Ident, FunType)
type FunType   = ([Type], Type)

-- Env is a stack of context and the function initialisation
type Env     = (Signature, [Context])
type Context = [(Type, Ident)]

-- Building the context
-- Has to check if already exist
extendVar :: Env -> Ident -> Type -> Env
extendVar (s, (gamma:stack)) id ty = (s, ((ty, id):gamma):stack)
extendVar (s, [])            id ty = (s, ((ty, id):[]):[])

extendFun :: Env -> Def -> Env 
extendFun = undefined

emptyEnv :: Env
emptyEnv = (("", ([], "")), [])

newblock :: Env -> Env
newblock (s, c) = (s, []:c)



-- get informations from the context

lookupVar :: Ident -> Env -> Error Type
lookupVar id (_, [])                   = Error "Variable not found"
lookupVar id (s, ([]:stack))           = lookupVar id (s, stack)
lookupVar id (s, (((t, i):env):stack)) | i == id   = Ok t
                                       | otherwise = lookupVar id (s, (env:stack))

lookupFun :: Ident -> Env -> Error FunType
lookupFun = undefined
