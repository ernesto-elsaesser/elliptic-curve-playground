import Control.Monad.State

-----------------
-- Unification --
-----------------

type TypeName = String
data Type = TVar(TypeName) | TyTerm(TypeName, [Type])

type SubstitutionRule = (Type, Type)
type Substitution = [SubstitutionRule]

unify :: Substitution -> Substitution -> Substitution
-- case term = term:
unify ((TyTerm(t1, args1), TyTerm(t2, args2)) : e1') e2
    | t1 /= t2 = error ("Type mismatch: " ++ t1 ++ " = " ++ t2)
    | length args1 /= length args2 = error ("Argument count mismatch: " ++ t1)
    | otherwise = unify ((zip args1 args2) ++ e1') e2 -- (Reduce)
-- case var = var:
unify ((TVar(v1), TVar(v2)) : e1') e2 
    | v1 == v2 = unify e1' e2 -- (Erase)
    | otherwise = let s = (TVar(v1), TVar(v2)) 
                  in unify (substAll s e1') (s : e2) -- (Subst)
-- case term = var:
unify ((TyTerm(t, args), TVar(v)) : e1') e2
    = unify ((TVar(v), TyTerm(t, args)) : e1') e2 -- (Swap)
-- case var = term:
unify ((TVar(v), TyTerm(t, args)) : e1') e2
    | occurs args v = error ("Recursive reference: " ++ v)
    | otherwise = let s = (TVar(v), TyTerm(t, args))
                  in unify (substAll s e1') (s : (substAll s e2)) -- (Subst)
-- end:
unify [] e = e

substAll :: SubstitutionRule -> Substitution -> Substitution
substAll s e = map (\(t1,t2) -> (subst [s] t1, subst [s] t2)) e

occurs :: [Type] -> TypeName -> Bool
occurs (TyTerm(_, args) : tail) str = occurs args str || occurs tail str
occurs (TVar(vstr) : tail) str = vstr == str || occurs tail str
occurs [] str = False

subst :: Substitution -> Type -> Type
subst s (TyTerm(str, args)) = TyTerm(str, map (subst s) args)
subst ((TVar(str1), ty) : sus) (TVar(str2)) | str1 == str2 = ty
                                            | otherwise = subst sus (TVar(str2))
subst [] ty = ty

----------------------------------------
-- Counter Monad (for variable names) --
----------------------------------------

type CM = State Int

freshName :: CM TypeName
freshName = state (\i -> (("t" ++ show i), i+1))
-- NOTE: the names t1, t2, ... must not be used in expressions

freshTVar :: CM Type
freshTVar = freshName >>= (\name -> return (TVar(name)))

--------------------
-- Type Inference --
--------------------

data TypeScheme = Mono Type | Poly TypeName TypeScheme deriving (Show)
data Assumption = A TypeName TypeScheme deriving (Show)

makeFun :: Type -> Type -> Type
makeFun tin tout = TyTerm("->", [tin, tout])

-- Expression Language
data Expr = Var TypeName
    | App Expr Expr
    | Abs TypeName Expr
    | Let TypeName Expr Expr deriving (Show)

infer :: [Assumption] -> Expr -> (Substitution, Type)
infer a e = evalState (_infer a e) 1

_infer :: [Assumption] -> Expr -> CM (Substitution, Type)

-- rule i
_infer a (Var x) = do
    newType <- deriveInstance x a
    return ([], newType)

-- rule ii
_infer a (App e1 e2) = do 
    (s1, t1) <- _infer a e1
    (s2, t2) <- _infer (applySub s1 a) e2
    newType <- freshTVar
    let { funType = makeFun t2 newType
        ; s3 = unifyPair (subst s2 t1) funType
        ; s = s1 ++ s2 ++ s3 }
    return (s, subst s newType)

-- rule iii
_infer a (Abs x e) = do
    newType <- freshTVar
    (s, t1) <- _infer (A x (Mono newType) : strip a x) e
    let t2 = subst s newType
    return (s, (makeFun t2 t1))

-- rule iv
_infer a (Let x e1 e2) = do
    (s1, t1) <- _infer a e1
    genType <- return (gen a t1)
    (s2, t) <- _infer (A x genType : (applySub s1 (strip a x))) e2
    let s = s1 ++ s2
    return (s, t)

deriveInstance :: TypeName -> [Assumption] -> CM Type
deriveInstance x (A x' ts : as)
    | x == x' = instantiateScheme ts
    | otherwise = deriveInstance x as
deriveInstance x [] = error ("No assumption for " ++ x) -- for debugging

instantiateScheme :: TypeScheme -> CM Type
instantiateScheme (Mono t) = return t
instantiateScheme (Poly name ts) = do
    newName <- freshName
    inst <- instantiateScheme ts
    return (swapNames name newName inst)

swapNames :: TypeName -> TypeName -> Type -> Type
swapNames old new (TVar(var)) = if var == old then TVar new else TVar var
swapNames old new (TyTerm(str, args)) = TyTerm(str, (map (swapNames old new) args))

applySub :: Substitution -> [Assumption] -> [Assumption]
applySub s a = map (substAsmpt s) a

substAsmpt :: Substitution -> Assumption -> Assumption
substAsmpt s (A x ts) = A x (substScheme s ts)

substScheme :: Substitution -> TypeScheme -> TypeScheme
substScheme s (Mono t) = Mono (subst s t)
substScheme s (Poly name ts) = Poly (substName s name) (substScheme s ts)

substName :: Substitution -> TypeName -> TypeName
substName (((TVar(str1)), TVar(str2)) : sus) name
    | str1 == name = str2
    | otherwise = substName sus name
substName [] name = name

unifyPair :: Type -> Type -> Substitution
unifyPair t1 t2 = unify [(t1, t2)] []

strip :: [Assumption] -> TypeName -> [Assumption]
strip a name = filter (\(A x _) -> x /= name) a

gen :: [Assumption] -> Type -> TypeScheme
gen a t = let
    typeNames = allNames t
    vars = filter (not . boundInContext a) typeNames
    in makePoly vars t

allNames :: Type -> [TypeName]
allNames t = uniqeNames t []

uniqeNames :: Type -> [TypeName] -> [TypeName]
uniqeNames (TVar(v)) set 
    | elem v set = set
    | otherwise = v : set
uniqeNames (TyTerm(t, (arg : args))) set = uniqeNames (TyTerm(t, args)) (uniqeNames arg set)
uniqeNames (TyTerm(_, [])) set = set

boundInContext :: [Assumption] -> TypeName -> Bool
boundInContext (A _ ts : as) name = boundInScheme ts name || boundInContext as name
boundInContext [] _ = False

boundInScheme :: TypeScheme -> TypeName -> Bool
boundInScheme (Mono t) name = occurs [t] name
boundInScheme (Poly vname ts) name = name == vname || boundInScheme ts name
-- if a name appears as quantor, we already know that it will occur

makePoly :: [TypeName] -> Type -> TypeScheme
makePoly (name : names) t = Poly name (makePoly names t)
makePoly [] t = Mono t

-----------
-- Tests --
-----------

-- for readability
instance Show Type where
    show (TVar(name)) = show name
    show (TyTerm(str, [])) = str
    show (TyTerm(str, (a1 : (a2 : _)))) = "(" ++ (show a1) ++ " " ++ str ++ " " ++  (show a2) ++ ")"

bool = TyTerm("bool", [])
int = TyTerm("int", [])

a1 = [A "s" (Mono (makeFun int int))]
e1 = Abs "x" (App (Var "s") (Var "x"))
-- exp: ([("t2",int),("t1",int)] , (int -> int))

a2 = []
e2 = Abs "x" (Abs "y" (App (Var "x") (Var "y")))
-- exp: ([("t1",("t2" -> "t3"))] , (("t2" -> "t3") -> ("t2" -> "t3")))

a3 = []
e3 = Let "id" (Abs "x" (Var "x")) (Abs "id" (Var "id"))
-- exp: ([] , ("t2" -> "t2"))

a4 = [A "n" (Mono int), A "f" (Mono (makeFun (TVar("a")) (TVar("b"))))]
e4 = Let "x" (Var "n") (App (Var "f") (Var "x"))
-- exp: ([("b","t1"),("a",int)] , "t1")

a5 = [A "f" (Mono (makeFun (TVar("a")) bool)), A "x" (Mono int)]
e5 = App (Var "f") (Var "x")
-- exp: ([("t1",bool),("a",int)] , bool)

a6 = [A "f" (Mono (makeFun (makeFun (TVar("a")) (TVar("b"))) (TVar("b")))), A "x" (Mono (TVar("c")))]
e6 = App (Var "f") (Var "x")
-- exp: ([("b","t1"),("c",("a" -> "b"))] , "t1")

a7 = [A "x" (Poly "a" (Mono (makeFun (TVar("a")) (TVar("a")))))]
e7 = Var "x"
-- exp: ([] , ("t1" -> "t1"))

a8 = [A "i" (Mono int)]
e8 = Abs "f" (App (Var "f") (Var "i"))
-- exp: ([("t1",(int -> "t2"))] , ((int -> "t2") -> "t2"))

a9 = [A "i" (Mono int)]
e9 = Abs "x" (Let "y" (Var "x") (App (Var "y") (Var "i")))
-- exp: ([("t1",(int -> "t2"))] , ((int -> "t2") -> "t2"))

a10 = []
e10 = Abs "x" (App (Var "x") (Var "x"))
-- exp: recursive reference

tests = [(a1, e1), (a2, e2), (a3, e3), (a4, e4), (a5, e5), (a6, e6), (a7, e7), (a8, e8), (a9, e9), (a10, e10)]
test i = let (a,e) = tests !! (i-1) in infer a e
res i = snd (test i)
