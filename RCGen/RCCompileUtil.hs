{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module RCCompileUtil where

import Data.List
import qualified Data.Map.Strict as Map

import RCData

lukup lut a = case Map.lookup a lut of
                Nothing -> error ("unable to find atom "++show a++" in herbrand universe")
                Just x  -> x

newtype TypeMap = TypeMap { tmData :: [Constdef] }
                deriving Show

-- CK: Evaluation environment representing the current domain knowledge and options used for the transformation steps.
data EvalEnv = EE { eeTypes  :: TypeMap,   -- defined types and objects
                    eePreds  :: [Symbol],  -- names of static predicates
                    eeFluents :: [Symbol],  -- names of dynamic functions, i.e. fluents
                    eeFluentTypes :: Map.Map Symbol Type, -- map from fluent name to the type of the fluent
                    eeLits   :: [Literal], -- literals
                    eeSFuncs :: Map.Map Term Constval, -- static functions
                    eeClosed :: Bool       -- use Closed World? (forces evals to True or False)
                  }
               deriving Show

----------------------------------------------------------------------

data Ternary = TernaryTrue | TernaryFalse | TernaryUndecided
             deriving (Show, Eq)

class LNot a where
   lnot :: a -> a

instance LNot Ternary where
   lnot TernaryTrue      = TernaryFalse
   lnot TernaryFalse     = TernaryTrue
   lnot TernaryUndecided = TernaryUndecided

instance LNot Literal where
   lnot (Literal p a) = Literal (not p) a
   lnot (LiteralF f)  = LiteralF $ case f of
                        FEqual t1 t2        -> FNEqual t1 t2
                        FNEqual t1 t2       -> FEqual t1 t2
                        FGreaterThan t1 t2  -> FLessEqual t1 t2
                        FGreaterEqual t1 t2 -> FLessThan t1 t2
                        FLessThan t1 t2     -> FGreaterEqual t1 t2
                        FLessEqual t1 t2    -> FGreaterThan t1 t2


ternaryAnd []                    = TernaryTrue
ternaryAnd (TernaryTrue:as)      = ternaryAnd as
ternaryAnd (TernaryFalse:_)      = TernaryFalse
ternaryAnd (TernaryUndecided:as) = case ternaryAnd as of
                                     TernaryFalse -> TernaryFalse
                                     _            -> TernaryUndecided

ternaryOr []                    = TernaryFalse
ternaryOr (TernaryTrue:_)       = TernaryTrue
ternaryOr (TernaryFalse:as)     = ternaryOr as
ternaryOr (TernaryUndecided:as) = case ternaryOr as of
                                     TernaryTrue -> TernaryTrue
                                     _           -> TernaryUndecided

----------------------------------------------------------------------

-- CK: find all free variables in the given object
class Freevars a where
    freevars :: a -> [Symbol]
    fvlist :: [a] -> [Symbol]
    fvlist = foldl union [] . map freevars

instance Freevars a => Freevars (Maybe a) where
    freevars Nothing = []
    freevars (Just x) = freevars x

instance Freevars Formula where
    freevars f = case f of
           FAtom _ ts   -> fvlist ts
           FAnd fs      -> fvlist fs
           FOr fs       -> fvlist fs
           FNot f'      -> freevars f'
           FImply a b   -> fvlist [a, b]
           FIff a b     -> fvlist [a, b]
           FForall d f' -> freevars f' \\ declvars d
           FExists d f' -> freevars f' \\ declvars d
           FIsType t _  -> freevars t
           FLitForm f'  -> freevars f'

instance Freevars EFormula where
    freevars f = case f of
           EFAtom _ ts   -> fvlist ts
           EFAssign a b  -> fvlist [a, b]
           EFAnd fs      -> fvlist fs
           EFNot f'      -> freevars f'
           EFWhen a b    -> freevars a `union` freevars b
           EFForall d f' -> freevars f' \\ declvars d

instance Freevars LitFormula where
    freevars f = litfapply (\t1 t2 -> fvlist [t1, t2]) f

instance Freevars Term where
    freevars (Atomic e)      = freevars e
    freevars (Function _ es) = fvlist es
    freevars (FFun c)        = freevars c

instance Freevars FunCall where
    freevars c = funcallapply (\t1 t2 -> fvlist [t1, t2]) c

instance Freevars AtomicExpression where
    freevars (Variable v)   = [v]
    freevars (PrevFunVar v) = [v]
    freevars (Constant _)   = []

instance Freevars Atom where
    freevars (Atom _ ts) = fvlist ts

----------------------------------------------------------------------

-- get all constants in the given object
class Constvars a where
    constvars :: a -> [Constval]
    constlist :: [a] -> [Constval]
    constlist = foldl union [] . map constvars

instance Constvars a => Constvars (Maybe a) where
    constvars Nothing = []
    constvars (Just x) = constvars x

instance Constvars Formula where
    constvars f = case f of
           FAtom _ ts   -> constlist ts
           FAnd fs      -> constlist fs
           FOr fs       -> constlist fs
           FNot f'      -> constvars f'
           FImply a b   -> constlist [a, b]
           FIff a b     -> constlist [a, b]
           FForall _ f' -> constvars f'
           FExists _ f' -> constvars f'
           FLitForm f   -> constvars f
           FIsType t _  -> constvars t

instance Constvars EFormula where
    constvars f = case f of
           EFAtom _ ts   -> constlist ts
           EFAssign a b  -> constlist [a, b]
           EFAnd fs      -> constlist fs
           EFNot f'      -> constvars f'
           EFWhen a b    -> constvars a ++ constvars b
           EFForall _ f' -> constvars f'

instance Constvars LitFormula where
    constvars f = litfapply (\t1 t2 -> constlist [t1, t2]) f

instance Constvars Term where
    constvars (Atomic e)      = constvars e
    constvars (Function _ es) = constlist es
    constvars (FFun c)        = constvars c

instance Constvars FunCall where
    constvars c = funcallapply (\t1 t2 -> constlist [t1, t2]) c

instance Constvars AtomicExpression where
    constvars (Variable _)   = []
    constvars (PrevFunVar _) = []
    constvars (Constant c)   = [c]

instance Constvars Atom where
    constvars (Atom _ ts) = constlist ts

----------------------------------------------------------------------

-- CK: replace all variables in the object with fixed values based on a given mapping from variable symbols to constant values. Variables not mentioned in the map stay unaltered.
--     The mapping consists of 'vars', the list of variable symbols to replace, and 'vals', their corresponding values.
class Instantiate a where
    -- instantiate only variables, no prevFunVars are allowed and an error is thrown
    instantiate :: [(Symbol, Constval)] -> a -> a
    -- instantiate variables and prevFunVariables
    -- default implementation is equal to instantiate, ignoring all prevFunVars
    instantiatepff :: [(Symbol, Constval, Constval)] -> a -> a
    instantiatepff env f = instantiate (map (\(s,a,_) -> (s,a)) env) f

instance Instantiate AtomicExpression where
    instantiate env e = case e of
            e@(Constant{}) -> e
            e@(PrevFunVar{}) -> error $ "Cannot have previous-instance variable " ++ show e ++ " in any formula except :non-repeating"
            e@(Variable v) -> case lookup v env of
                                Just c  -> Constant c
                                Nothing -> e
    instantiatepff env e = case e of
            e@(Constant{}) -> e
            e@(PrevFunVar v) -> case lookup v (map (\(s,_,b) -> (s,b)) env) of
                                 Just c  -> Constant c
                                 Nothing -> e
            e@(Variable v) -> case lookup v (map (\(s,a,_) -> (s,a)) env) of
                                Just c  -> Constant c
                                Nothing -> e

instance Instantiate Term where
    instantiate env (Atomic a) = Atomic $ instantiate env a
    instantiate env (Function f as) = Function f $ map (instantiate env) as
    instantiate env (FFun c)   = FFun $ instantiate env c
    instantiatepff env (Atomic a) = Atomic $ instantiatepff env a
    instantiatepff env (Function f as) = Function f $ map (instantiatepff env) as
    instantiatepff env (FFun c)   = FFun $ instantiatepff env c

instance Instantiate FunCall where
    instantiate env c = funcallmap (instantiate env) c
    instantiatepff env c = funcallmap (instantiatepff env) c

instance Instantiate Formula where
    instantiate env f = case f of
            FAtom a ts   -> FAtom a (map (instantiate env) ts)
            FAnd fs      -> FAnd (map (instantiate env) fs)
            FOr  fs      -> FOr (map (instantiate env) fs)
            FNot fs      -> FNot (instantiate env fs)
            FImply a b   -> FImply (instantiate env a) (instantiate env b)
            FIff a b     -> FIff (instantiate env a) (instantiate env b)
            FForall d f' -> FForall d (subinst d env f')
            FExists d f' -> FExists d (subinst d env f')
            -- we do not want to have constants in FIsType, so pattern-match only variable
            -- this also suppresses an overlapping pattern warning with the _
            FIsType t ty -> FIsType (instantiate env t) ty
            FLitForm f   -> FLitForm (instantiate env f)
        where
            subinst d e f = let env' = [va | va@(var,_) <- e, not $ var `elem` (declvars d)]
                            in instantiate env' f
    instantiatepff env f = case f of
            FAtom a ts   -> FAtom a (map (instantiatepff env) ts)
            FAnd fs      -> FAnd (map (instantiatepff env) fs)
            FOr  fs      -> FOr (map (instantiatepff env) fs)
            FNot fs      -> FNot (instantiatepff env fs)
            FImply a b   -> FImply (instantiatepff env a) (instantiatepff env b)
            FIff a b     -> FIff (instantiatepff env a) (instantiatepff env b)
            FForall d f' -> FForall d (subinst d env f')
            FExists d f' -> FExists d (subinst d env f')
            -- we do not want to have constants in FIsType, so pattern-match only variable
            -- this also suppresses an overlapping pattern warning with the _
            FIsType t ty -> FIsType (instantiatepff env t) ty
            FLitForm f   -> FLitForm (instantiatepff env f)
        where
            subinst d e f = let env' = [va | va@(var,_,_) <- e, not $ var `elem` (declvars d)]
                            in instantiatepff env' f

instance Instantiate EFormula where
    instantiate env f = case f of
            EFAtom a ts   -> EFAtom a (map (instantiate env) ts)
            EFAssign a b  -> EFAssign (instantiate env a) (instantiate env b)
            EFAnd fs      -> EFAnd (map (instantiate env) fs)
            EFNot fs      -> EFNot (instantiate env fs)
            EFWhen a b    -> EFWhen (instantiate env a) (instantiate env b)
            EFForall d f' -> EFForall d (subinst d env f')
        where
            subinst d e f = let env' = [va | va@(var,_) <- e, not $ var `elem` (declvars d)]
                            in instantiate env' f

instance Instantiate LitFormula where
    instantiate env = litfmap (instantiate env)
    instantiatepff env = litfmap (instantiatepff env)

instance Instantiate Literal where
    instantiate env (Literal b a) = Literal b (instantiate env a)
    instantiate env (LiteralF f)  = LiteralF (instantiate env f)
    instantiatepff env (Literal b a) = Literal b (instantiatepff env a)
    instantiatepff env (LiteralF f)  = LiteralF (instantiatepff env f)

instance Instantiate Atom where
    instantiate env (Atom s ts) = Atom s (map (instantiate env) ts)
    instantiatepff env (Atom s ts) = Atom s (map (instantiatepff env) ts)

instance Instantiate Effect where
    instantiate env (EPredicate l)    = EPredicate $ instantiate env l
    instantiate env (EAssignment f v) = EAssignment (instantiate env f) (instantiate env v)

instance Instantiate CEffect where
   instantiate env (Unconditional e)  = Unconditional $ instantiate env e
   instantiate env (Conditional c es) = Conditional (instantiate env c) (map (instantiate env) es)

instance Instantiate DNF where
    instantiate env = MkDNF . (instantiate env) . ors
    instantiatepff env = MkDNF . (instantiatepff env) . ors

instance Instantiate RawDNF where
    instantiate env dnf = map (map (instantiate env)) dnf
    instantiatepff env dnf = map (map (instantiatepff env)) dnf

----------------------------------------------------------------------
-- handle static and dynamic function calls

class ZapFunc a where
    -- removes all occurrences of static/dynamic functions in a.
    zapFunc :: EvalEnv -> a -> a
    zapFunc ee = zapFuncFl ee False
    -- like zapFunc, but if the second argument is true, giving a hint to not raise an error when fluents are encountered
    -- (this is required for comparison in preconditions, e.g. (= (fl-1) (fl-2)))
    zapFuncFl :: EvalEnv -> Bool -> a -> a
    zapFuncFl ee _ = zapFunc ee

instance ZapFunc CEffect where
    zapFunc ee (Unconditional e) = Unconditional $ zapFunc ee e
    zapFunc ee (Conditional c es) = Conditional c (map (zapFunc ee) es)

instance ZapFunc Effect where
    zapFunc ee (EPredicate l) = EPredicate $ zapFunc ee l
    zapFunc ee (EAssignment f v) = EAssignment (zapFuncFl ee True f) (zapFuncFl ee True v)

instance ZapFunc Literal where
    zapFunc ee (Literal p a) = Literal p (zapFunc ee a)
    zapFunc ee (LiteralF f) = LiteralF (zapFuncFl ee True f)

instance ZapFunc LitFormula where
    zapFuncFl ee ignore f = litfmap (zapFuncFl ee ignore) f

instance ZapFunc Atom where
    zapFunc ee (Atom s ts) = Atom s (map (zapFunc ee) ts)

instance ZapFunc Term where
    zapFuncFl ee ignore t =
          case t of
            Atomic {}   -> t
            Function s ts -> let t' = Function s (map (zapFuncFl ee False) ts) -- use False here because nested fluents are not allowed, only nested static function calls
                             in case Map.lookup t' (eeSFuncs ee) of
                             Just c -> Atomic(Constant c)
                             Nothing -> if s `elem` eeFluents ee
                                -- if we want to evaluate fluents, we just return t here
                                -- we then must implement dynamic predicate lookups (currently, all predicates are known in grounded actions)
                                then if ignore then t' else error $ "Fluents cannot be evaluated yet: " ++ show t'
                                else error $ "no mapping for non-fluent function call "++show t'
            FFun c      -> FFun $ zapFuncFl ee ignore c

instance ZapFunc FunCall where
    zapFuncFl ee ignore c = funcallmap (zapFuncFl ee ignore) c

----------------------------------------------------------------------

-- splits a timed formula into those with AtBegin, AtEnd and OverAll tag, respectively
class SplitTimedFormula tformula formula where
    splitTimedFormula :: tformula -> ([formula],[formula],[formula])

instance SplitTimedFormula tformula formula => SplitTimedFormula (Maybe tformula) formula where
    splitTimedFormula Nothing = ([],[],[])
    splitTimedFormula (Just f)  = splitTimedFormula f

instance SplitTimedFormula TimedFormula Formula where
    splitTimedFormula (TimedF AtStart f) = ([f],[],[])
    splitTimedFormula (TimedF AtEnd f)   = ([],[f],[])
    splitTimedFormula (TimedF OverAll f) = ([],[],[f])
    splitTimedFormula (TFAnd fs)         = let (s,e,o)=unzip3 $ map splitTimedFormula fs in (concat s,concat e,concat o)

instance SplitTimedFormula TimedEFormula EFormula where
    splitTimedFormula (TimedEF AtStart f) = ([f],[],[])
    splitTimedFormula (TimedEF AtEnd f)   = ([],[f],[])
    splitTimedFormula (TimedEF OverAll f) = ([],[],[f])
    splitTimedFormula (TEFAnd fs)         = let (s,e,o)=unzip3 $ map splitTimedFormula fs in (concat s,concat e,concat o)

----------------------------------------------------------------------

-- Classes that support evaluation to a ternary value
class Eval a where
      eval :: EvalEnv -> a -> Ternary -- ^ The eval function

-- Note: the current strategy for equality tests is to allow as much comparisons as possible, and simply return "False" for comparisons of incompatible types
-- E.g. comparing a number-fluent to an object may be considered as a type error, but this may be intended and allows to have types with values (a b 1 2)
-- It is an error when using >/</... with objects and object fluents, to reduce the surprise of modelling errors

-- CK: evaluates the value of a literal based on the current environment state (literals known to be true) closed world assumption may be used.
--     the evaluated value is either known to be true, known to be false, or unknown (open world, e.g. based on unknown variables)

instance Eval Literal where
  eval ee l'@(Literal _ _)
    | l `elem` eeLits ee                        = TernaryTrue -- literal in world --> true
    | lnot l `elem` eeLits ee                   = TernaryFalse -- negated literal in world --> false
    -- literal not in world and world closed: -- if literal true -> false; else if true in world --> false else --> true;
    | eeClosed ee                               = if p then TernaryFalse else (if (Literal True a) `elem` eeLits ee then TernaryFalse else TernaryTrue)
    | s `elem` eePreds ee && all isConstTerm ts = if p then TernaryFalse else TernaryTrue
    | True                                      = TernaryUndecided
   where l@(Literal p a@(Atom s ts)) = zapFunc ee l'
         isConstTerm (Atomic e) = isConst e
         isConstTerm (Function _ _) = False
         isConstTerm (FFun _) = False

  eval ee (LiteralF f) = eval ee f

instance Eval LitFormula where
  eval ee f = case f of
        FEqual t1 t2        -> builtinEqual ee [t1, t2]
        FNEqual t1 t2       -> lnot $ builtinEqual ee [t1, t2]
        FGreaterThan t1 t2  -> builtinNumCmp ee TGreaterThan [t1, t2]
        FGreaterEqual t1 t2 -> builtinNumCmp ee TGreaterEqual [t1, t2]
        FLessThan t1 t2     -> builtinNumCmp ee TLessThan [t1, t2]
        FLessEqual t1 t2    -> builtinNumCmp ee TLessEqual [t1, t2]

instance Eval DNF where
  eval ee = ternaryOr . map (ternaryAnd . map (eval ee)) . ors


builtinEqual :: EvalEnv -> [Term] -> Ternary
builtinEqual _ []     = TernaryTrue
builtinEqual _ [_]    = TernaryTrue
-- Consecutively check the first with the next term.
-- The fold function tracks the result and the term to compare next.
-- The equal method returns the result from comparing two terms, and, from both terms, which to keep for comparing:
-- When comparing a constant to a variable, we want to keep next terms with the constant, because it allows giving a more exact result.
builtinEqual ee (t:ts) = fst $ foldl' equal' (TernaryTrue, t) ts
  where
  equal' (r, t1) t2 = let (r', tn) = equal t1 t2 in (ternaryAnd [r', r], tn)
  equal t@(Atomic (Constant a)) (Atomic (Constant b))     = (if a==b then TernaryTrue else TernaryFalse, t)
  equal t@(Atomic (Variable a)) (Atomic (Variable b))     = (if a==b then TernaryTrue else TernaryUndecided, t)
  equal t@(Atomic (PrevFunVar a)) (Atomic (PrevFunVar b)) = (if a==b then TernaryTrue else TernaryUndecided, t)
  equal (Atomic (Variable _)) t                           = (TernaryUndecided, t)
  equal (Atomic (PrevFunVar _)) t                         = (TernaryUndecided, t)
  equal t@(Atomic (Constant _)) (Atomic (Variable _))     = (TernaryUndecided, t)
  equal t@(Atomic (Constant _)) (Atomic (PrevFunVar _))   = (TernaryUndecided, t)
  equal (FFun a) t@(FFun b)                           = (if a==b then TernaryTrue else TernaryUndecided, t) -- we could do some term rewriting here, but it does not seem to be worth the effort
  equal (FFun _) t@(Atomic _)                         = (TernaryUndecided, t)
  equal (FFun c) t@(Function f _)                     = case (eeFluentTypes ee `lukup` f, c) of
                                                (Type_Object _, _) -> error $ "Cannot assign numeric term to object fluent " ++ show t
                                                _                  -> (TernaryUndecided, t) -- all numeric types will be automatically cast to the correct type, incl. rounding
  equal t1 t2@(FFun _)                                = equal t2 t1
  equal (Function f _) t@(Atomic (Constant b))        = case (eeFluentTypes ee `lukup` f, b) of
                                                (Type_Object ft, _)            -> (if b `elem` tvalues ft then TernaryUndecided else TernaryFalse, t)
                                                (_, Symconst _)                -> (TernaryFalse, t) -- numeric fluent and symbol
                                                (Type_Num from to, Intconst i) -> (if i >= from && i <= to then TernaryUndecided else TernaryFalse, t) -- the constant is out of range
                                                (Type_Num _ _, Numconst i)     -> (if (fromIntegral.round) i == i then TernaryUndecided else TernaryFalse, t)   -- int fluent and float
                                                (Type_NativeNum, Numconst i)   -> (if (fromIntegral.round) i == i then TernaryUndecided else TernaryFalse, t)   -- int fluent and float
                                                _                              -> (TernaryUndecided, t) -- we also say undecided for (= int-fluent float-constant), as float-constant may be equal
  equal t@(Function _ _) (Atomic (Variable _))        = (TernaryUndecided, t) -- no information about the variable whatsoever
  equal t@(Function _ _) (Atomic (PrevFunVar _))      = (TernaryUndecided, t) -- no information about the variable whatsoever
  equal t1@(Function f1 _) t2@(Function f2 _)         = case (eeFluentTypes ee `lukup` f1, eeFluentTypes ee `lukup` f2) of
                                                (Type_Object ft1, Type_Object ft2)       -> (if null $ intersect (tvalues ft1) (tvalues ft2) then TernaryFalse else TernaryUndecided, t2)
                                                (Type_Object ft, Type_Num _ _)           -> (if all isIntconst (tvalues ft) then TernaryUndecided else TernaryFalse, t2) -- may be true if object fluent contains numbers
                                                (Type_Object ft, Type_NativeNum)         -> (if all isIntconst (tvalues ft) then TernaryUndecided else TernaryFalse, t2) -- may be true if object fluent contains numbers
                                                (Type_Object ft, Type_Float)             -> (if all isSymconst (tvalues ft) then TernaryFalse else TernaryUndecided, t2) -- may be true if object fluent contains numbers
                                                (_, Type_Object _)                       -> equal t2 t1 -- symmetric
                                                (Type_Num from1 to1, Type_Num from2 to2) -> (if from1 > to2 || from2 > to1 then TernaryFalse else TernaryUndecided, t2)
                                                (Type_Num _ _, _)                        -> (TernaryUndecided, t1)
                                                (_, Type_Num _ _)                        -> (TernaryUndecided, t2)
                                                _                                        -> (TernaryUndecided, t2)
  equal t1 tf@(Function _ _)                          = equal tf t1

  -- returns the list of constants associated with a type
  tvalues :: Symbol -> [Constval]
  tvalues t = case lookup t ((tmData.eeTypes) ee) of
                Just cs -> cs
                Nothing -> error $ "Cannot find type " ++ show t
  isSymconst c = case c of
                Symconst _ -> True
                _          -> False
  isIntconst c = case c of
                Intconst _ -> True
                Numconst f -> (fromIntegral.round) f == f
                _          -> False



-- Check order of terms, (< 1 3 4) is true, (< 1 4 2 3) is false
builtinNumCmp :: EvalEnv -> Token -> [Term] -> Ternary
builtinNumCmp _ _ []   = TernaryTrue
builtinNumCmp _ _ [_]  = TernaryTrue
builtinNumCmp ee op (t:ts) = fst $ foldl' cmp' (TernaryTrue, t) ts
  where
  cmp' (r, t1) t2 = let (r', tn) = cmp op t1 t2 in (ternaryAnd [r', r], tn)
  cmp op (Atomic (Constant a)) t@(Atomic (Constant b)) = case (a, b) of
                                      (Symconst _, _)             -> error $ "comparing non-number " ++ show a
                                      (_, Symconst _)             -> error $ "comparing non-number " ++ show b
                                      (Intconst i1, Intconst i2)  -> (if evalOp op i1 i2 then TernaryTrue else TernaryFalse, t) -- use second term, because list must be "transitive" wrt. op
                                      (Intconst i1, Numconst i2)  -> (if evalOp op (fromIntegral i1) i2 then TernaryTrue else TernaryFalse, t)
                                      (Numconst i1, Intconst i2)  -> (if evalOp op i1 (fromIntegral i2) then TernaryTrue else TernaryFalse, t)
                                      (Numconst i1, Numconst i2)  -> (if evalOp op i1 i2 then TernaryTrue else TernaryFalse, t)
  cmp op t@(Atomic (Variable a)) (Atomic (Variable b)) | a == b        = (if op `elem` [TGreaterThan, TLessThan] then TernaryFalse else TernaryUndecided, t)
                                                       | otherwise     = (TernaryUndecided, t)
  cmp op t@(Atomic (PrevFunVar a)) (Atomic (PrevFunVar b)) | a == b    = (if op `elem` [TGreaterThan, TLessThan] then TernaryFalse else TernaryUndecided, t)
                                                           | otherwise = (TernaryUndecided, t)
  cmp _ (Atomic (Variable _)) t@(Atomic (Constant b))        = case b of
                                      Symconst _ -> error $ "comparing non-number " ++ show b
                                      _          -> (TernaryUndecided, t)
  cmp _ (Atomic (PrevFunVar _)) t@(Atomic (Constant b))      = case b of
                                      Symconst _ -> error $ "comparing non-number " ++ show b
                                      _          -> (TernaryUndecided, t)
  cmp op t1@(Atomic (Constant _)) t2@(Atomic (Variable _))   = cmp (invertOp op) t2 t1
  cmp op t1@(Atomic (Constant _)) t2@(Atomic (PrevFunVar _)) = cmp (invertOp op) t2 t1
  cmp _ (Atomic (Variable _)) (Atomic (PrevFunVar _))        = (TernaryUndecided, t)
  cmp _ (Atomic (PrevFunVar _)) (Atomic (Variable _))        = (TernaryUndecided, t)

  cmp _ (FFun _) t                                     = (TernaryUndecided, t) -- we maybe want some term rewriting here, but it does not seem to be worth the effort
  cmp _ t (FFun _)                                     = (TernaryUndecided, t)
  cmp op (Function f _) t@(Atomic (Constant b))        = case (eeFluentTypes ee `lukup` f, b) of
                                      (Type_Object _, _)             -> error $ "comparing non-number object fluent " ++ show f
                                      (_, Symconst _)                -> error $ "comparing non-number " ++ show b
                                      (Type_Num from to, Intconst i) -> case (evalOp op from i, evalOp op to i) of
                                                      (True, True)    -> (TernaryTrue, t) -- every fluent value satisfies the condition
                                                      (False, False)  -> (TernaryFalse, t)
                                                      _               -> (TernaryUndecided, t) -- some values satisfy, others not -> undecided
                                      (Type_Num from to, Numconst i) -> case (evalOp op (fromIntegral from) i, evalOp op (fromIntegral to) i) of
                                                      (True, True)    -> (TernaryTrue, t) -- every fluent value satisfies the condition
                                                      (False, False)  -> (TernaryFalse, t)
                                                      _               -> (TernaryUndecided, t) -- some values satisfy, others not -> undecided
                                      _                            -> (TernaryUndecided, t)
  cmp _ t@(Function f _) (Atomic (Variable _))         = checkFluentT f (TernaryUndecided, t) -- pass the fluent, as it may contain more information than a variable
  cmp _ t@(Function f _) (Atomic (PrevFunVar _))       = checkFluentT f (TernaryUndecided, t) -- pass the fluent, as it may contain more information than a variable
  cmp op t1@(Function f1 _) t2@(Function f2 _)         = case (eeFluentTypes ee `lukup` f1, eeFluentTypes ee `lukup` f2) of
                                      (Type_Object _, _)        -> error $ "comparing non-number object fluent " ++ show f1
                                      (_, Type_Object _)        -> error $ "comparing non-number object fluent " ++ show f2
                                      (Type_Num from1 to1, Type_Num from2 to2) -> let ltleft = evalOp op to1 from2
                                                                                      gtright  = evalOp op from1 to2
                                                                                      checkBounds = if op == TLessThan || op == TLessEqual then (ltleft, gtright) else (gtright, ltleft)
                                                                                  in case checkBounds of
                                                                                         (True, _)   -> (TernaryTrue, t2) -- f1 is always </<= f2
                                                                                         (_, False)  -> (TernaryFalse, t2) -- f1 is always >/>= f2
                                                                                         _           -> (TernaryUndecided, t2)
                                      (Type_Num _ _, _)         -> (TernaryUndecided, t1)
                                      (_, Type_Num _ _)         -> (TernaryUndecided, t2)
                                      _                               -> (TernaryUndecided, t2)
  cmp op t1 t2@(Function _ _)                          = cmp (invertOp op) t2 t1

  evalOp :: Ord a => Token -> a -> a -> Bool
  evalOp op a b | op == TGreaterThan  = a > b
                | op == TGreaterEqual = a >= b
                | op == TLessThan     = a < b
                | op == TLessEqual    = a <= b
                | otherwise           = error $ "Operator " ++ show op ++ " not supported"

  invertOp op | op == TGreaterThan  = TLessThan
              | op == TGreaterEqual = TLessEqual
              | op == TLessThan     = TGreaterThan
              | op == TLessEqual    = TGreaterEqual
              | otherwise           = error $ "Operator " ++ show op ++ " not supported"

  -- checks if the fluent's type is numeric
  -- throws an error if the types are not comparable
  -- returns the result otherwise
  checkFluentT :: Symbol -> a -> a
  checkFluentT f r = case eeFluentTypes ee `lukup` f of
              Type_Object _ -> error $ "comparing non-number object fluent " ++ show f
              _             -> r
