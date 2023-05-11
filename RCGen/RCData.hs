{-# LANGUAGE
    FlexibleContexts,
    TypeSynonymInstances #-}

module RCData where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Char
import qualified Data.List as List

data Token = TEOF
           | TSym Symbol
           | TNum Double
           | TInt Int
           | TOpen
           | TClose
           | TPrevVar
           | TVar
           -----------------
           | THyphen
           | TComma
           ----------------
           | TDefine
           | TDomain
           | TProblem
           -----------------
           | TTypes
           | TConstants
           | TPredicates
           | TFunctions
           | TAction
           | TDurativeAction
           | TObservation
           | TSaliency
           | TAgent
           | TDuration
           | TParams
           | TPrecond
           | TEffect
           | TDDomain
           | TObjects
           | TInit
           | TInits
           | TGoal
           | TGoals
           | TLandmarks
           | TIrrational
           | TNonRepeating
           -----------------
           | TAtStart
           | TAtEnd
           | TOverAll
           -----------------
           | TIsType
           -----------------
           | TAnd
           | TOr
           | TNot
           | TIff
           | TImply
           | TForall
           | TExists
           | TWhen
           | TEquals
           | TGreaterThan | TGreaterEqual
           | TLessThan | TLessEqual
           | TPlus -- TMinus is THyphen
           | TMult | TDiv
           | TAssign
           | TIncrease | TDecrease
           | TScale
           deriving (Eq)

predefined = [("-",THyphen)
             ,(",",TComma)
             ----------------------------
             ,("define",TDefine)
             ,("domain",TDomain)
             ,("problem",TProblem)
             ----------------------------
             ,(":types",TTypes)
             ,(":constants",TConstants)
             ,(":predicates",TPredicates)
             ,(":functions",TFunctions)
             ,(":action",TAction)
             ,(":durative-action",TDurativeAction)
             ,(":observation",TObservation)
             ,(":callbacks",TObservation)
             ,(":saliency",TSaliency)
             ,(":agent",TAgent)
             ,(":duration",TDuration)
             ,(":parameters",TParams)
             ,(":params",TParams)
             ,(":precondition",TPrecond)
             ,(":condition",TPrecond)
             ,(":precond",TPrecond)
             ,(":effect",TEffect)
             ,(":domain",TDDomain)
             ,(":objects",TObjects)
             ,(":init",TInit)
             ,(":inits",TInits)
             ,(":goal",TGoal)
             ,(":goals",TGoals)
             ,(":landmarks",TLandmarks)
             ,(":irrational",TIrrational)
             ,(":non-repeating",TNonRepeating)
             ----------------------------
             ,("at-start",TAtStart)
             ,("at-end",TAtEnd)
             ,("over-all",TOverAll)
             ----------------------------
             ,(":is-type",TIsType)
             ----------------------------
             ,("and",TAnd)
             ,("or",TOr)
             ,("not",TNot)
             ,("iff",TIff)
             ,("imply",TImply)
             ,("forall",TForall)
             ,("exists",TExists)
             ,("when",TWhen)
             ,("=",TEquals)
             ,(">",TGreaterThan) ,(">=",TGreaterEqual)
             ,("<",TLessThan) ,("<=",TLessEqual)
             ,("+",TPlus)
             ,("*",TMult), ("/",TDiv)
             ,("assign",TAssign)
             ,("increase",TIncrease) ,("decrease",TDecrease)
             ,("scale",TScale)
             ]
tnames = [(b,a) | (a,b) <- predefined]

instance Show Token where
    showsPrec _ t = case lookup t tnames of
                         Just n -> (n++)
                         Nothing -> case t of
                                         TSym s -> (name s ++)
                                         TNum v -> shows v
                                         TInt v -> shows v
                                         TOpen  -> ('(':)
                                         TClose -> (')':)
                                         TVar   -> ('?':)
                                         TPrevVar -> ("?'"++)
                                         TEOF   -> ("{EOF}"++)
                                         _      -> error "Bad Token"

listShow sl = ('(':) . showsRawList shows sl . (')':)

data Symbol = Sym {index :: !Int, token :: !Token, name :: !String}

instance Eq Symbol where
         s1 == s2 = if index s1 >= 0 || index s2 >= 0 then index s1 == index s2 else name s1 == name s2
instance Ord Symbol where
         compare s1 s2 = if index s1 >= 0 || index s2 >= 0 then compare (index s1) (index s2) else compare (name s1) (name s2)

instance Show Symbol where
         showsPrec _ s = (name s++)
         showList = listShow

type Symtab = Map.Map String Symbol

data RCParserState = RCParserState { symidx :: !Int,
                                     symtab :: !Symtab,
                                     input :: String,
                                     file :: String,
                                     line :: !Int,
                                     stoken :: !Token,
                                     dpred :: [Symbol],
                                     preds :: [Symbol] }

nullSymbol   = Sym {token=(TSym nullSymbol), name = "NULL", index=0}
objectSymbol = Sym {token=(TSym objectSymbol), name = "object", index=1}
numberTypeSymbol = Sym {token=(TSym numberTypeSymbol), name="number", index=2}  -- signalling numeric fluents when used as the fluent type
floatTypeSymbol = Sym {token=(TSym floatTypeSymbol), name="float", index=3}  -- signalling numeric fluents when used as the fluent type
allSymbol    = Sym {token=(TSym allSymbol), name=":all", index=4}  -- signalling common definitions for initial state or goal state formula

predefined_sym = [objectSymbol,numberTypeSymbol,floatTypeSymbol,allSymbol]

initialSymtab = let speci   = zip predefined [(length predefined_sym + 1)..] -- index of predefined tokens are after predefined symbols
                    specs   = [s | ((n,t),i) <- speci, let s = Sym {token=t, name=n, index=i}]
                in Map.fromList [(name s, s) | s <- predefined_sym++specs]

initialState = RCParserState {symidx=Map.size initialSymtab + 1,
                              symtab=initialSymtab,
                              input=undefined,
                              file=undefined,
                              line=1,
                              stoken=TEOF,
                              dpred=[],
                              preds=[]}

pushPred s ts = do state <- get
                   put state{preds=List.union [s] (preds state)}
                   return (FAtom s (reverse ts))

pushDPred s ts = do state <- get
                    put state{preds=List.union [s] (preds state), dpred=List.union [s] (dpred state)}
                    return (EFAtom s (reverse ts))

-- type RCParserMonad = StateT RCParserState IO

type RCParserMonad = State RCParserState

--statef :: MyMonad ()

--enterSym :: String -> MyMonad Symbol
enterSym symname = do state <- get
                      let st = symtab state
                      case Map.lookup symname st of
                           Just s  -> return s
                           Nothing -> do let ix  = symidx state + 1
                                             sym = Sym {token=(TSym sym),name=symname,index=ix}
                                             st' = Map.insert symname sym st
                                         put $! state {symidx=ix,symtab=st'}
                                         return sym

dumpSyms :: RCParserMonad ShowS
dumpSyms = do state <- get
              return $ Map.foldr showsym id (symtab state)
       where showsym s m =  (name s++) . (' ':) . shows (index s) . ('\n':) . m

withSym symname f = enterSym symname >>= (return . f)


----------------------------------------------------------------------

errorMessage state = (("Parse error in file "++(file state)++", line ")++)
                     . shows (line state)
                     . (": unexpected token "++)
                     . shows (stoken state)
                     $ "\n"

happyError :: RCParserMonad a
happyError = do state <- get
                error (errorMessage state)

lexerM :: (Token -> RCParserMonad a) -> RCParserMonad a
lexerM cont = do state <- get
                 let (etk,rest,ln,fn) = lexer (input state) (line state) (file state)
                     cont1 tok     = do state' <- get
                                        put state' {input=rest,line=ln,file=fn,stoken=tok}
                                        cont tok
                 case etk of
                      Left tk -> cont1 tk
                      Right n -> do sym <- enterSym n
                                    cont1 (token sym)

isConstituent c = not (c `elem` "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM ;()?")

skipToEOL []         = []
skipToEOL s@('\n':_) = s
skipToEOL (_:r)      = skipToEOL r

lexer :: String -> Int -> String -> (Either Token String, String, Int, String)
lexer [] l f                      = (Left TEOF,[],l,f)
lexer ('\n':';':'#':' ':r) _ _    = let (lineno', ' ':'"':r1) = span isDigit r
                                        lineno = read lineno'
                                        (fname, '"':' ':r2)   = span (/= '"') r1
                                        (flag', '\n':rest)    = span isDigit r2
                                        _flag = read flag' :: Int
                                    in lexer rest lineno fname
lexer ('\n':r) l f                = lexer r (l+1) f
lexer ('(':r) l f                 = (Left TOpen,r,l,f)
lexer (')':r) l f                 = (Left TClose,r,l,f)
lexer (';':r) l f                 = lexer (skipToEOL r) l f
lexer ('?':'?':r) l f             = (Left TPrevVar,r,l,f)
lexer ('?':r) l f                 = (Left TVar,r,l,f)
lexer (c:r) l f | isSpace c       = lexer r l f
                | isConstituent c = let (name,r') = span isConstituent r
                                        fullname  = c:name
                                    in case fullname of
                                       "-"  -> (Left THyphen,r',l,f)
                                       _    -> case reads fullname :: [(Int,String)] of
                                                 [(v,"")] -> (Left (TInt v),r',l,f)
                                                 _        -> case reads fullname :: [(Double,String)] of
                                                               [(v,"")] -> (Left (TNum v),r',l,f)
                                                               _        -> (Right (c:name),r',l,f)
                | True            = error ("Bad input: "++(c:r))


----------------------------------------------------------------------
-- Various helper functions for showing things (could need some cleaning up ...)

showsTaggedList tag []   = ('(':) . tag . (')':)
showsTaggedList tag list = ('(':) . tag . foldr (\elt rest -> (' ':) . shows elt . rest) (')':) list

showsRawList _ [] = id
showsRawList how (e:l) =  how e . foldr (\elt rest -> (' ':) . how elt . rest) id l

sfoldr how rest = foldr (\e r -> how e . r) rest
tagfold _ _ []     = id
tagfold tag how l  = crnl 0 . (('(':tag)++) . sfoldr how (")"++) l

lshow how =  foldr (\e r -> (' ':) . how e . r) (')':)
tlshow tag how l = ('(':) . (tag++) . lshow how l

crnl k = ((take (k+1) str)++)
     where str = '\n':repeat '\t'

----------------------------------------------------------------------
-- Terms, Formulae, and Normal Forms

data Constval = Symconst Symbol
              | Numconst Double
              | Intconst Int
              deriving (Ord,Eq)

instance Show Constval where
         showsPrec _ (Symconst s) = shows s
         showsPrec _ (Numconst d) = shows d
         showsPrec _ (Intconst d) = shows d
         showList = listShow

data AtomicExpression = Variable { theVar :: Symbol }
                      | PrevFunVar { thePrevVar :: Symbol }
                      | Constant { theConst :: Constval }
          deriving (Ord, Eq)

instance Show AtomicExpression where
         showsPrec _ (Variable s) = ('?':) . shows s
         showsPrec _ (PrevFunVar s) = ("??"++) . shows s
         showsPrec _ (Constant s) = shows s
isConst (Constant {}) = True
isConst _             = False
isVar = not . isConst

data FunCall = FPlus Term Term
             | FMinus Term Term
             | FMult Term Term
             | FDiv Term Term
             deriving (Ord, Eq)

-- transforms the terms of a function call, replacing both by applying the function
-- Atomic and Function terms are not covered and must be handled separately
funcallmap :: (Term -> Term) -> FunCall -> FunCall
funcallmap f (FPlus t1 t2)  = FPlus (f t1) (f t2)
funcallmap f (FMinus t1 t2) = FMinus (f t1) (f t2)
funcallmap f (FMult t1 t2)  = FMult (f t1) (f t2)
funcallmap f (FDiv t1 t2)   = FDiv (f t1) (f t2)

-- applies the terms of a function call to a function
-- Atomic and Function terms are not covered and must be handled separately
funcallapply :: (Term -> Term -> a) -> FunCall -> a
funcallapply f (FPlus t1 t2)  = f t1 t2
funcallapply f (FMinus t1 t2) = f t1 t2
funcallapply f (FMult t1 t2)  = f t1 t2
funcallapply f (FDiv t1 t2)   = f t1 t2

instance Show FunCall
   where showsPrec _ (FPlus t1 t2)  = showsTaggedList (shows TPlus) [t1, t2]
         showsPrec _ (FMinus t1 t2) = showsTaggedList (shows THyphen) [t1, t2]
         showsPrec _ (FMult t1 t2)  = showsTaggedList (shows TMult) [t1, t2]
         showsPrec _ (FDiv t1 t2)   = showsTaggedList (shows TDiv) [t1, t2]

data Term = Atomic {theAtom::AtomicExpression}
          | Function Symbol [Term] -- Fluents or constant function, e.g. (id-of alice). parameter [Term] are only allowed to be static function, not nested fluents
          | FFun FunCall
          deriving (Ord, Eq)

instance Show Term
   where showsPrec p (Atomic e)     = showsPrec p e
         showsPrec _ (Function s l) = showsTaggedList (shows s) l
         showsPrec p (FFun c)       = showsPrec p c

-- formulae in preconditions, conditional effects, ...
data Formula = FAtom Symbol [Term]
             | FAnd [Formula]
             | FOr [Formula]
             | FNot Formula
             | FImply Formula Formula
             | FIff Formula Formula
             | FForall VDecls Formula
             | FExists VDecls Formula
             | FIsType Term Type -- FIsType Variable Type
             | FLitForm LitFormula

-- formulae of the :effect clause
-- EFormulas are normalized to CEffect and Effect (Literal / Assignment)
data EFormula = EFAtom Symbol [Term]
              | EFNot EFormula -- only valid for EFAtom
              | EFAssign Term Term
              | EFAnd [EFormula]
              | EFWhen Formula EFormula
              | EFForall VDecls EFormula

instance Show Formula where
         showsPrec _ f = case f of
                              (FAtom a [])  -> shows a
                              (FAtom a l)   -> showsTaggedList (shows a) l
                              (FAnd l)      -> showsTaggedList ("and"++) l
                              (FOr l)       -> showsTaggedList ("or"++) l
                              (FNot f)      -> showsTaggedList ("not"++) [f]
                              (FImply a b)  -> showsTaggedList ("imply"++) [a,b]
                              (FIff a b)    -> showsTaggedList ("iff"++) [a,b]
                              (FForall d f) -> ("(forall ("++) . (shows d) . (") "++) . shows f . (')':)
                              (FExists d f) -> ("(exists ("++) . (shows d) . (") "++) . shows f . (')':)
                              (FIsType v t) -> ("(:is-type "++) . shows v . (' ':) . shows t . (')':)
                              (FLitForm f)  -> shows f

instance Show EFormula where
         showsPrec _ f = case f of
                              (EFAtom a [])  -> shows a
                              (EFAtom a l)   -> showsTaggedList (shows a) l
                              (EFAssign a b) -> showsTaggedList ("assign"++) [a,b]
                              (EFAnd l)      -> showsTaggedList ("and"++) l
                              (EFNot f)      -> showsTaggedList ("not"++) [f]
                              (EFWhen a b)   -> ("(when ("++) . (shows a) . (") "++) . shows b . (')':)
                              (EFForall d f) -> ("(forall ("++) . (shows d) . (") "++) . shows f . (')':)

-- Literal Formulas are those formulas that may appear as a Literal, i.e. cannot be resolved during compilation
-- this includes equalities of terms, esp. fluents which must be checked at run-time
data LitFormula = FEqual Term Term
                | FNEqual Term Term
                | FGreaterThan Term Term
                | FGreaterEqual Term Term
                | FLessThan Term Term
                | FLessEqual Term Term
                deriving (Ord, Eq)

instance Show LitFormula where
         showsPrec _ f = case f of
                              (FEqual t1 t2)   -> showsTaggedList ("="++) [t1,t2]
                              (FNEqual t1 t2)  -> ("(not "++) . showsTaggedList ("="++) [t1,t2] . (")"++)
                              (FGreaterThan t1 t2)   -> showsTaggedList (">"++) [t1,t2]
                              (FGreaterEqual t1 t2)   -> showsTaggedList (">="++) [t1,t2]
                              (FLessThan t1 t2)   -> showsTaggedList ("<"++) [t1,t2]
                              (FLessEqual t1 t2)   -> showsTaggedList ("<="++) [t1,t2]

-- transforms the terms of a LitFormula, replacing both by applying the function
litfmap :: (Term -> Term) -> LitFormula -> LitFormula
litfmap g f = case f of
                FEqual t1 t2        -> FEqual (g t1) (g t2)
                FNEqual t1 t2       -> FNEqual (g t1) (g t2)
                FGreaterThan t1 t2  -> FGreaterThan (g t1) (g t2)
                FGreaterEqual t1 t2 -> FGreaterEqual (g t1) (g t2)
                FLessThan t1 t2     -> FLessThan (g t1) (g t2)
                FLessEqual t1 t2    -> FLessEqual (g t1) (g t2)

-- applies the terms of a LitFormula to a function
litfapply :: (Term -> Term -> a) -> LitFormula -> a
litfapply g f = case f of
                FEqual t1 t2        -> g t1 t2
                FNEqual t1 t2       -> g t1 t2
                FGreaterThan t1 t2  -> g t1 t2
                FGreaterEqual t1 t2 -> g t1 t2
                FLessThan t1 t2     -> g t1 t2
                FLessEqual t1 t2    -> g t1 t2

data TimeSpecifier = AtStart | AtEnd | OverAll

instance Show TimeSpecifier where
         showsPrec _ f = case f of
                              AtStart -> ("at start"++)
                              AtEnd   -> ("at end"++)
                              OverAll -> ("over all"++)

data TimedFormula = TFAnd [TimedFormula]
                  | TimedF TimeSpecifier Formula

instance Show TimedFormula where
         showsPrec _ f = case f of
                              (TFAnd l)  -> showsTaggedList ("and"++) l
                              (TimedF t l)   -> showsTaggedList (shows t) [l]

data TimedEFormula = TEFAnd  [TimedEFormula]
                   | TimedEF TimeSpecifier EFormula

instance Show TimedEFormula where
         showsPrec _ f = case f of
                              (TEFAnd l)  -> showsTaggedList ("and"++) l
                              (TimedEF t l)   -> showsTaggedList (shows t) [l]

-- we are currently abusing 'Atom' for representing both
-- atomic propositions and function calls.
-- one day this may be a cause for confusion ...
data Atom = Atom Symbol [Term]
          deriving (Ord, Eq)

instance Show Atom where
         showsPrec _ (Atom s []) = shows s
         showsPrec _ (Atom s l)  = tlshow (show s) shows l
         showList = listShow

data Literal = Literal Bool Atom -- the state predicate must hold (or must not hold if False), e.g. (position alice outside)
             | LiteralF LitFormula -- the formula (esp. of fluents) must hold in the current state, e.g. (= (hands-available alice) 2)
             deriving (Ord, Eq)

instance Show Literal where
         showsPrec p (Literal True a) = showsPrec p a
         showsPrec _ (Literal False a) = tlshow "not" shows [a]
         showsPrec _ (LiteralF f) = shows f
         showList = listShow

newtype DNF = MkDNF { ors::[[Literal]] }
            deriving (Ord, Eq)

class WithLiterals a where
      literals :: a -> [Literal]

instance WithLiterals DNF where
         literals dnf = [ l | as <- ors dnf, l <- as ]

instance Show DNF where
         showsPrec _ (MkDNF []) = ("FALSE"++)
         showsPrec _ (MkDNF [[]]) = ("TRUE"++)
         showsPrec _ (MkDNF [as]) = (tlshow "and" shows) as
         showsPrec _ (MkDNF l)  = tlshow "or" (tlshow "and" shows) l

type RawDNF = [[Literal]]


trueDNF = MkDNF [[]]
isTrue (MkDNF [[]]) = True
isTrue _ = False

falseDNF = MkDNF []
isFalse (MkDNF []) = True
isFalse _ = False

newtype ENF = MkENF { effs::[CEffect] }

instance WithLiterals ENF where
  literals (MkENF es) = lits es
    where
    lits es = concatMap getl es
    getl (Unconditional e) = filtlit e
    getl (Conditional _ es) = lits es
    filtlit :: Effect -> [Literal]
    filtlit (EPredicate l) = [l]
    filtlit _ = []

-- collect all instantiated fluents assigned in the ENF
assignments :: ENF -> [Term]
assignments (MkENF es) = ass es
    where
    ass es = concatMap geta es
    geta (Unconditional e) = filtass e
    geta (Conditional _ es) = ass es
    filtass :: Effect -> [Term]
    filtass (EAssignment f _) = [f]
    filtass _ = []

instance Show ENF where
  showsPrec _ (MkENF []) = ("()"++)
  showsPrec _ (MkENF [e]) = shows e
  showsPrec _ (MkENF es)  = ("(and "++) . shows es . (')':)

instance Eq ENF where
 a == b = effs a == effs b

noEffect = MkENF []
doesSomething (MkENF []) = False
doesSomething _ = True

-- An effect is either setting a literal, or assigning a fluent (a function term with name and parameters) a value
data Effect = EPredicate Literal
            | EAssignment Term Term
    deriving Eq

instance Show Effect where
  showsPrec _ (EPredicate l) = shows l
  showsPrec _ (EAssignment f v) = ("(assign "++) . shows f . (' ':) . shows v . (')':)


data CEffect = Conditional DNF [CEffect]
             | Unconditional { thePeff::Effect }

instance Show CEffect where
  showsPrec _ (Conditional c [e]) = ("(when "++) . shows c . (' ':) . shows e . (')':)
  showsPrec _ (Conditional c es)  = ("(when "++) . shows c . (' ':) . showsTaggedList ("and"++) es . (')':)
  showsPrec _ (Unconditional l)   = shows l
  showList = showsRawList shows

instance Eq CEffect where
  (Conditional a b) == (Conditional c d) = and [a==c,b==d]
  (Unconditional eff1) == (Unconditional eff2) = eff1 == eff2
  _ == _ = False

----------------------------------------------------------------------
-- Fluents

-- A fluent declaration (without type): name and variables.
data Fluent = Fluent {flsym  :: Symbol,
                      fldecl :: VDecls}

instance Show Fluent where
    showsPrec _ f = ("("++) . shows (flsym f)
                    . (" "++) . shows (fldecl f) . (")"++)

-- The type of a fluent or variable (only Object and Num from to)
-- Type_Object indicates usual object fluents, Type_Num , and Type_NativeNum
data Type = Type_Object Symbol -- usual object fluents
            | Type_Num Int Int -- numeric (integer) fluents within a pre-defined range
            | Type_NativeNum -- some implementation-specific integer fluent without a guaranteed range (usually 32 bit)
            | Type_Float -- IEEE754 single precision
     deriving (Eq, Ord)

instance Show Type where
    showsPrec _ (Type_Object sym) = shows sym
    showsPrec _ Type_NativeNum    = shows numberTypeSymbol
    showsPrec _ Type_Float        = shows floatTypeSymbol
    showsPrec _ (Type_Num from to) = ("("++) . shows numberTypeSymbol . (" "++) . shows from . (" "++) . shows to . (")"++)

----------------------------------------------------------------------
-- Actions and GroundActions

data Action = Action {
                asym :: Symbol,
                adecl:: Maybe VDecls,
                asal :: Maybe Term,
                aagnt:: Maybe Term,
                adur :: Maybe Atom,
                apre :: Maybe Formula,
                aeff :: Maybe EFormula,
                airr :: Bool,
                apff :: Maybe (Maybe Formula),
                acbk :: Maybe EFormula}
            | DurativeAction {
                asym::Symbol,
                adecl:: Maybe VDecls,
                asal :: Maybe Term,
                aagnt:: Maybe Term,
                adur :: Maybe Atom,
                dapre:: Maybe TimedFormula,
                daeff:: Maybe TimedEFormula,
                airr :: Bool,
                apff :: Maybe (Maybe Formula),
                acbk :: Maybe EFormula}

instance Eq Action where
    a1 == a2 | asym a1 /= asym a2 = False
             | adecl a1 /= adecl a2 = False
             | otherwise = True

instance Ord Action where
    compare a1 a2 =
        case compare (asym a1) (asym a2) of
            LT -> LT
            GT -> GT
            EQ -> compare (adecl a1) (adecl a2)

instance Show Action where
    showsPrec _ a = case a of
                 Action{} -> ("(:action "++) . shows (asym a)
                             . ((if airr a then crnl 1 . (":irrational"++) else (""++)))
                             . maybeShow showVars (adecl a)
                             . maybeShow showSaliency (asal a)
                             . maybeShow showAgent (aagnt a)
                             . maybeShow showDuration (adur a)
                             . maybeShow showPrecond (apre a)
                             . maybeShow showEffect (aeff a)
                             . maybeShow showCallbacks (acbk a)
                             . (')':)
                 DurativeAction{} -> ("(:durative-action "++) . shows (asym a)
                                     . ((if airr a then crnl 1 . (":irrational"++) else (""++)))
                                     . maybeShow showVars (adecl a)
                                     . maybeShow showSaliency (asal a)
                                     . maybeShow showAgent (aagnt a)
                                     . maybeShow showDuration (adur a)
                                     . maybeShow showTimedPrecond (dapre a)
                                     . maybeShow showEffect (daeff a)
                                     . maybeShow showCallbacks (acbk a)
                                     . (')':)
        where maybeShow _ Nothing  = id
              maybeShow f (Just a) = f a
              showVars v    = crnl 1 . (":parameters ("++) . shows v . (')':)
              showSaliency a    = crnl 1. (":saliency "++) . shows a
              showAgent a    = crnl 1. (":agent "++) . shows a
              showDuration d = crnl 1 . (":duration "++) . shows d
              showPrecond p = crnl 1 . (":precondition "++) . shows p
              showTimedPrecond p = crnl 1 . (":condition "++) . shows p
              showEffect e  = crnl 1 . (":effect "++) . shows e
              showCallbacks e  = crnl 1 . (":callbacks "++) . shows e
    showList = sfoldr (\e -> crnl 0 . shows e) id

data ASTFile = ASTDomain { astSym :: Symbol, astEntries :: [ASTEntry] }
             | ASTProblem { astSym :: Symbol, astEntries :: [ASTEntry] }
             deriving Show


type AgentID = Maybe Constval

data GAction = GAction {gname::[Constval],
                        baseAction :: Action, -- base action scheme
                        gsal::Maybe Term,
                        gagt::AgentID,
                        gdur::Maybe Atom,
                        gpre::DNF,
                        geff::ENF,
                        gcbk::ENF,
                        prelits :: [Literal],
                        efflits :: [Literal], -- all literals possible
                        efffluents :: [Term], -- instantiated fluents assigned any value (note: because of assignments like
                                              -- (assign (f1 a) (f2 b)) fluents mostly have an undetermined value at
                                              -- compile time, keeping track of them would probably be not worth the effort)
                        bindingIndex :: Int,  -- unique id of this action, used in cantOccurAfter
                        cantOccurAfter :: [Int], -- list of actions after which this action must not be executed (non-repeatable actions)
                        glevel :: Int,        -- ^ distance to goal (sort of ...)
                        gachieve :: Int,       -- ^ number of relevant literals achieved
                                              --
                                              -- Big question: is there any correlation between
                                              -- the 'glevel' and 'gachieve' values and
                                              -- the probability of the action appearing at
                                              -- a certain position in the plan? (or appearing at
                                              -- all?) I other words: are these values helpful
                                              -- in guiding operator selection?
                        specificity :: Int,
                            -- ^ an act-r conflict resolution strategy
                        girr :: Bool
                            -- ^ Is the action irrational?
                       }
mkGAction name base sal agt dur pre eff cbk spec irr bi coa =
         GAction{
                gname=name,
                baseAction=base,
                gsal=sal,
                gagt=agt,
                gdur=dur,
                gpre=pre,
                geff=eff,
                gcbk=cbk,
                prelits=literals pre,
                efflits=literals eff,
                efffluents=assignments eff,
                bindingIndex=bi,
                cantOccurAfter=coa,
                glevel=(-1),
                gachieve=0,
                specificity = spec,
                girr = irr
         }
instance Show GAction where
    showsPrec _ a = ("(:action "++) . shows (head (gname a)) . sfoldr (\e -> ('.':) . shows e) id (tail (gname a))
                    . (" (:glevel "++) . shows (glevel a) . (" :gachieve "++) . shows (gachieve a) . (')':)
                    . mshow ":saliency" (gsal a)
                    . mshow ":agent" (gagt a)
                    . mshow ":duration" (gdur a)
                    . maybeShow showPrecond (gpre a)
                    . crnl 1 . (":bindingIndex "++) . shows (bindingIndex a)
                    . crnl 1 . (":cantOccurAfter "++) . shows (cantOccurAfter a)
                    . showEffect (geff a)
                    . showCallbacks (gcbk a)
                    . (')':)
        where mshow _ Nothing = id
              mshow s (Just v) = crnl 1 . (s++) . (' ':) . shows v
              showPrecond p = crnl 1 . (":precondition "++) . shows p
              showEffect (MkENF []) = id
              showEffect e  = crnl 1 . (":effect "++) . shows e
              showCallbacks (MkENF []) = id
              showCallbacks e  = crnl 1 . (":callbacks "++) . shows e
              maybeShow how dnf = if isTrue dnf then id else how dnf
    showList = sfoldr (\e -> crnl 0 . shows e) id

-- Eq and Ord are defined in terms of the action's name, action scheme and binding index
-- the binding index is unique and is used as a last resort in case (scheme, name) is not unique
instance Eq GAction where
    a == b = gname a == gname b && baseAction a == baseAction b && bindingIndex a == bindingIndex b

instance Ord GAction where
    -- first, order according to action scheme (keeps ground actions together)
    compare a b =
        case compare (baseAction a) (baseAction b) of
            EQ -> case compare (gname a) (gname b) of
                EQ -> compare (bindingIndex a) (bindingIndex b)
                x  -> x
            x  -> x

----------------------------------------------------------------------
-- Files

type Typedef  = (Symbol, [Symbol]) -- hierarchy of object types
type Constdef = (Symbol, [Constval])
type Fluentdef = (Type, [Fluent])

data ASTEntry = ASTTypesEntry [Typedef]
              | ASTConstantsEntry [Constdef]
              | ASTPredicatesEntry [PDecl]
              | ASTFluentsEntry [Fluentdef]
              | ASTActionEntry Action
              | ASTObservationEntry [EFormula]
              ---------------------
              | ASTDomainEntry Symbol
              | ASTObjectsEntry [Constdef]
              | ASTInitEntries (Maybe Atom) (Map.Map Symbol EFormula) (Map.Map Symbol [(Term,Constval)]) -- supports multipe named initial states
              | ASTGoalEntries (Map.Map Symbol Formula) -- supports multiple named goals
              | ASTLandmarksEntry Formula
              deriving Show

showCD (typ,cnst) = crnl 1 . (showsRawList shows cnst) . (" - "++) . shows typ

newtype VDecls = VDecls [(Type, [Symbol])]
    deriving (Eq, Ord)

instance Show VDecls where
         showsPrec _ (VDecls d) = showsRawList showVD d
                   where showVD (typ,vars) = (showsRawList showV vars) . (" - "++) . shows typ
                         showV v           = ('?':) . shows v

declvars (VDecls decls) = concat (map snd decls)

data PDecl = PDecl Symbol VDecls
instance Show PDecl where
         showsPrec _ (PDecl s v) = ('(':) . shows s . (' ':) . shows v . (')':)
----------------------------------------------------------------------
-- Consolidated PDDL Definition

data Domain = Domain { domain :: Symbol,
                       types :: [Typedef],
                       constants :: [Constdef],
                       predicates :: [PDecl],
                       fluents :: [Fluentdef],
                       actions :: [Action],
                       observation :: [EFormula] }

instance Show Domain where
  showsPrec _ d = ("(define (domain "++) . shows (domain d) . (')':)
                  . tagfold ":types" showCD (types d)
                  . tagfold ":constants" showCD (constants d)
                  . tagfold ":predicates" (\e -> crnl 1 . shows e) (predicates d)
                  . tagfold ":functions" showCD (fluents d)
                  . shows (actions d)
                  . case observation d of {[] -> id; obs -> ("\n(:observation\n\t"++) . shows obs . (')':) }
                  . (')':)

emptyDomain = Domain {domain=nullSymbol
                     ,types=[]
                     ,constants=[]
                     ,predicates=[]
                     ,fluents=[]
                     ,actions=[]
                     ,observation=[]}


addDomainEntry :: ASTEntry -> Domain -> Domain
addDomainEntry e d =
 case e of
 ASTTypesEntry t        -> d{types = t ++ types d}
 ASTConstantsEntry c    -> d{constants = c ++ constants d}
 ASTPredicatesEntry pds -> d{predicates = pds ++ predicates d}
 ASTFluentsEntry fls    -> d{fluents = fls'}
                            where fls1 = fls ++ fluents d
                                  names = concat $ map (map flsym . snd) fls1 -- all fluent names
                                  multiple = names List.\\ List.nub names
                                  fls' = if null multiple then fls1 else error $ "The fluent has already been defined: " ++ show multiple
 ASTActionEntry a | a `elem` actions d -> error $ "Action has already been defined: " ++ show a ++ "\nprevious definition: " ++ show (List.find (== a) (actions d))
                  | otherwise -> d{actions = a:actions d}
 ASTObservationEntry o  -> d{observation = observation d ++ o}
 _                      -> error $ "Bad domain entry: "++show e

data Problem = Problem { problem :: Symbol,
                         pdomain :: Symbol,
                         objects :: [Constdef],
                         inits :: Map.Map Symbol EFormula, -- map of all named initial states
                         -- after parsing, initFuns contains static function assignments and initial fluent assignments for each named initial state
                         -- later (when it is known which Terms are fluents, and which are static), this is split into fconsts and initFluents
                         -- it is an error to define static function other than in ":all"
                         initFuns :: Map.Map Symbol (Map.Map Term Constval),
                         -- fconsts is the list of static function assignments, which hold in all initial states
                         fconsts :: Map.Map Term Constval,
                         -- initial assignments of fluents, set in compile function after domain knowledge (i.e. when it is known which functions are declared as fluents)
                         -- ground instantiated fluents are represented as a term, which will always be a function with only constants
                         -- this simplifies some computations, as this is the same type used for static functions (which are also only functions with constants)
                         initFluents :: Map.Map Symbol (Map.Map Term Constval), -- for each named initial state, a list of all initial fluents
                         idur :: Maybe Atom, -- duration of the initial action
                         goals :: Map.Map Symbol Formula, -- list of all named, potential goals; each goal is one goal formula
                         landmarks :: Formula}

instance Show Problem where
  showsPrec _ p = ("(define (problem "++) . shows (problem p) . (')':)
                  . crnl 0 . ("(:domain "++) . shows (pdomain p) . (')':)
                  . tagfold ":objects" showCD (objects p)
                  . crnl 0 . ("(:inits "++) . idurshow p
                           . foldr initShow id initNames
                           . (')':) -- (\e -> crnl 1 . shows e) (inits p)
                  . crnl 0 . ("(:goals "++)
                           . Map.foldrWithKey (\name g r -> crnl 1 . ("( = " ++) . shows name . (' ':) . shows g . (')':) . r) id (goals p)
                           . (')':)
                  . (')':)
    where idurshow p = case idur p of
                         Nothing -> id
                         Just a  -> (":duration "++) . shows a . ('\n':)
          fcshow k v r = (" (= "++) . shows k . (' ':) . shows v . (')':) . r
          -- all names of the initial states, setting ":all" at first
          initNames = List.nub $ Map.keys (inits p) ++ Map.keys (initFluents p) ++ Map.keys (initFuns p)
          -- show one named initial state
          initShow name r = crnl 1 . ("( = " ++) . shows name . (' ':)
                            . case Map.lookup name (inits p) of
                                Nothing -> id
                                Just i -> shows i
                            -- find the named maps from initFluents and initFuns, and unify it with fconsts
                            . Map.foldrWithKey fcshow id ( Map.unions . (fconsts p :) . map (Map.findWithDefault Map.empty name) $ [initFuns p, initFluents p] )
                            . (')':) . r

emptyProblem = Problem {problem=nullSymbol, pdomain=nullSymbol, objects=[], inits=Map.empty, initFuns=Map.empty, fconsts=Map.empty, initFluents=Map.empty, idur=Nothing, goals=Map.empty, landmarks=FAnd []}

addProblemEntry :: ASTEntry -> Problem -> Problem
addProblemEntry e p =
 case e of
    ASTDomainEntry d     -> p{pdomain=d}
    ASTObjectsEntry o    -> p{objects=o++objects p}
    ASTInitEntries d is fs -> p{inits = Map.unionWith (\(EFAnd fs1) (EFAnd fs2) -> EFAnd (fs1++fs2)) is (inits p) -- merge initial states with same name
                               ,idur = if idur p == Nothing || idur p == d then d else error $ "Error: currently only one initial duration is supported"
                               ,initFuns = unifyFuns (Map.mapWithKey (\iname -> Map.fromListWithKey (unifyFunsError iname)) fs) (initFuns p)
                               }
    ASTGoalEntries gs    -> p{goals = Map.unionWith (\g1 g2 -> FAnd [g1, g2]) gs (goals p)} -- conjunct goals with same name
    ASTLandmarksEntry l  -> p{landmarks=l}
    _                    -> error $ "Bad problem entry: "++show e
  where
    unifyFunsError iname fl v1 v2 = error $ "Error: In initial state " ++ show iname ++ ", function/fluent " ++ show fl ++ "assigned two values: " ++ show [v1, v2]
    -- unify the function assignment maps of two initial-state maps -- defining a function twice is an error
    unifyFuns = Map.unionWithKey (\iname -> Map.unionWithKey (unifyFunsError iname))

-- Removes the :all init/goal state and adds it to all other states
-- Therefore, we have only those states that are actually required (:all is only a meta-state)
-- in case only :all exists in the map, rename it to the problem name
consolidateInitGoalStates p = p{inits = cinits, initFuns = cfuns, goals = cgoals}
    where
        cinits = consMap (\iall i -> EFAnd [iall, i]) (inits p) -- combine initial state assignments with AND
        cfuns  = consMap (\fall f -> Map.union f fall) (initFuns p) -- join static function / fluent assignments, and prefer specific values from the named map
        cgoals = consMap (\gall g -> FAnd [gall, g]) (goals p) -- join goal state formulas
        -- consMap fun map:
        -- fun is the combination function with two arguments: the entry :all, and the entry of another initial/goal state, and returns their combination
        -- map is the map from the name to the initial/goal state
        -- it removes :all from the map, and combined all other entries with :all
        consMap :: (a -> a -> a) -> Map.Map Symbol a -> Map.Map Symbol a
        consMap f m
            | Map.keys m == [allSymbol] = Map.mapKeys (const $ problem p) m
            | otherwise = case Map.lookup allSymbol m of
                            Nothing ->  m -- no :all, return unchanged
                            Just all -> Map.map (f all) (Map.delete allSymbol m)

consolidateFile :: ASTFile -> Either Problem Domain
consolidateFile (ASTProblem s l) = Left . consolidateInitGoalStates $ foldr addProblemEntry (emptyProblem{problem=s}) l
consolidateFile (ASTDomain s l)  = Right $ foldr addDomainEntry (emptyDomain{domain=s}) l

data DomainInfo = DomainInfo {
    diDomain :: Domain,
    diProblem :: Problem,
    diAllPreds :: [Symbol],
    diDynPreds :: [Symbol],
    diWeedOut :: Bool,
    diResolveCondEff :: Bool,
    diSelectedInit :: Maybe String, -- only compile using a single initial state / goal
    diSelectedGoal :: Maybe String
}

isValid :: DomainInfo -> Bool
isValid di = domain(diDomain di) == pdomain(diProblem di)
diDomainName = domain . diDomain
diPDomainName = pdomain . diProblem

infocomment n s = (((take n (repeat '\n'))++(take 70 (repeat ';'))++"\n;;;; "++s++"\n")++)

instance Show DomainInfo where
  showsPrec _ di = ("(:parserInfo ; BEGIN PARSER INFO\n"++)
                   . infocomment 0 "Domain"
                   . shows (diDomain di)
                   . infocomment 2 "Problem"
                   . shows (diProblem di)
                   . infocomment 2 "Predicates used in preconditions & effects"
                   . shows (diAllPreds di)
                   . infocomment 2 "Predicates changed by effects"
                   . shows (diDynPreds di)
                   . ("\n) ; END PARSER INFO"++)
