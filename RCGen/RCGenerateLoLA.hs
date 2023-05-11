-- Generates a model file for the PRISM model checker (http://www.prismmodelchecker.org/)
-- A simple MDP without durations will be generated.
module RCGenerateLoLA (generateLoLA) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Maybe

import Util
import RCGenerateUtil
import RCData hiding (crnl)
import RCCompile

-- returns
-- * the petri net
-- * a list of all goal formulas
-- * more formula (esp. consistency checks for the net)
generateLoLA :: CompileResult -> (ShowS, Map Symbol ShowS, Map String ShowS)
generateLoLA cr =
  let
    luts = buildLoLALuts cr
    net = highLevelNet cr luts
    goals = Map.empty -- Map.mapWithKey generateGoalFormulas (goalDNFs cr)
    formulaConsistency = genFormulaConsistency luts
  in (
      ps "/* LoLA high level petri net, generated from CCBM/PDDL\n"
    . ps " Domain " . (shows.fst.dpid $ cr) . ps "\n"
    . ps " Problem " . (shows.snd.dpid $ cr) . ps "\n"
    . ps "*/\n"
    -- LoLA doe snot support constants (TRUE, FALSE, 1, "knife") in the CONSUME and PRODUCE rules, only terms
    -- therefore, we define these operations which can be used as a term true()
    . ps "FUNCTION true() : BOOLEAN BEGIN RETURN TRUE END\n"
    . ps "FUNCTION false() : BOOLEAN BEGIN RETURN FALSE END\n"
    . crnl
    . net
    , goals
    , Map.fromList [("placeConsistency", formulaConsistency)]
     )

-- LoLA specific functions

-- get the Luts
-- in contrast to probably most other formats, we use an interval of [1, n] for representing values of object fluents
-- The reason is that we need some distinguished and unique element to represent "invalid" values, which is 0 here (C, e.g.: -1)
buildLoLALuts :: CompileResult -> Luts
buildLoLALuts cr = luts{luObjConstIDs = objConstIDs', luObjFlMap = objFlMap'}
  where
    luts = buildLuts cr compare strescape
    objConstIDs' = Map.map (+1) (luObjConstIDs luts)
    objFlMap' = Map.map (Map.map (+1)) (luObjFlMap luts)

-- escape characters not permitted by LoLA for identifiers
strescape :: String -> String
strescape =
      snd . foldr zap ('\0', "") -- replace repeated _ by a single _
    . map (\c -> if c `elem` invalid then '_' else c) -- escape invalid
    . filter (not . (`elem` strip)) -- remove unwanted
  where
    zap '_' r@('_', _) = r
    zap c (_, result) = (c, c:result)
    invalid = ",;:(){} "
    strip = "()" -- remove parantheses completely

escape :: Show a => a -> String
escape = strescape . show

escapes :: Show a => a -> ShowS
escapes s = ps $ escape s

-- Generate the HighLevel net

highLevelNet :: CompileResult -> Luts -> ShowS
highLevelNet cr luts =
  let
    (initMarking, numberFunctions) = genInitialMarking cr luts
    transitions = foldr (.) id . List.intersperse crnl . map (\(action, (gaName, _idx)) -> genTransition luts action gaName) . Map.toList $ luActions luts
  in
      numberFunctions
    . genSorts luts
    . crnl
    . genPlaces luts
    . crnl
    . initMarking
    . crnl
    . transitions


-- for a given type, return the LoLA type name (name of the "sort"/domain)
-- in LoLA, we use one domain for all object fluents
-- Using individual domains of object fluents complicates assigning and comparing types which overlap, esp. with "invalid" numbers
-- But this blows up the code, and LoLA should be gould at compressing the model anyway
getSortName :: Type -> ShowS
getSortName (Type_Object s) = ps "object_" . escapes s
getSortName (Type_Num from to) = ps "[ 0 , " . shows (to - from) . ps " ]"
getSortName Type_NativeNum = error $ "LoLA cannot efficiently handle large numbers as occured by fluent type number" -- the low-level net would explode
getSortName Type_Float = error $ "LoLA does not support float values (occured as type of a fluents)"

-- Generate the various type domains used for (object) fluents
-- Each object fluent type gets its own value domain
-- Therefore, we need (and generate here) conversion functions to convert an object ID to a global unique ID (UID)
-- This is required for comparing object fluents of different types
-- Also, generate functions to access each object constant (required at least for the initial marking)
genSorts :: Luts -> ShowS
genSorts Luts{luObjFlMap=objFlMap, luObjConstIDs = objConstIDs} =
  let
    -- For each type, two functions are generated: ID-to-UID and UID-to-ID
    -- This is required to compare (and assign) object fluents of different types (where one may be a subset of another)
    -- While the former is always possible, the latter conversion must return in invalid value for UIDs of a constant that are not part of that specific type
    -- Here, we use "0" for every ID to denote an invalid value (this value must be unique across all domains, which may well have different sizes)
    -- Hence, the "usual" values of a fluent are in the interval [1, n], where n is the number of possible values.
    -- For consistency, the "all_objects" sort also gets an interval [0, n], where 0 is the invalid UID assigned to each invalid ID
    -- Usually, any fluent should have never assigned the value 0 -- this is only required for comparison

    objSort (typeSym, m) = getSortName (Type_Object typeSym) . ps " = [ 0 , " . shows (Map.size m) . ps " ];\n"

    objFunctions :: Symbol -> (Constval, Int) -> ShowS
    objFunctions typeSym (c, i) = ps "FUNCTION const_" . getSortName (Type_Object typeSym) . ps "_" . escapes c . ps " () : " . getSortName (Type_Object typeSym) . ps " BEGIN RETURN " . shows i . ps " END\n"

    -- the list of all constants in objConstIDs in increasing order of index
    sortedAllConsts :: [Constval]
    sortedAllConsts = map fst . List.sortBy (\a b -> compare (fst a) (fst b)) . Map.toList $ objConstIDs

    -- generate an array for each object fluent type, where
    -- - the index = the ("compressed") value of an object for this fluent type
    -- - the value = the unique id
    -- Additionally, generate the inverse array from unique id → value
    showObjects :: (Symbol, Map.Map Constval Int) -> ShowS
    showObjects (typeSym, objMap) =
        -- type-value ID → UID
          ps "FUNCTION " . getSortName (Type_Object typeSym) . ps "_id-to-uid(id: " . getSortName (Type_Object typeSym) . ps ") : all_objects\n"
        . ps "VAR arr: ARRAY " . getSortName (Type_Object typeSym) . ps " OF all_objects;\n"
        . ps "BEGIN\n"
        . ps "\tarr = [ 0 | " . foldsIntersperse (ps " | ") (\c -> shows $ objConstIDs `lukup` c) sortedConsts . ps " ];\n"
        . ps "\tRETURN arr [ id ]\n"
        . ps "END\n"
        -- UID → type-value ID
        . ps "FUNCTION " . getSortName (Type_Object typeSym) . ps "_uid-to-id(uid: all_objects) : " . getSortName (Type_Object typeSym) . ps "\n"
        . ps "VAR arr: ARRAY all_objects OF " . getSortName (Type_Object typeSym) . ps ";\n"
        . ps "BEGIN\n"
        . ps "\tarr = [ 0 | " . foldsIntersperse (ps " | ") (\c -> shows $ Map.findWithDefault 0 c objMap) sortedAllConsts . ps " ];\n"
        . ps "\tRETURN arr [ uid ]\n"
        . ps "END\n"
      where
        -- For this type, the constants in order of the numeric representation (0 .. (length - 1))
        sortedConsts :: [Constval]
        sortedConsts = map fst . List.sortBy (\a b -> compare (fst a) (fst b)) . Map.toList $ objMap
  in
      ps "SORT\n"
    . ps "all_objects = [ 0 , " . shows (Map.size objConstIDs) . ps " ];\n"
    . folds objSort (Map.toList objFlMap)
    . crnl
    . folds (\(typeSym, m) -> folds (objFunctions typeSym) (Map.toList m)) (Map.toList objFlMap)
    . crnl
    . folds showObjects (Map.toList objFlMap)


getPlaceNameAtom Luts{luAtom=luAtom} a = ps "pa_" . ps (luAtom `lukup` a)
getPlaceNameFluent Luts{luFluent=luFluent} f = ps "pf_" . ps (luFluent `lukup` f)

-- Generate the places and the initial marking

-- all places are safe:
-- fluents have only one value, atoms are either TRUE or FALSE
-- note that the SAFE counts for each kind of token (i.e., each possible value) independently: it would still be allowed that a BOOLEAN place contains one TRUE and one FALSE token (therefore, we generate a formula that checks this)
genPlaces luts =
  let
    atomDef a   = ps "\t" . getPlaceNameAtom luts a . ps ": BOOLEAN"
    fluentDef f = ps "\t" . getPlaceNameFluent luts f . ps ": " . getSortName (luFlTypes luts `lukup` f)

    placesAtoms = map atomDef (Map.keys $ luAtom luts)
    placesFluents = map fluentDef (Map.keys $ luFluent luts)
  in
      ps "PLACE SAFE:\n"
    . foldsIntersperse (ps ",\n") id (placesAtoms ++ placesFluents)
    . ps "\n;\n"


showsOffset offset = case compare offset 0 of
        LT -> ps " - " . shows (-offset) -- force spaces, + and - are valid parts of identifiers
        EQ -> id
        GT -> ps " + " . shows offset

fluentValue :: Luts -> Term -> Term -> ShowS
fluentValue luts f v = fluentValue' luts f (Just v)

-- show fluent value, if value is missing, show the lowest value
fluentValue' :: Luts -> Term -> Maybe Term -> ShowS
fluentValue' luts f value =
    let fltype = luFlTypes luts `lukup` f
    in case value of
        Nothing -> case fltype of -- if there is no defined initial value, use the "lowest" value
             (Type_Object _)    -> ps "1" -- first non-invalid value
             (Type_Num _ _)     -> ps "0" -- all values are shifted towards 0
             Type_NativeNum     -> ps "0"
             Type_Float         -> error $ "LoLA does not support float fluent: " ++ show f
        Just (Atomic (Constant c)) -> case fltype of
             t@(Type_Object _)  -> ps "const_" . getSortName t . ps "_" . escapes c . ps "()"
             (Type_Num from _)  -> shows $ (\(Intconst i)->i) c - from
             Type_NativeNum     -> shows c
             Type_Float         -> error $ "LoLA does not support float fluent: " ++ show f
        Just v -> error $ "Unexpected non-const value " ++ show v


-- returns
-- - the definition of the initial marking
-- - definitions of functions required in the initial marking (for numeric fluents)
-- the distinction is necessary because the sorts and functions must be declared at first
genInitialMarking :: CompileResult -> Luts -> (ShowS, ShowS)
genInitialMarking cr luts =
  let
    (ils, ifl) = if Map.size (initialStates cr) == 1
                   then head . Map.elems $ initialStates cr
                   else error $ "LoLA supports only one initial state, but there are: " ++ show (Map.keys $ initialStates cr)

    -- In the initial marking, we cannot use numeric expressions but must use functions returning the number
    -- we generate number functions only for the initial marking - numbers are otherwise required only in the guard where numeric expressions are allowed
    numFunction i = ps "FUNCTION number_" . shows i . ps " () : [ " . shows i . ps " , " . shows i . ps " ] BEGIN RETURN " . shows i . ps " END\n"
    initialNums = List.sort . List.nub . filter (\c -> case c of { (Intconst _) -> True; _ -> False }) $ Map.elems ifl
    numFunctions = if null initialNums then id else folds numFunction initialNums . crnl

    atomInit a = ps "\t" . getPlaceNameAtom luts a . ps ": "
               . ps (if Literal True a `elem` ils then "true()" else "false()")
    fluentInit f =
      let
        initValue = let v = fromJust . Map.lookup f $ ifl in case v of
            Symconst _ -> fluentValue luts f (Atomic . Constant $ v)
            Intconst i -> ps "number_" . shows i . ps "()"
            Numconst _ -> error $ "LoLA does not support floats"
      in
        ps "\t" . getPlaceNameFluent luts f . ps ": " . initValue

    markingsAtom = map atomInit (Map.keys $ luAtom luts)
    markingsFluent = map fluentInit (Map.keys $ luFluent luts)

    markings =
          ps "MARKING\n"
        . foldsIntersperse (ps ",\n") id (markingsAtom ++ markingsFluent)
        . ps "\n;\n"
  in (markings, numFunctions)

-- Generate the transitions

-- `shows luts pfx dnf` shows the DNF, where each predicate/fluent name is prefixed with pfx
showDNF :: Luts -> ShowS -> DNF -> ShowS
showDNF luts@(Luts{luFlTypes=fluentTypes}) pfx pre@(MkDNF lss) | isTrue pre = ps "TRUE"
                                 | otherwise  = showDisjuncts lss
  where
    showDisjuncts :: [[Literal]] -> ShowS
    showDisjuncts = foldsIntersperse (ps " OR ") showConjuncts
    showConjuncts :: [Literal] -> ShowS
    showConjuncts ls = ps "(" . (foldsIntersperse (ps " AND ") showLit ls) . ps ")"
    showLit (Literal b a) = pfx . getPlaceNameAtom luts a . ps " = " . ps (if b then "TRUE" else "FALSE")
    showLit (LiteralF f) = ps "(" . case f of
                                      FEqual t1 t2        -> showFluentCmp " = " t1 t2
                                      FNEqual t1 t2       -> showFluentCmp " <> " t1 t2
                                      FGreaterThan t1 t2  -> showFluentCmp " > " t1 t2
                                      FGreaterEqual t1 t2 -> showFluentCmp " >= " t1 t2
                                      FLessThan t1 t2     -> showFluentCmp " < " t1 t2
                                      FLessEqual t1 t2    -> showFluentCmp " <= " t1 t2
                         . ps ")"
    showFluentCmp _ (Atomic _) (Atomic _)                = error $ "Unexpected const-expression"
    showFluentCmp op f1@(Function _ _) f2@(Atomic _)     = pfx . getPlaceNameFluent luts f1 . ps op . fluentValue luts f1 f2
    showFluentCmp op f1@(Function _ _) f2@(Function _ _) = pfx . getPlaceNameFluent luts f1 . ps op .
        case (typef1, typef2) of
            _ | typef1 == typef2 -> pfx . getPlaceNameFluent luts f2-- exactly same type: raw comparison
            (Type_Object _,     Type_Object _   ) -> getSortName typef1 . ps "_uid-to-id(" . getSortName typef2 . ps "_id-to-uid(" . pfx . getPlaceNameFluent luts f2 . ps "))" -- convert id → uid → id
            (Type_Object _,     _               ) -> mkError
            (Type_Num from _,   Type_Num from' _) -> pfx . getPlaceNameFluent luts f2   . showsOffset (from' - from)
            (Type_Num from _,   Type_NativeNum  ) -> pfx . getPlaceNameFluent luts f2 . showsOffset (- from)
            (Type_Num _ _,      _               ) -> mkError
            (Type_NativeNum,    Type_Num from _ ) -> pfx . getPlaceNameFluent luts f2 . showsOffset from
            (Type_NativeNum,    _               ) -> mkError
            (Type_Float,        Type_Object _   ) -> mkError
            (Type_Float,        Type_Num from _ ) -> pfx . getPlaceNameFluent luts f2 . showsOffset from
            (Type_Float,        Type_NativeNum  ) -> pfx . getPlaceNameFluent luts f2
            _ -> mkError
      where typef1 = fluentTypes `lukup` f1
            typef2 = fluentTypes `lukup` f2
            mkError = error $ "Unexpected comparison of fluent " ++ show f1 ++ " (type " ++ show typef1 ++ ") to fluent " ++ show f2 ++ " (type " ++ show typef2 ++ ")"
    showFluentCmp op (FFun c) f2@(Atomic _)              = showFunCall luts pfx c . ps op . shows f2
    showFluentCmp op (FFun c) f2@(Function _ _)          = showFunCall luts pfx c . ps op .
        case typef2 of
            Type_Object _   -> error $ "Unexpected comparison of (arithmetic) function call " ++ show c ++ " to object fluent " ++ show f2
            Type_Num from _ -> pfx . getPlaceNameFluent luts f2 . showsOffset from
            Type_NativeNum  -> pfx . getPlaceNameFluent luts f2
            Type_Float      -> pfx . getPlaceNameFluent luts f2
      where
        typef2 = fluentTypes `lukup` f2
    showFluentCmp op (FFun c1) (FFun c2)                 = showFunCall luts pfx c1 . ps op . showFunCall luts pfx c2
    showFluentCmp op f1 f2                               = showFluentCmp (swapOp op) f2 f1
    swapOp op | op == " > "  = " < "
              | op == " >= " = " <= "
              | op == " < "  = " > "
              | op == " <= " = " >= "
              | op == " = "  = " = "
              | op == " <> " = " <> "
              | otherwise  = error $ "Unsupported operator " ++ op

-- Here, we do not print the value of a constant wrt. a fluent, but its "true" value in the model
showFunCall :: Luts -> ShowS -> FunCall -> ShowS
showFunCall luts@(Luts{luFlTypes=fluentTypes}) pfx c =
    ps "(" . case c of
        FPlus t1 t2  -> showTerm t1 . ps " + " . showTerm t2
        FMinus t1 t2 -> showTerm t1 . ps " - " . showTerm t2
        FMult t1 t2  -> showTerm t1 . ps " * " . showTerm t2
        FDiv t1 t2   -> showTerm t1 . ps " / " . showTerm t2
  . ps ")"
  where
    showTerm v@(Atomic _)      = shows v
    showTerm (FFun c')         = showFunCall luts pfx c'
    showTerm f@(Function _ _)  =
        case typef of
            Type_Object _   -> error $ "Unexpected object fluent " ++ show f ++ " in a function call " ++ show c
            Type_Num from _ -> ps "(" . pfx . getPlaceNameFluent luts f . showsOffset from . ps ")"
            Type_NativeNum  -> pfx . getPlaceNameFluent luts f
            Type_Float      -> pfx . getPlaceNameFluent luts f
      where
        typef = fluentTypes `lukup` f

showAssignment :: Luts -> Term -> Term -> ShowS
showAssignment luts@(Luts{luFlTypes=fluentTypes}) fl value  =
      ps "("
    . ps "n" . getPlaceNameFluent luts fl . ps " = " . showTerm fl value
    . ps ")\n"
  where
    showTerm f v@(Atomic _)       = fluentValue luts f v
    showTerm f f'@(Function _ _)  =
      case (typef, typef') of
          _ | typef == typef'                 -> ps "x" . getPlaceNameFluent luts f' -- exactly same type: raw copy
          (Type_Object _,   Type_Object _   ) -> getSortName typef . ps "_uid-to-id(" . getSortName typef' . ps "_id-to-uid(x" . getPlaceNameFluent luts f' . ps "))" -- convert id → uid → id
          (Type_Object _,   _               ) -> mkError
          (Type_Num from _, Type_Num from' _) -> ps "x" . getPlaceNameFluent luts f' . showsOffset (from' - from)
          (Type_Num from _, Type_NativeNum  ) -> ps "x" . getPlaceNameFluent luts f' . showsOffset (- from)
          (Type_Num _ _,    _               ) -> mkError
          (Type_NativeNum,  Type_Num from _ ) -> ps "x" . getPlaceNameFluent luts f' . showsOffset from
          (Type_NativeNum,  _               ) -> mkError
          (Type_Float,      _               ) -> error $ "Floats not supported"
      where typef = fluentTypes `lukup` f
            typef' = fluentTypes `lukup` f'
            mkError = error $ "Unexpected assignment of fluent " ++ show f' ++ " (type " ++ show typef' ++ ") to fluent " ++ show f ++ " (type " ++ show typef ++ ")"
    showTerm f (FFun c)           = case fluentTypes `lukup` f of
                                        Type_Object _    -> error $ "Unexpected object fluent " ++ show f ++ " as assignment-target of (arithmetic) function call " ++ show c
                                        Type_Num from _  -> showFunCall luts (ps "x") c . showsOffset (- from)
                                        Type_NativeNum   -> showFunCall luts (ps "x") c
                                        Type_Float       -> showFunCall luts (ps "x") c

genTransition :: Luts -> GAction -> String -> ShowS
genTransition luts@(Luts{luFlTypes=fluentTypes}) action gaName =
  let
    name = ps "TRANSITION t_" . ps gaName . crnl

    isPred (Literal _ _) = True
    isPred _ = False
    getAtom (Literal _ a) = a
    getAtom _ = undefined

    collectFluentsFromTerms :: Term -> Term -> [Term]
    collectFluentsFromTerms t1 t2 = List.nub $ collectT t1 ++ collectT t2
      where collectT f@(Function _ _) = [f]
            collectT (FFun fc) = funcallapply collectFluentsFromTerms fc
            collectT (Atomic _) = []

    prePredicates :: [Atom]
    prePredicates = List.nub . map getAtom . filter isPred . concat . ors $ gpre action
    preFluents :: [Term]
    preFluents = List.nub . foldr (\l -> (++) $ collectF l) [] . concat . ors $ gpre action
      where collectF (LiteralF f) = List.nub . litfapply collectFluentsFromTerms $ f
            collectF _ = []

    allEffs :: [Effect]
    allEffs = map thePeff . effs . geff $ action -- conditional effects have been resolved

    effLiterals :: [Literal]
    effLiterals = List.nub . foldr getEffPred [] $ allEffs
      where getEffPred (EPredicate l) = if isPred l then (l:) else id
            getEffPred _ = id
    effPredicates :: [Atom]
    effPredicates = map getAtom effLiterals

    -- all fluents used in the effects (assigned or part of an expression)
    effFluents :: [Term]
    effFluents = List.nub . foldr getEffFluents [] $ allEffs
      where getEffFluents (EPredicate l) = if isPred l then id else error $ "Cannot set a formula " ++ show l
            getEffFluents (EAssignment t1 t2) = (++) (collectFluentsFromTerms t1 t2)

    -- all fluents (key) which are assigned some value (element)
    effAssignedFluents :: Map Term Term
    effAssignedFluents = foldr addAssignment Map.empty allEffs
      where
        addAssignment (EPredicate _) = id
        addAssignment (EAssignment t1 t2) = Map.insert t1 t2

    -- The incoming CONSUME arcs must fulfill three functionalities
    -- * check for applicability according to the precondition, e.g. pa_holds_knife : TRUE
    -- * remove any tokens for states that get set only, e.g. pa_holds_spoon : x1 (where x1 is a BOOLEAN variable)
    --   this is required to ensure safety: producing a value (e.g. TRUE) does neither remove any other values (e.g. FALSE), nor ensures that there is exactly one value
    -- * check for applicability of the "grounded" transition, i.e. where some fluent has a particular value, and this fluent is part of some expression. These token get removed and must be re-set

    -- the outgoing PRODUCE arcs also have three functions:
    -- * set values of states according to the effect, e.g. pa_holds_spoon : FALSE
    -- * re-set values of states that got check in the precondition, e.g. pa_holds_knife: TRUE
    -- * re-set values of fluents that are part of an expression, as in (assign (fluent-a) (+ (fluent-b) (fluent-c)))

    -- Therefore we collect all predicates/fluents "touched" by the action and make them variables
    allPredicates :: [Atom]
    allPredicates = List.nub $ prePredicates ++ effPredicates
    allFluents :: [Term]
    allFluents = List.nub $ preFluents ++ effFluents
    allObjFluents = filter (\f -> case fluentTypes `lukup` f of {Type_Object _ -> True; _ -> False}) allFluents

    -- in LoLA, we can neither use constants (e.g. 2, "knife") nor expressions (e.g. fluent_a + 1, "spoon") in the production rule of a transition
    -- constants and expressions are, however, allowed in the GUARD
    -- therefore, any value assigned to fluents will be an additional variable prefixed with "n"

    variables =
          ps "VAR\n"
        -- all predicates are boolean
        . (if null allPredicates then id else
            ps "\t" . (foldsIntersperse (ps ", ") (\a -> ps "x" . getPlaceNameAtom luts a) allPredicates) . ps ": BOOLEAN;\n"
          )
        -- each fluent gets its own type
        . folds (\fl -> ps "\tx" . getPlaceNameFluent luts fl . ps ": " . getSortName (fluentTypes `lukup` fl) . ps ";\n") allFluents
        -- new values assigned to fluents
        . folds (\fl -> ps "\tn" . getPlaceNameFluent luts fl . ps ": " . getSortName (fluentTypes `lukup` fl) . ps ";\n") (Map.keys effAssignedFluents)

    showEff = folds (ps "\tAND " .: uncurry (showAssignment luts)) (Map.toList effAssignedFluents)

    -- the guard is the precondition and defines which "token configurations" are valid for this transition
    -- the guard contains the majority of the "intelligence" of this transition
    guard =
          ps "GUARD\n"
        -- the precondition
        . ps "\t{ Precondition }\n"
        . ps "\t(" . showDNF luts (ps "x") (gpre action) . ps ")\n"
        -- object fluents technically have 0 in their value domain, but this signals only "invalid" used for comparison
        -- therefore, we can force these values to be greater than 0
        . if null allObjFluents then id else (
              ps "\t{ 0 is \"invalid\" for object fluents }\n"
            . ps "\t" . folds (\fl -> ps " AND x" . getPlaceNameFluent luts fl . ps " > 0" ) allObjFluents
            . crnl
          )
        -- the effect (new values) of the fluents
        . if Map.null effAssignedFluents then id else (
              ps "\t{ Effect: new values for fluents }\n"
            . showEff
          )

    -- consume all predicates and fluents
    -- this is the condition that this transition only fires if the predicates/fluents have the same values as the corresponding variables
    consume =
          ps "CONSUME\n"
        . foldsIntersperse (ps ",\n") id (consume_pred ++ consume_fluents)
        . ps "\n;\n"
      where
        consume_pred = map (\a -> ps "\t" . getPlaceNameAtom luts a . ps ": x" . getPlaceNameAtom luts a) allPredicates
        consume_fluents = map (\fl -> ps "\t" . getPlaceNameFluent luts fl . ps ": x" . getPlaceNameFluent luts fl) allFluents


    -- set or re-set values
    produce =
          ps "PRODUCE\n"
        . foldsIntersperse (ps ",\n") id (prod_newPred ++ prod_newFluents ++ prod_oldPred ++ prod_oldFluents)
        . ps "\n;\n"
      where
        prod_newPred = map (\(Literal b a) -> ps "\t" . getPlaceNameAtom luts a . ps ": " . ps (if b then "true()" else "false()")) effLiterals
        prod_newFluents = map (\f -> ps "\t" . getPlaceNameFluent luts f . ps ": " . ps "n" . getPlaceNameFluent luts f) (Map.keys effAssignedFluents)
        prod_oldPred = map (\a -> ps "\t" . getPlaceNameAtom luts a . ps ": " . ps "x" . getPlaceNameAtom luts a) (prePredicates List.\\ effPredicates)
        prod_oldFluents = map (\f -> ps "\t" . getPlaceNameFluent luts f . ps ": " . ps "x" . getPlaceNameFluent luts f) ((List.nub $ preFluents ++ effFluents) List.\\ Map.keys effAssignedFluents)
  in
    name . variables . guard . consume . produce


-- Generate formulas to check consistency of all places

-- map from literal to low-level place name
llPlaceMapAtoms :: Luts -> Map Atom (Map Bool ShowS)
llPlaceMapAtoms luts@(Luts{luAtom=uniAtoms}) = foldr ins Map.empty (Map.keys uniAtoms)
  where ins a = Map.insert a $ Map.fromList [(False, getPlaceNameAtom luts a . ps ".FALSE"), (True, getPlaceNameAtom luts a . ps ".TRUE")]

-- map from fluent and its value to low-level place name
llPlaceMapFluents :: Luts -> Map Term (Map Constval ShowS)
llPlaceMapFluents luts@(Luts{luFlTypes=uniFluents, luObjFlMap=objFlMap}) = Map.foldrWithKey insFluent Map.empty uniFluents
  where
    values (Type_Object s)    = let m = objFlMap `lukup` s in zip (map fst . List.sortBy (\a b -> compare (fst a) (fst b)) . Map.toList $ m) [1 .. Map.size m]
    values (Type_Num from to) = zip (map Intconst [from .. to]) [0 .. to - from]
    values Type_NativeNum     = error $ "LoLA cannot efficiently handle NativeNums"
    values Type_Float         = error $ "LoLA does not support floats"
    insFluent fl flType = Map.insert fl $ foldr (\(c, i) -> Map.insert c (getPlaceNameFluent luts fl . ps "." . shows i)) Map.empty (values flType)

-- generate for each atom/fluent a line checking that exactly one place is set
-- LoLA can most efficiently check individual formulas, because the state spaces are much smaller than for one giant formula
-- Each line is supposed to be read by a shell script and called with --formula="AG <formula>"
genFormulaConsistency luts =
  let
    llAtoms = llPlaceMapAtoms luts
    llFluents = llPlaceMapFluents luts

    consistentPlaces :: Map key ShowS -> ShowS
    consistentPlaces placeMap = foldsIntersperse (ps " + ") id (Map.elems placeMap) . ps " = 1\n"
  in
      folds consistentPlaces (Map.elems llAtoms)
    . folds consistentPlaces (Map.elems llFluents)

-- Generate the formulas for the goal states

{- currently not implemented

-- generate the goal formulas for one goal state
generateGoalFormulas :: Symbol -> DNF -> ShowS
generateGoalFormulas goal dnf =
  let
    -- formulas must be over low-level places, e.g. place pf_fluent-a : [0,2] has three places pf_fluent-a.0 ... pf_fluent-a.2
    -- to compare fluents, we need to compare all low-level places, e.g. pf_fluent-a.0 = pf_fluent-b.0 etc.
    -- For numeric fluents with different (but overlapping) domains, the intersection must be equal and all others must be false
    -- A similar conversion must be done for object-fluents
    formula = 
  in
      ps "{ Goal formula for goal " . shows goal . ps "}\n"
    . formula


-- OLD CODE


generateTask :: FormulaDNF -> String
generateTask [c] = generateTask2 c
generateTask (c:cs) =
  let f1 x y = x ++ "\n    OR " ++ y
      f2 x = "(" ++ x ++ ")"
      f3 = f2 . generateTask2
  in foldl f1 (f3 c) $ map f3 cs

generateTask2 :: [DNFLiteral] -> String
generateTask2 [l] = "          " ++ generateTask3 l
generateTask2 (l:ls) =
  let f1 x y = x ++ "\n      AND " ++ y
      f2 = generateTask3
  in foldl f1 ("          " ++ f2 l) $ map f2 ls

generateTask3 :: DNFLiteral -> String
generateTask3 (b, name) =
  let pn = getPlaceName name
      extension = if b then ".TRUE" else ".FALSE"
  in pn ++ extension ++ " = 1"

lola2Task :: String -> String
lola2Task xs = "FORMULA\n  EXPATH EVENTUALLY (\n" ++ xs ++ "\n);"


-}
