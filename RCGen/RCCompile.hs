{-# LANGUAGE
    TypeSynonymInstances, CPP #-}
module RCCompile (CompileResult(..), TypeMap(..),compile,goalInfo) where

#if !MIN_VERSION_base(4,6,0)
-- catch has been marked deprecated in ghc 7.2/base-4.4.0 and removed in ghc 7.6/base-4.6.0
import Prelude hiding (catch) -- remove old version of catch in favor of Control.Exception
#endif

import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Map.Strict as Map
import System.IO.Unsafe
import System.IO
import Control.Exception
import Control.Arrow

import Util
import RCData
import RCCompileUtil


-- *Building the TypeMap



-- CK: lookup in a map containing lists as values. In case the key is not found, the empty list is returned.
dassoc name map = case lookup name map of
                      Just v -> v
                      Nothing -> []

-- CK: lookup in a TypeMap (mapping of types to list of constants). In case the key is not found, an error is returned.
tassoc (Type_Object name) map = case lookup name (tmData map) of
                                    Just v -> v
                                    Nothing -> error $ "eassoc: undefined at "++show name
tassoc (Type_Num from to) _   = map Intconst [from..to]
tassoc t _                    = error $ "Type " ++ show t ++ " not supported in variable definitions"
-- :: buildType

-- CK: collects all constants of type 'tname'. The set of constants is defined recursively by including the constants directly defined as type 'tname' and the union of all constants of all (direct) subtypes of 'tname'.
--     'tdefs' contains the type hierarchy information (mapping type->subtypes)
--     'tmap' is the mapping of all types to their respective constants. Note that this is the map this function is ought to fill. Due to lazy evaluation and the (hopefully) acyclicity of the type hierarchy, the vales for all subtypes can be used safely.
--     'typesets' is the mapping of types to constants directly defined to be of this type.

buildType tname tdefs tmap typesets =
    let direct   = dassoc tname typesets
        indirect = concat [ dassoc subtype tmap | subtype <- dassoc tname tdefs ]
    in nub (direct++indirect)


-- CK: builds map of all found types (decratations and uses, e.g. in constants or as subtypes of other types) to a list of all object of this type.
--     an object will appear in the list of every type it belongs to, directly or transitively by inheritance (due to the recursive self-definition of tmap).
--     Note that not every type appearing in the domain will be in the mapping (e.g. types used in predicate parameters but nowhere else).
--     Also note that cyclic type inheritance causes the program to crash as the list of objects will get infinitely long

buildTypeMap dom prob =
    let (tdefs,_) = typedefs dom
        alltypes  = delete objectSymbol . nub . concat $ [tn:tc | (tn,tc) <- tdefs]
        objdefs   = constants dom ++ objects prob
        allobjs   = nub . concat . map snd $ objdefs
        objdefs'  = (objectSymbol,allobjs):objdefs
        objtypes  = nub . map fst $ objdefs'
        typesets  = [(tn, (nub . concat) [d | (tn',d) <- objdefs', tn==tn']) | tn <- objtypes]
        tmap      = [(tn,buildType tn tdefs tmap typesets) | tn <- union alltypes objtypes]
        tmapClean = map (\(tn,os) -> (tn,clean tn os)) tmap
    in TypeMap tmapClean
    where
    -- clean implicitly checks for cyclic definitions, relying on the fact
    -- that GHC is able to detect loops in terms. This, together with the use
    -- of unsafePerformIO, is rather hairy.
    clean tn os = unsafePerformIO (catch (lengthOK os) (lengthError tn))
    lengthOK os = if length os < 1 -- this really has only the purpose to uncover a potential loop
                     then error "Empty type"
                     else return os
    lengthError tn e = do let err = show (e :: SomeException)
                          hPutStrLn stderr ("*** Bogus type "++show tn++", error: "++err)
                          return []

typedefs dom =
    let tdefs  = (objectSymbol,[]) : types dom
        tnames = nub . map fst $ tdefs
        tdefs' = [ (tn, (nub . concat) [d | (tn',d) <- tdefs, tn==tn']) | tn <- tnames]
        alltypes  = nub . concat $ [tn:tc | (tn,tc) <- tdefs']
    in (tdefs',alltypes)
-- * Instantiating Formulae

-- CK: returns a list of all possible combinations of selecting a constant for each of the given types in tns
--     e.g. if tns contains [foo,bar] and (1 2 - foo  3 4 - bar), then it returns [[1,3],[1,4],[2,3],[2,4]]

bindings tmap tns = sequence [tassoc n tmap | n <- tns]

getLandmarks :: Formula -> [Atom]
getLandmarks f = startlm f
                    where
                    startlm f = case f of
                        FAtom a ts -> [(Atom a ts)]
                        FAnd fs ->  foldr (++) [] $ map getLandmarks fs
                        _          -> error $ "getLandmarks: unexpected landmarks " ++ show f

getSpecificity :: [Symbol] -> Formula -> Int
getSpecificity vars f = inst f
  where env = zip vars vars -- vals
        instAtomic (Constant{}) = 1
        instAtomic (Variable v) =  case lookup v env of
                                        Just _  -> 1
                                        Nothing -> 99999
        instAtomic e@(PrevFunVar{}) = error $ "Cannot have previous-instance variable " ++ show e ++ " in any formula except :non-repeating" -- getSpecificity is only called for :precondition formulas

        instTerm (Atomic a) = (instAtomic a)
        instTerm (Function _ as) = instTerms as
        instTerm (FFun c)   = funcallapply (\t1 t2 -> instTerms [t1, t2]) c

        instTerms [] = 0
        instTerms (t:ts) = instTerm t + instTerms ts

        inst f = case f of
                 FAtom _ ts   -> instTerms ts
                 FAnd fs      -> foldr (+) 0 (map inst fs)
                 FOr  fs      -> foldr (+) 0 (map inst fs)
                 FNot fs      -> inst fs
                 FImply a b   -> inst a + inst b
                 FIff a _     -> inst a -- + (inst b)
                 FForall d f' -> subinst d env f'
                 FExists d f' -> subinst d env f'
                 FLitForm f   -> instl f
                 FIsType a _  -> instTerm a
        instl l = litfapply (\t1 t2 -> instTerms [t1, t2]) l

        subinst d e f = let (vals',_) = unzip [va | va@(var,_) <- e, not $ var `elem` (declvars d)]
                        in getSpecificity vals' f

----------------------------------------------------------------------

-- CK: unfolds variable declarations (map of types to array of variables) to an array [(<typename>,<variablename>)]

splitDecl :: VDecls -> ([Type],[Symbol])
splitDecl (VDecls dcs) = unzip (spl dcs)
     where spl [] = []
           spl ((tn,vns):dcs) = zip (repeat tn) vns ++ spl dcs


-- CK: ground instantiate the given Action. Collect all free variables in the action ('fv') and check whether all of them are declared as a parameter.
--     Then consider every possible assignment of the variables to values of their respective types and create the corresponding GAction.
--     A GAction is filtered if it is useless (unsatisfiable precondition or no effect).
-- resolveCondEff: if true, resolve conditional actions

instantiateOp :: Bool -> EvalEnv -> Action -> [GAction]
instantiateOp resolveCondEff ee a =
  let fvpre           = freevars (apre a)
      fveff           = freevars (aeff a)
      fvcbk           = freevars (acbk a)
      fvagt           = freevars (aagnt a)
      fvdur           = freevars (adur a)
      fvsal           = freevars (asal a)
      fv              = fvpre `union` fveff `union` fvcbk `union` fvagt `union`fvdur `union` fvsal
      (types,vars)    = case adecl a of {Just ds -> splitDecl ds; Nothing -> ([],[]) }
      undeclared      = fv \\ vars
      -- bindings must be indexed for GAction bindingIndex (non-repeating actions)
      -- Here, bindingIndices for different Action schemes are considered independent
      -- Unifying all bindingIndices is done in instantiateOps, after all GActions are generated
      -- note: all GActions that belong to the conditional effect get the same bindingIndex here
      indexedBindings = zip (bindings (eeTypes ee) types) [0..]
  in if null undeclared
    then if not resolveCondEff
       then if null types
             then [mkGAction [Symconst (asym a)] a sal agt dur pre eff cbk spec (airr a) 0 cantFollowIndices
                  | let pre = maybeNormP (apre a)
                  , not (isFalse pre)
                  , let cantFollowIndices = maybeNormPFF (apff a)
                  , let eff = maybeNormE (aeff a)
                  , let cbk = maybeNormE (acbk a)
                  , let agt = liftM (theConst . theAtom) (aagnt a)
                  , let dur = liftM (zapFunc ee) (adur a)
                  , let sal = liftM (zapFunc ee) (asal a)
                  , let spec = 1
                  , doesSomething eff || airr a]
             else [mkGAction (Symconst (asym a) : b) a sal agt dur pre eff cbk spec (airr a) i cantFollowIndices
                  | (b,i) <- indexedBindings
                  , let env = zip vars b
                  , let pre = maybeInstP vars (apre a) b
                  , not (isFalse pre)
                  , let cantFollowIndices = maybeInstPFF indexedBindings vars (apff a) (b,i)
                  , let eff = maybeInstE vars (aeff a) b
                  , let cbk = maybeInstE vars (acbk a) b
                  , let agt = liftM (theConst . theAtom . instantiate env) (aagnt a)
                  , let dur = liftM (zapFunc ee . instantiate env) (adur a)
                  , let sal = liftM (zapFunc ee . instantiate env) (asal a)
                  , let spec = getSpec vars (apre a) -- (eeTypes ee)
                  , doesSomething eff || airr a]
       else if null types -- resolve conditional effects
             then [mkGAction [Symconst (asym a)] a sal agt dur pre eff cbk spec (airr a) 0 cantFollowIndices
                  | let cpre = maybeNormP (apre a)
                  , not (isFalse cpre)
                  , let ceffs = maybeNormE (aeff a)

                  , (c, es) <- resolveCEffects $ effs ceffs
                  , let pre = combineConditions [cpre, c] []
                  , not (isFalse pre)
                  , let cantFollowIndices = maybeNormPFF (apff a)
                  , let eff = MkENF . simplifyEffects ee $ map Unconditional es

                  , let cbk = maybeNormE (acbk a)
                  , let agt = liftM (theConst . theAtom) (aagnt a)
                  , let dur = liftM (zapFunc ee) (adur a)
                  , let sal = liftM (zapFunc ee) (asal a)
                  , let spec = 1
                  , doesSomething eff || airr a]
             else [mkGAction (Symconst (asym a) : b) a sal agt dur pre eff cbk spec (airr a) i cantFollowIndices
                  | (b,i) <- indexedBindings
                  , let env = zip vars b
                  , let cpre = maybeInstP vars (apre a) b
                  , not (isFalse cpre)
                  , let ceffs = maybeInstE vars (aeff a) b

                  , (c, es) <- resolveCEffects $ effs ceffs
                  , let pre = combineConditions [cpre, c] []
                  , not (isFalse pre)
                  , let cantFollowIndices = maybeInstPFF indexedBindings vars (apff a) (b,i)
                  , let eff = MkENF . simplifyEffects ee $ map Unconditional es

                  , let cbk = maybeInstE vars (acbk a) b
                  , let agt = liftM (theConst . theAtom . instantiate env) (aagnt a)
                  , let dur = liftM (zapFunc ee . instantiate env) (adur a)
                  , let sal = liftM (zapFunc ee . instantiate env) (asal a)
                  , let spec = getSpec vars (apre a) -- (eeTypes ee)
                  , doesSomething eff || airr a]
    else error $ "undeclared variables: "++show undeclared
  where maybeInstP _ Nothing _  = trueDNF
        maybeInstP v (Just f) b = normalize ee $ instantiate (zip v b) f
        maybeNormP Nothing = trueDNF
        maybeNormP (Just f) = normalize ee f
        maybeInstE _ Nothing _ = noEffect
        maybeInstE v (Just f) b = normalizeEffect ee $ instantiate (zip v b) f
        maybeNormE Nothing = noEffect
        maybeNormE (Just f) = normalizeEffect ee f

        maybeNormPFF :: Maybe (Maybe Formula) -> [Int]
        maybeNormPFF Nothing = []
        maybeNormPFF (Just Nothing) = [0] -- :non-repeating -> action is not allowed to repeat
        maybeNormPFF (Just (Just _)) = error "Cannot use formula with :non-repeating for actions without parameters"
        -- calculate the list of GActions (from the same action scheme) after this action is not allowed
        -- maybeInstPFF :: ibs -> vars -> pff -> (b, i) -> [i]
        -- ibs = all possible variable bindings for this action scheme
        -- vars = all parameters of this action scheme ; pff = :non-repeating formula
        -- (b, i) = binding and bindingIndex for the current GAction
        maybeInstPFF :: [([Constval],Int)] -> [Symbol] -> Maybe (Maybe Formula) -> ([Constval], Int) -> [Int]
        maybeInstPFF _ _ Nothing _ = [] -- no :non-repeating flag: this GAction is allowed after any action
        maybeInstPFF _ _ (Just Nothing) (_,i) = [i] -- :non-repeating without formula: this action must not follow itself
        maybeInstPFF ibs vars (Just (Just pff)) (b,_) = map snd . filter ((==trueDNF).fst) $ -- all indices of GActions where the action must not follow that GAction
                                             map (first $ \bi -> normalize ee $ instantiatepff (zip3 vars b bi) pff) ibs -- for every GAction of the scheme, compute value for the prevFunFormula

        getSpec _ Nothing = 0
        getSpec v (Just f) = getSpecificity v f

        combineConditions cs ncs = normalize ee . FAnd $ map dnf2formula cs ++ map (FNot . dnf2formula) ncs
        -- The workhorse function for resolving conditional effects and removing nested conditional effects
        -- It takes a list of CEffects, and returns all possible combinations of conditional effects (but making them unconditional) with the required pre-conditions
        -- When given a list of n conditional effects, it returns a list of up to 2^n combinations, each containing all unconditional effects and the appropriate conditional effects
        resolveCEffects :: [CEffect] -> [(DNF, [Effect])]
        resolveCEffects es = fst . until (null.snd) nextEff . initSubset $ ceffsKV
            where
            (uceffs, ceffs) = splitEffs es
            -- recurse on all conditional effects (KV = Key/Value, Key = DNF, Value = Effect)
            -- use concat to collect all KVs in one big list to choose from
            ceffsKV = concatMap expandCEffs ceffs
            splitEffs effs = partition (\eff -> case eff of Unconditional _ -> True; _ -> False) effs
            -- recurse on all sub-effects of a conditional effect, and add the condition c to all effects
            expandCEffs (Conditional c es) = map (\(sc, ses) -> (combineConditions [c, sc] [], ses)) $ resolveCEffects es
            expandCEffs (Unconditional _)  = undefined

            nextEff (effs, stack) =
                            let
                                act_ceffsKV = currentSet stack
                                act_DNFs = map fst act_ceffsKV
                                inact_ceffsKV = ceffsKV \\ act_ceffsKV
                                inact_DNFs = map fst inact_ceffsKV
                                act_DNF = combineConditions act_DNFs []

                                c = combineConditions act_DNFs inact_DNFs
                                es = map thePeff uceffs ++ concatMap snd act_ceffsKV
                            in
                                -- In the following backtracking heuristic, it is assumed that active conditional effects more
                                -- likely contradict each other than inactive conditional effects.
                                -- Therefore, if the conjunction of all active conditions is statically false,
                                -- it will also be false for any superset of active conditional effects.
                                -- An efficient way of expanding conditional effects is therefore:
                                -- * Generate active conditional effects in "inclusion order", if possible always generating supersets
                                -- * As soon as the conjunction of conditions do not hold, stop generating supersets and backtrack
                                -- * If the overall condition is not statically false, add the resulting combined effect; otherwise, continue
                                if all (not.isFalse) act_DNFs && (not.isFalse) act_DNF
                                then (if (not.isFalse) c then (c, es):effs else effs, nextSubset stack) -- do not backtrack
                                else (effs, nextSubset . backtrack $ stack) -- backtrack
            -- powerset functions
            backtrack = tail
            currentSet = fst . head
            -- Function for generating all elements of a powerset in inclusion order.
            -- Given a stack of unprocessed sets (subset, unprocessed elements) , this function returns the next subset and an updated stack to be passed again
            initSubset xs = ([], [([], xs)])
            nextSubset :: Show a => [([a], [a])] -> [([a], [a])]
            nextSubset [] = []
            nextSubset ((_, []):ss) = nextSubset ss -- the current set cannot be expanded any more, so just backtrack
            nextSubset ((xs, nx:nxs):ss) = let
                                            xs' = nx:xs     -- generate next superset of xs
                                            last = (xs, nxs) -- update backtracking information: do not use nx again, continue with nxs
                                            cur = (xs', nxs) -- save current element, so the next iteration will increase the current subset even more
                                           in if not.null $ nxs
                                                then cur:last:ss -- More supersets can be generated, "last" keeps the information where to continue
                                                else cur:ss      -- No more supersets possible, in the next iteration: backtrack


-- compute the specificity for each predicate of the universe
-- count the occurencies of each atom in eet to get the depth in the type hierarchie
-- returns a map with specificities


-- specificity (act-r heuristic)

-- literals
increaseConstSpecs specs s = let f x = Just (x+1)
                             in case Map.lookup s specs of
                                Nothing -> Map.insert s 1 specs -- new one
                                Just _ -> Map.update f s specs -- increase a

increaseSpec specs [] = specs
increaseSpec specs (a:as) = increaseSpec (increaseConstSpecs specs a) as

-- only all prelits of each GA are needed
specCompute [] specs= specs
specCompute (g:gs) specs = increaseSpec (specCompute gs specs) (prelits g)


-- end specificity


{-
specOp :: EvalEnv -> Action -> [GAction]
specOp ee a =
  let fvpre        = freevars (apre a)
      fveff        = freevars (aeff a)
      fvcbk        = freevars (acbk a)
      fvagt        = case (aagnt a) of {Nothing -> []; Just a -> fvExpr a}
      fvdur        = case (adur a) of {Nothing -> []; Just a -> fvAtom a}
      fvsal        = case (asal a) of {Nothing -> []; Just a -> fvExpr a}
      fv           = fvpre `union` fveff `union` fvcbk `union` fvagt `union`fvdur `union` fvsal
      (types,vars) = case adecl a of {Just ds -> splitDecl ds; Nothing -> ([],[]) }
      tvl          = [tv | tv@(_,v) <- zip types vars, v `elem` fv]
      undeclared   = fv \\ vars
  in if null undeclared
     then [mkGAction [Symconst (asym a)] sal agt dur pre eff cbk spec
                  | let pre = maybeNormP (apre a)
                  , let eff = maybeNormE (aeff a)
                  , let cbk = maybeNormE (acbk a)
                  , let agt = Nothing
                  , let dur = Nothing
                  , let sal = Nothing
                  , let spec = 1
                  , not(isFalse pre)
                  , doesSomething eff]
     else error $ "undeclared variables: "++show undeclared
  where maybeInstP _ Nothing _  = trueDNF
        maybeInstP v (Just f) b = normalize ee $ instantiate v b f
        maybeNormP Nothing = trueDNF
        maybeNormP (Just f) = normalize ee f
        maybeInstE _ Nothing _ = noEffect
        maybeInstE v (Just f) b = normalizeEffect ee $ instantiate v b f
        maybeNormE Nothing = noEffect
        maybeNormE (Just f) = normalizeEffect ee f
        getSpec _ Nothing = 0
        getSpec v (Just f) = getSpecificity v f

specOps ee = foldr (\a -> (specOp ee a ++)) []
-}

instantiateOps :: Bool -> EvalEnv -> [Action] -> [GAction]
instantiateOps resolveCondEff ee = fst . foldr sumOps ([],0) where
    sumOps a (prev,iOffset) = let ops = instantiateOp resolveCondEff ee a -- all GActions of action scheme a
                                  (ops', nextbi) = cleanBindingIndicesOff iOffset ops
                              in (ops' ++ prev, nextbi)

-- cleans unused binding indices, starting at a free binding index "offset"
-- due to ruling out irrelevant or impossible actions, some binding indices may be unused
-- generating these can be inefficient and may lead to potential problems (eg references in cantOccurAfter that are no longer valid)
-- returns adjusted GActions and the next free offset
cleanBindingIndicesOff :: Int -> [GAction] -> ([GAction], Int)
cleanBindingIndicesOff offset [] = ([], offset)
cleanBindingIndicesOff offset ops = (map adjustOp ops, (1+) $ snd.head $ bimap)
  where
    makeBis :: GAction -> [(Int, Int)] -> [(Int, Int)]
    makeBis ga [] = [(bindingIndex ga, offset)]
    makeBis ga zips@((_, nbis):_) | isJust (bindingIndex ga `lookup` zips) = zips -- bindingIndices may not be guaranteed to be increasing (actions are sorted at some point), so using `lookup` is required here
                                  | otherwise = (bindingIndex ga, nbis+1):zips
    bimap = foldr makeBis [] ops
    -- remove any binding indices in cantOccurAfter that refer to non-existent GActions, and update bindingIndices to new indices of bimap
    -- also update bindingIndex of this action according to bimap
    adjustOp ga@GAction{cantOccurAfter=coa,bindingIndex=i} = ga{cantOccurAfter=mapMaybe (`lookup` bimap) coa, bindingIndex=fromJust (lookup i bimap)}

-- wrapper function for cleanBindingIndicesOff, starting always at offset 0
cleanBindingIndices :: [GAction] -> [GAction]
cleanBindingIndices = fst . cleanBindingIndicesOff 0

----------------------------------------------------------------------
-- * Compiling a Domain

-- CK: structure containing information about the 'compile' process and its result, which is returned by the 'compile' function.
data CompileResult = CompileResult {
                          dpid :: (Symbol,Symbol),
                          staticLiterals :: [Literal],
                          initialStates :: Map.Map Symbol ([Literal], Map.Map Term Constval), -- Map initial state -> (literals, fluents)
                          initialDuration :: Maybe Atom,
                          achievableLiterals :: Map.Map Symbol [Literal], -- for each initial state, the literals achievable from this state
                          groundOps :: [GAction],
                          goalDNFs :: Map.Map Symbol DNF,
                          goalInitials :: Map.Map (Symbol,Symbol) Ternary, -- truth value of goals in all initial/goal state combinations -- if TernaryUndecided, an error has occured (we're evaluating in closed world!)
                          goalFinals :: Map.Map (Symbol, Symbol) Ternary, -- for the initial/goal state combinations, if the goals are achievable
                          relvops :: [GAction],   -- Actions relevant to goal
                          possops :: [GAction],   -- Actions possible given init (subset of 'relops')
                          universe :: [Atom],     -- Herbrand universe given 'possops'
                          univFluents :: Map.Map Term Type, -- All declared fluents: Map from instantiated fluent → type of fluent
                          observe :: ENF,          -- observation
                          typeNames :: [Symbol],  -- list of existing types
                          typeHierarchy :: [(Symbol,[Symbol])], -- map type => children
                          specLut :: Map.Map Literal Int, -- the specification value for the act-r heuristic
                          userLandmarks :: [Atom],
                          tmap :: TypeMap
                     }

-- CK: gives summarized goal achievability information (ShowS)
goalInfo cr = let (dnfTrue, dnfFalse) = Map.foldr (\g (t, f) -> if isFalse g then (t, f+1) else if isTrue g then (t+1, f) else (t, f)) (0, 0) (goalDNFs cr)
                  initTrue = Map.foldr (\gi t -> if gi == TernaryTrue then t+1 else t) 0 (goalInitials cr)
                  goalPoss = Map.foldr (\gi t -> if gi == TernaryTrue then t+1 else t) 0 (goalFinals cr)
                  nGoalInits = Map.size (goalFinals cr)
                  nGoals = Map.size (goalDNFs cr)
              in  (if dnfTrue + dnfFalse <= 0 then id else ps ";;;;==> Out of " . shows nGoals . ps " goals, " . shows dnfTrue . ps " are tautological TRUE, " . shows dnfFalse . ps " are contradictory FALSE\n" )
                . (if initTrue <= 0 then id else ps ";;;;==> Out of " . shows nGoalInits . ps " initial/goal state combinations, " . shows initTrue . ps " are TRUE in the initial state\n")
                . (if goalPoss >= nGoalInits then ps ";;;;==> All goals are achievable\n" else ps ";;;;==> Out of " . shows nGoalInits . ps " initial/goal state combinations, " . shows (nGoalInits - goalPoss) . ps " are not achievable\n")


instance Show CompileResult where
  showsPrec _ cr = ps "(:compilerInfo ; BEGIN COMPILER INFO\n"
                   . infocomment 0 "Relevant Static Literals"
                   . shows (staticLiterals cr)
                   . infocomment 2 "Initial Duration"
                   . case initialDuration cr of {Nothing -> ps ":immediate"; Just a -> shows a}
                   . infocomment 2 "Actions in DNF"
                   . shows (groundOps cr)
                   . ps "\n;;;; " . shows (length (groundOps cr)) . ps " actions"
                   . infocomment 2 "Achievable literals"
                   . shows (achievableLiterals cr)
                   . infocomment 2 "Goal Expression"
                   . Map.foldrWithKey (\name dnf rest -> shows name . ps ": " . shows dnf . ('\n':) . rest) id (goalDNFs cr)
                   . ('\n':)
                   . goalInfo cr
                   . infocomment 2 "Relevant actions"
                   . ('(':) . shows (relvops cr) . (')':)
                   . ps "\n;;;; " . shows (length (relvops cr)) . ps " actions"
                   . infocomment 2 "Possible actions"
                   . ('(':) . shows (possops cr) . (')':)
                   . ps "\n;;;; " . shows (length (possops cr)) . ps " actions"
                   . infocomment 2 "Universe"
                   . shows (universe cr)
                   . ps "\n;;;; " . shows (length (universe cr)) . ps " booleans\n"
                   . shows (univFluents cr)
                   . ps "\n;;;; " . shows (Map.size (univFluents cr)) . ps " fluents"
                   . infocomment 2 "Relevant Initial Literals and Fluents"
                   . Map.foldrWithKey (\name istate -> (.) $ shows name . ps ": " . shows (fst istate) . shows (snd istate) . ('\n':) ) id (initialStates cr)
                   . infocomment 2 "Observations"
                   . shows (observe cr)
                   . ps "\n) ; END COMPILER INFO"


--------- CK: durative actions resolvation

-- creates new Symbol. Unlike the ones created while parsing, this Symbol is not identified by its index number, but by its name.
toSymbol :: String -> Symbol
toSymbol s = Sym{index= -1,token=TEOF,name=s}

-- data structure containing information about a protection predicate. This predicates are created during the resolvation of a durative action and ensure a predicate defined to be true throughout the duration of the durative-action is not altered.
data ProtectionPredicate = ProtectionPredicate {
                            ppSym :: Symbol,             -- symbol of protection predicate

                            ppactionSym :: Symbol,       -- symbol of action it was created from
                            ppactionVars :: VDecls,      -- variables of the action it was created from

                            pppredSym        :: Symbol,  -- symbol of predicate it was created from
                            pppredNegated    :: Bool,    -- true, if the literal was negative
                            pppredParamTypes :: [Type],-- parameter types of the predicate it was created from (currently unused)
                            ppendActionSymbol :: Symbol  -- symbol of the end action corresponding to this protection predicate. Protection predicates won't be applied to their own end action
                           } deriving Show

-- data structure containing information about a mutex predicate. This predicates are created during the resolvation of a durative action and ensure that the ending action is used only after the corresponding begin action and that all durative-actions are finished by the end of the planning process.
data MutexPredicate = MutexPredicate {
                       mpSym :: Symbol,       -- symbol of mutex predicate
                       mpactionSym :: Symbol, -- symbol of action it was created from
                       mpactionVars :: VDecls -- variables of the action it was created from
                      } deriving Show

-- data structure holding all information about the durative protection and mutex predicates created during the resolvation process
data DurativeProtectionPredicates = DurativeProtectionPredicates {
                                     protectionPredicates :: [ProtectionPredicate],
                                     mutexPredicates :: [MutexPredicate],
                                     agentBusySym :: Symbol
                                    } deriving Show

-- splits Actions in start actions, end actions, and generates additional protection predicates that have to be integrated in a later step
resolveDurativeActions :: EvalEnv -> [PDecl] -> [Action] -> ([Action],[Action],DurativeProtectionPredicates)
resolveDurativeActions ee pdecls as = let busySym = toSymbol "agent-busy"
                                          (startActions,endActions,protectionPreds,mutexPreds) = unzip4$map rda as
                               in (concat startActions,concat endActions,DurativeProtectionPredicates {
                                                   protectionPredicates=concat protectionPreds,
                                                   mutexPredicates=concat mutexPreds,
                                                   agentBusySym=busySym})
 where rda :: Action -> ([Action],[Action],[ProtectionPredicate],[MutexPredicate])
       rda (a@Action{}) = ([a],[],[],[])
       rda (a@DurativeAction{}) =
        let (startConds,endConds,overallConds) = splitTimedFormula (dapre a)
            (startEffs,endEffs,[]) = splitTimedFormula (daeff a)
            overallDNFs = map (normalize ee) overallConds
            overallLits = concatMap (\l -> case l of (MkDNF [x]) -> x
                                                     (MkDNF [])  -> [] -- false DNF; handled later (results in returning ([],[],[],[]))
                                                     l           -> error $ "resolveDurativeActions: over-all conditions of action "++(show (asym a))++" are only allowed to contain conjunctions of literals. The given conditions "++(show overallConds)++" reduce (in parts) to the violating DNF "++(show l)
                                    ) $ overallDNFs
            actionSym = asym a
            actionVars = if isNothing $ adecl a then VDecls [] else fromJust $ adecl a
            protPreds = map (\x -> let (negated,predSym,predParams) = (\(Literal n (Atom s p)) -> (not n,s,p)) x
                                   in ProtectionPredicate {
                                       ppSym              = toSymbol $ "protect-"++(name actionSym)++(if negated then "-not-" else "-")++(name predSym),
                                       ppactionSym        = actionSym,
                                       ppactionVars       = actionVars,
                                       pppredSym          = predSym,
                                       pppredNegated      = negated,
                                       pppredParamTypes   = case lookup predSym $ map (\(PDecl s p) -> (s,p)) pdecls of
                                                             Nothing -> replicate (length predParams) (Type_Object objectSymbol)
                                                             Just vars -> fst.splitDecl $ vars,
                                       ppendActionSymbol  = undefined -- unknown by now, set it later
                                      }) overallLits
            mutexPred = MutexPredicate{
                         mpSym        = toSymbol $ "mutex-"++(name actionSym),
                         mpactionSym  = actionSym,
                         mpactionVars = actionVars
                        }
            actionTerms = vDeclsToTerms actionVars
            mutexAtom = FAtom (mpSym mutexPred) actionTerms
            mutexEAtom = EFAtom (mpSym mutexPred) actionTerms
            agentBusyAtoms = if (isNothing (aagnt a)) then [] else [EFAtom (toSymbol "agent-busy") actionTerms]
            protectionAtoms = map (\((Literal _ (Atom _ terms)),pp) -> EFAtom (ppSym pp) (actionTerms++terms)
                                  ) $ zip overallLits protPreds
            startAction = Action {
                           asym  = toSymbol.ps "begin-".name.asym $ a,
                           adecl = adecl a,
                           asal  = asal a,
                           aagnt = aagnt a,
                           adur  = adur a,
                           apre  = Just (FAnd (startConds++overallConds++[FNot mutexAtom])),
                           aeff  = Just (EFAnd (startEffs++[mutexEAtom]++protectionAtoms++agentBusyAtoms)),
                           airr = True,
                           -- grounded begin action can't be repeated - is this correct?
                           apff = Just Nothing,
                           acbk  = acbk a}
            endAction = Action {
                         asym  = toSymbol.ps "end-".name.asym $ a,
                         adecl = adecl a,
                         asal  = asal a,
                         aagnt = aagnt a,
                         adur  = Nothing,
                         apre  = Just (FAnd (endConds++[mutexAtom])),
                         aeff  = Just (EFAnd (endEffs++[EFNot mutexEAtom]++(map EFNot protectionAtoms)++(map EFNot agentBusyAtoms))),
                         airr = True,
                         -- grounded end action can't be repeated, is this correct?
                         apff = Just Nothing,
                         acbk  = acbk a}
            finalProtPreds = map (\pp -> pp{ppendActionSymbol=asym endAction}) protPreds
        in if elem (MkDNF []) overallDNFs
            then ([],[],[],[]) -- precondition not satisfiable
            else ([startAction],[endAction],finalProtPreds,[mutexPred])

-- converts a list of variable declarations to a list of corresponding variable terms
vDeclsToTerms :: VDecls -> [Term]
vDeclsToTerms = map (Atomic . Variable).snd.splitDecl

-- checks if an actions alters protected predicates and adds the corresponding protection predicates if necessary
addProtectionPredicates :: DurativeProtectionPredicates -> Action -> Action
addProtectionPredicates dpp a = let protForms = if isNothing (aeff a) then [] else getEffectLiterals.fromJust.aeff $ a
                                in if null protForms then a else a{apre=if isNothing (apre a) then Just (FAnd protForms) else Just .FAnd .(:protForms).fromJust.apre $ a }
 where
  getEffectLiterals :: EFormula -> [Formula]
  getEffectLiterals (EFNot (EFAtom s t))= getProtectionFormulae $ Literal False (Atom s t)
  getEffectLiterals (EFNot _)           = undefined
  getEffectLiterals (EFAtom s t)        = getProtectionFormulae $ Literal True (Atom s t)
  getEffectLiterals (EFAssign _ _)      = [] -- TODO: protect fluents, too?
  getEffectLiterals (EFAnd as)          = concatMap getEffectLiterals as
  getEffectLiterals (EFWhen _ e)        = getEffectLiterals e
  getEffectLiterals (EFForall d e)      = map (FForall d) $ getEffectLiterals e

  getProtectionFormulae :: Literal -> [Formula]
  getProtectionFormulae (Literal n (Atom s t)) = concatMap (\pp -> let actionVars = addVarPrefix "protection-" (ppactionVars pp) in
                                                                   if asym a /= ppendActionSymbol pp && s == pppredSym pp && n == pppredNegated pp
                                                                   then [FForall actionVars (FAtom (ppSym pp) ((vDeclsToTerms actionVars)++t))]
                                                                   else []
                                                 ) (protectionPredicates dpp)
  getProtectionFormulae (LiteralF _) = error $ "getProtectionFormulae: formulas are not supported"

-- adds given prefix string to all variables declaration names in the list (creating new symbols)
addVarPrefix :: String -> VDecls -> VDecls
addVarPrefix prefix (VDecls vs) = VDecls .map (\(t,ss) -> (t,map (toSymbol.(prefix++).name) ss)) $ vs

-- checks whether an action specifies an agent and adds the "agent busy"-predicate to the preconditions
addBusyAgentPredicate :: DurativeProtectionPredicates -> Action -> Action
addBusyAgentPredicate dpp a = if isNothing (aagnt a)
                              then a
                              else let busyAtom = FNot (FAtom (agentBusySym dpp) [fromJust (aagnt a)])
                                   in if isNothing (apre a)
                                      then a{apre=Just busyAtom}
                                      else a{apre=Just (FAnd [busyAtom,fromJust.apre $ a])}

-- adds all mutex predicates to the goal formula to enusre all durative actions are ended by the end of the planning process
updateGoalFormula :: DurativeProtectionPredicates -> Formula -> Formula
updateGoalFormula dpp f = FAnd .(f:).map (\mp -> let actionVars = addVarPrefix "mutex-" (mpactionVars mp) in
                                      FForall actionVars (FNot (FAtom (mpSym mp) (vDeclsToTerms actionVars)))
                              ).mutexPredicates $ dpp
---------

-- Divide problem init literals in those using dynamic predicates and those using static predicates.
-- Any literal not matching any of these two sets (shouldn't be any) is omitted.
-- Because static literals can be defined differently in initial states, we only return these as truly static that are common to all initial states
-- Other static literals with different truth values are added to the dynamic predicates: these are not changed, but must be actually state predicates set in the initial states
splitInits :: EvalEnv -> [Symbol] -> [Symbol] -> (Map.Map Symbol EFormula) -> ((Map.Map Symbol [Literal]), [Literal])
splitInits eeini stats dyns inits = let
                                -- first split the literals based on the property if they are static/dynamic
                                (slMap,ilMap) = Map.foldrWithKey splitInitMaps (Map.empty, Map.empty) inits
                                -- then find all common static literals
                                sls = Map.foldr combineLits [] slMap
                                -- remove all literals from slMap that are in sls
                                slMap' = Map.mapMaybe (\s -> let s' = s \\ sls in if null s' then Nothing else Just s') slMap
                                -- these literals are not truly static and thus considered as dynamic
                                ilMap' = Map.unionWith (++) ilMap slMap'
                               in (ilMap', sls)
    where
        splitInitMaps :: Symbol -> EFormula -> (Map.Map Symbol [Literal], Map.Map Symbol [Literal]) -> (Map.Map Symbol [Literal], Map.Map Symbol [Literal])
        splitInitMaps name ini (slM, ilM) = let (sl, il) = splitInits' . simplifyAnd eeini{eeClosed=False} . literals . normalizeEffect eeini $ ini
                                            in (Map.insert name sl slM, Map.insert name il ilM)
        splitInits' Nothing = error $ "False / contradictory init formula"
        splitInits' (Just lits) = foldr spl ([],[]) lits
        spl l@(Literal _ (Atom s _)) r@(sl,il) = if s `elem` stats then (l:sl,il)
                                                 else if s `elem` dyns then (sl,l:il)
                                                 else r
        spl _ _ = undefined
        combineLits ls' [] = ls' -- first iteration: use literals as starting point
        combineLits ls' ls = intersect ls' ls -- only the intersection contains truly static literals

-- split the function assignments in the initial states into static functions and fluents
splitFluents :: Problem -> [Symbol] -> Problem
splitFluents prob flPreds = prob{fconsts=consta, initFluents=fluenta, initFuns=undefined} -- initFuns should not be used anymore
  where
    (constMap, fluenta) = Map.mapAccumWithKey splitConsts Map.empty (initFuns prob)
    -- consts: accumulator for static functions (should only be defined in :all, and are extracted during iteration over the map
    splitConsts constMap name m = let (flMap, consts) = Map.partitionWithKey (\(Function s _) _ -> s `elem` flPreds) m
                                  in (Map.insert name consts constMap, flMap)
    -- static function assignments should be identitical over all initial states
    -- check this, and extract this single assignment
    consta = Map.foldrWithKey extractConsts Map.empty constMap
    extractConsts name consts result
        | Map.null result = consts -- first constants, simply take this as a start
        | otherwise = Map.mergeWithKey checkConsts only only consts result -- check if all functions are declared identitically
        where checkConsts k f1 f2 | f1 == f2 = Just f1
                                  | otherwise = error $ "The static function " ++ show k ++ "is defined ambigously as " ++ show f1 ++ " and " ++ show f2 ++ ", involving initial state " ++ show name
              only submap | Map.null submap = submap
                          | otherwise = error $ "Static functions must be defined in all initial states, but the following are only declared in some: " ++ show (Map.toList submap) ++ ", involving initial state " ++ show name

-- check that all fluent instantiations conform to the declared type, returns all fluents
univFluentsCheck dom pops initFls fluentTypes typemap = Map.fromList . map getType $  if null univFluentsBadSig then univFluents' else error $ "Fluent's signature does not conform to declaration: " ++ show univFluentsBadSig
  where
    univFluentsBadSig = filter (not.check) univFluents'
    check :: Term -> Bool
    check fl@(Function f as) = all checkType $ if length typeList == length as then zip typeList as else error $ "Fluent requires different number of arguments: " ++ show fl
        where typeList = f `lukup` vartypeMap
    check t = error $ "Unexpected term " ++ show t ++ " in univFluentsCheck"
    -- a map Fluent name → List of variable types
    vartypeMap :: Map.Map Symbol [Type]
    vartypeMap = Map.fromList $ map (\f -> (flsym f, vartypes $ fldecl f)) $ concatMap snd (fluents dom)
    vartypes (VDecls ds) = concatMap (\(t, vs) -> replicate (length vs) t) ds
    checkType :: (Type, Term) -> Bool
    checkType (_, (Atomic (Variable v)))                = error $ "Cannot use variable " ++ show v ++ " as a parameter of another fluent"
    checkType (_, (Atomic (PrevFunVar v)))              = error $ "Cannot use variable " ++ show v ++ " as a parameter of another fluent"
    checkType (_, p@(Function _ _))                     = error $ "Cannot use fluent " ++ show p ++ " as a parameter of another fluent"
    checkType (_, FFun _)                               = error $ "Cannot use arithmetic operation as a parameter of another fluent"
    checkType (Type_Object t, (Atomic (Constant c)))    = c `elem` (case lookup t typemap of Just x -> x; Nothing -> error $ "Type not found: " ++ show t)
    checkType (Type_Num from to, (Atomic (Constant c))) = case c of
                                                    Symconst c -> error $ "Cannot use symbol " ++ show c ++ " as a parameter"
                                                    Intconst i -> i >= from && i <= to
                                                    Numconst f -> error $ "Cannot use float " ++ show f ++ " as a parameter"
    checkType (Type_NativeNum, _)                       = error $ "Unsupported parameter type"
    checkType (Type_Float, _)                           = error $ "Unsupported parameter type"

    -- All instantiated fluents - fluents are automatically instantied in the instantiateOp function
    univFluents' = sort . nub $ concatMap Map.keys (Map.elems initFls) `union` concatMap efffluents pops
    getType :: Term -> (Term, Type)
    getType fl@(Function s _) = (fl, s `lukup` fluentTypes)
    getType _ = undefined
    lukup s m = case Map.lookup s m of
                    Nothing -> error $ "Undeclared fluent " ++ show s
                    Just t -> t

-- CK: analyzes and converts the input domain/problem pair by resolving static predicates, grounding all actions, checking goal achievability and eliminating impossible/useless actions
compile :: DomainInfo -> Either String CompileResult
compile di =
   do let -- CK: fetching domain information
          dom       = diDomain di -- CK: domain description
          prob''    = diProblem di -- CK: problem decsription
          ap        = diAllPreds di -- CK: all predicates used in domain/problem
          dp        = diDynPreds di -- CK: all dynamic predicates, i.e., predicates the values of which are mutable (appear in action effects)
          weedOut   = diWeedOut di -- FK: weedout parameter from cmdline
          resolveCondEff = diResolveCondEff di

          -- filter out only selected init states / goal formulas
          prob'     = prob''{
                        RCData.inits = filterMap (RCData.inits prob'') (diSelectedInit di),
                        initFuns = filterMap (RCData.initFuns prob'') (diSelectedInit di),
                        goals = filterMap (RCData.goals prob'') (diSelectedGoal di)
                      }
            where
                filterMap m Nothing = m
                filterMap m (Just n) = let m' = Map.filterWithKey (\k _ -> name k == n) m
                                       in if Map.null m' then
                                            error $ "Name " ++ n ++ " not found as initial state / goal"
                                          else m'

          lm        = getLandmarks $ landmarks prob'

          -- CK: build type map (maps the types to a list of all constants it contains)
          tmap      = buildTypeMap dom prob'

          stats     = ap \\ dp -- CK: all static predicates, i.e., predicates the values of which are immutable (don't appear in action effects)
          flpreds   = nub . concatMap (map flsym . snd) $ fluents dom -- all fluent names
          -- Convert the fluent definitions from format [(Type, [(Name, VDecl)])] to a Map Name → Type
          fluentTypes :: Map.Map Symbol Type
          fluentTypes = Map.fromList $ concatMap (\(t, fs) -> map (\fl -> (flsym fl, t)) fs) $ fluents dom

          -- split initial fluent assignments from static function assignments
          prob      = splitFluents prob' flpreds

          -- environment with field set to those most commonly shared
          eebase    = EE{eeTypes=tmap, eeFluents=flpreds, eeFluentTypes=fluentTypes, eePreds=stats, eeLits=[], eeClosed=False, eeSFuncs=fconsts prob}
          -- CK: set initial environment: type mapping and the values of the (static) functions are set (for init statement normalization)
          eeini     = eebase{eePreds=[], eeLits=[], eeClosed=True}

          -- CK: normalize all inits (to a list of literals) and split them up in static literals (those using static predicates) and dynamic literals
          -- slMap/ilMap are both Map.Map Symbol [Literal] and split from the Map.Map Symbol EFormula
          -- slMap are only those static initial literals for individual states -- true static literals are those that are common to all maps (or in the :all initial state)
          (ilMap,sl) = splitInits eeini stats dp (RCData.inits prob)

          -- CK: set new environment: information about static predictes and literals added (for action precondition/effect normalization, elimination of all statics)
          eeops     = eebase{eeLits=sl}

          -- CK: search for free variables in domain/problem statements
          inifv     = fvlist . Map.elems $ RCData.inits prob
          goalfv    = fvlist . Map.elems $ RCData.goals prob
          obsfv     = concatMap freevars (RCData.observation dom)

          -- CK: START: at this point the durative actions are resolved.
          (startOps',endOps',pps) = resolveDurativeActions eeops (predicates dom) (actions dom)
          startOps = map (addBusyAgentPredicate pps.addProtectionPredicates pps) startOps'
          endOps = map (addProtectionPredicates pps) endOps'
          ops = instantiateOps resolveCondEff eeops (startOps++endOps)
          goaldnfs   = Map.map (normalize eeops . updateGoalFormula pps) (RCData.goals prob)
          -- CK: END: at this point the durative actions are resolved.

          -- CK: environment for goal condition check in the initial states: all (dynamic) initial literals and fluents are said to be true (and nothing else).
          eegoals :: Map.Map Symbol EvalEnv -- map from initial state to EvalEnv
          eegoals   = Map.map (\il -> eebase{eeLits=il, eeClosed=True}) ilMap
          -- CK: evaluate goal condition in all initial/goal states (using all static domain information collected so far)
          goalinis :: Map.Map (Symbol, Symbol) Ternary
          goalinis  = Map.fromList [((isname, gsname), eval ee gdnf) | (isname, ee) <- Map.toList eegoals, (gsname, gdnf) <- Map.toList goaldnfs]

          -- set of all achievable literals in domain, separately for the initial states: inits+effects
          -- starting with the (dynamic) literals from init, collect all literals possibly achievable by applying some (grounded) actions
          allLits :: Map.Map Symbol [Literal]
          allLits   = Map.map (\il -> foldr collectAchievables il ops) ilMap

          -- CK: environment for goal state achievability check: all (dynamic) initial literals possibly achievable are said to be true (and nothing else).
          eegoalfinal :: Map.Map Symbol EvalEnv
          eegoalfinal = Map.map (\ls -> eebase{eeLits=ls, eeClosed=True}) allLits
          -- for each initial/goal state, compute if the goal is achievable
          goalfinals :: Map.Map (Symbol, Symbol) Ternary
          goalfinals  = Map.fromList [((isname, gsname), eval ee gdnf) | (isname, ee) <- Map.toList eegoalfinal, (gsname, gdnf) <- Map.toList goaldnfs]

          -- CK: filter actions that are not relevant (don't help with goal achieval) or not possible (can't be used whatsoever)
          -- compute possibe ops for every initial state independently, then unify the lists
          goallits  = Map.foldr (\dnf ls -> nub $ literals dnf ++ ls) [] goaldnfs -- all literals of all goals
          rops      = relevantOps ops goallits
          eepopsM   = Map.map (\ils -> eebase{eeLits=ils, eeClosed=True}) ilMap
          pops      = cleanBindingIndices $ Map.foldr (\eepops possops -> possibleOps (if weedOut then rops else ops) eepops `union` possops) [] eepopsM
          otheratms = nub . map atom $ Map.foldr (nub .: (++)) [] ilMap -- CK: collect all predicates (atoms) mentioned in all initial states
          univ      = sort . foldr uniadd otheratms $ pops -- set of atoms that possibly can be true

          -- All instantiated fluents, e.g. (fl a), (fl b), (fl d), mapped to their type -- not the list of instantiated values assigned to fluents
          univFluents = univFluentsCheck dom pops (initFluents prob) fluentTypes (tmData tmap)

          -- create a combined map from initial state -> (initial literals, initial fluents)
          -- the initial state contains values for *all* literals/fluents, either by explicitly setting them or implicitly (to false/0/first constant)
          initialStates = Map.unionWith
                            -- combine both expanded maps
                            (\(ils1, ifl1) (ils2, ifl2) -> (ils1++ils2, Map.union ifl1 ifl2))
                            -- expand both single maps into the combined structure
                            (Map.map (\ils -> (ils, Map.empty)) ilMapFull)
                            (Map.map (\ifl -> ([], ifl)) initFlMapFull)
            where
                -- fill the initial literals and fluents with values not explicitly mentioned and set them to "0"
                fillLiterals :: [Literal] -> [Literal]
                fillLiterals ls =
                  let
                    initAtoms = map (\(Literal _ a) -> a) ls
                    unsetAtoms = univ \\ initAtoms
                  in ls ++ map (Literal False) unsetAtoms

                ilMapFull :: Map.Map Symbol [Literal]
                ilMapFull = Map.map fillLiterals ilMap

                fillFluents :: Map.Map Term Constval -> Map.Map Term Constval
                fillFluents flMap =
                  let
                    unsetFluents = Map.keys univFluents \\ Map.keys flMap

                    unsetValueT :: Type -> Constval
                    unsetValueT (Type_Object s) = head . fromJust . lookup s $ tmData tmap
                    unsetValueT (Type_Num from _) = Intconst from
                    unsetValueT Type_NativeNum = Intconst 0
                    unsetValueT Type_Float = Numconst 0.0

                    unsetValue :: Term -> Constval
                    unsetValue f@(Function _ _) = unsetValueT . fromJust . Map.lookup f $ univFluents
                    unsetValue x = error $ "unsetValue: unexpected " ++ show x
                  in foldr (\f -> Map.insert f (unsetValue f)) flMap unsetFluents

                initFlMapFull :: Map.Map Symbol (Map.Map Term Constval)
                initFlMapFull = Map.map fillFluents (initFluents prob)

          -- all fluent initial state assignments of wrong type
          -- for the purpose of this method, the fluent assignments from different initial states are combined into one map
          initFluentsWrongType :: [(Term, [Constval])]
          initFluentsWrongType = Map.assocs . Map.mapMaybeWithKey check $ combinedFluents
            where
                combinedFluents :: Map.Map Term [Constval]
                -- combine all constvals of the fluents from all initial states
                combinedFluents = Map.unionsWith (++) (map (Map.map (:[])) . Map.elems $ initFluents prob)
                check f cs = let wrong = filter (not . checkFluentType eeini f . Atomic . Constant) cs
                             in if null wrong then Nothing else Just wrong

-- CK: simplify grounded actions by removing preconditions and effects using atoms that aren't used whatsoever
          uops      = sort . map (hSimplifyGA univ univFluents) $ pops
          ugoaldnfs = Map.map (hSimplifyDNF univ univFluents) goaldnfs
-- CK: simplify observations: convert to ENF (list of unconditional literals and conditional literals
-- containing unconditionals), remove unused atoms
          obsenf    = MkENF (concat (map (effs . hSimplifyEff univ univFluents . normalizeEffect eeops) (RCData.observation dom)))

-- HS: Get type information
          (tdefs,tnames) = typedefs dom

          specs     = specCompute uops Map.empty

      when (not (null inifv)) (fail $ "*** free variables in init formula: "++show inifv)
      when (not (null goalfv)) (fail $ "*** free variables in goal formula: "++show goalfv)
      when (not (null obsfv)) (fail $ "*** free variables in observation formula: "++show obsfv)
      when (null pops) (fail $ "*** no actions possible")
      when (not.null $ intersect flpreds ap) (fail $ "*** clashing names of fluents and predicates: " ++ show (intersect flpreds ap))
      when (not $ null initFluentsWrongType) (fail $ "*** fluent assignment in initial state assigns wrong type: " ++ show initFluentsWrongType)
      return CompileResult{dpid=(pdomain prob, problem prob),
                           staticLiterals=sl,
                           initialStates=initialStates,
                           initialDuration=idur(diProblem di),
                           achievableLiterals=allLits,
                           groundOps=ops,
                           goalDNFs=ugoaldnfs,
                           goalInitials=goalinis,
                           goalFinals=goalfinals,
                           relvops=rops,
                           possops=uops,
                           universe=univ,
                           univFluents=univFluents,
                           observe=obsenf,
                           typeNames=tnames,
                           typeHierarchy=tdefs,
                           specLut=specs,
                           userLandmarks=lm,
                           tmap = tmap}
   where
   collectAchievables :: GAction -> [Literal] -> [Literal]
   collectAchievables action ls = ls `union` efflits action
   -- add all possible predicates
   uniadd op uv = uv `union` map atom (efflits op)
   atom (Literal _ a) = a
   atom (LiteralF _)  = undefined
   fail = Left

----------------------------------------------------------------------
-- * Normalization of Effect Formulae

-- CK: normalization converts e formula to an array of unconditional Literals and conditional literals the condition of which is normalized (in dnf) and the effect of which is an array of unconditional Literals.

normalizeEffect :: EvalEnv -> EFormula -> ENF
normalizeEffect ee = MkENF . simplifyEffects ee . nme1
   where
   nme1 f = case f of
              EFNot (EFAtom s t) -> [Unconditional . EPredicate $ Literal False (Atom s t)]
              EFNot t            -> error $ "not can only be used on atoms, but has been used on " ++ show t
              EFAtom s t         -> [Unconditional . EPredicate $ Literal True (Atom s t)]
              EFAssign f t       -> case f of
                                Function _ _ -> [Unconditional $ EAssignment f t]
                                _ -> error $ "assign can only be used on fluents, but has been used on " ++ show f
              EFAnd as           -> concat . map nme1 $ as
              EFWhen c e         -> let dnf  = normalize ee c
                                        effs = nme1 e
                                    in if isTrue dnf then effs
                                     else if isFalse dnf then []
                                     else [Conditional dnf effs]
              EFForall d e       -> let effs = nme1 e
                                        (types,vars) = splitDecl d
                                        bnd = bindings (eeTypes ee) types
                                    in [instantiate env ce | ce <- effs
                                                           , b <- bnd
                                                           , let env = zip vars b]

-- check if the assignment of a term (constant or other fluent) to a fluent conforms to the declared fluent type
-- for other comparisons, the functions performing the comparisons should check the types
checkFluentType :: EvalEnv -> Term -> Term -> Bool
checkFluentType ee f1 f2 = case (f1, f2) of
    (Function s _, Atomic (Constant c)) -> checkConstant c s
    (Atomic (Constant _), Function _ _) -> error $ "Cannot assign fluent " ++ show f2 ++ " to constant " ++ show f1
    (Function s _, FFun c)              -> checkFuncall s c
    (FFun c, _)                         -> error $ "Cannot assign value to formula " ++ show c
    (Function s1 _, Function s2 _)      -> checkSubtype s1 s2
    _ -> True
    where
          checkConstant c fl = case flt fl of -- check a single constant
             -- handle different fluent types
             Type_Object t    -> c `elem` values t    -- object fluents: c must be a compatible constant for that object
             Type_Num from to -> case c of    -- numeric fluents: c must be an integer in range
                                    Intconst i -> i >= from && i <= to
                                    Numconst x -> error $ "float value " ++ show x ++ " not compatible with numeric fluent " ++ show fl
                                    Symconst s -> error $ "symbol " ++ show s ++ " not compatible with numeric fluent " ++ show fl
             Type_NativeNum   -> case c of    -- numeric native fluents: c must just be some integer (we cannot determine the range, this is up to the implementation/compiler)
                                    Intconst _ -> True
                                    Numconst x -> error $ "float value " ++ show x ++ " not compatible with numeric fluent " ++ show fl
                                    Symconst s -> error $ "symbol " ++ show s ++ " not compatible with numeric fluent " ++ show fl
             Type_Float       -> case c of
                                    Symconst s -> error $ "symbol " ++ show s ++ " not compatible with numeric fluent " ++ show fl
                                    Intconst _ -> True
                                    Numconst _ -> True
          checkSubtype fl1 fl2 = case (flt fl1, flt fl2) of
             -- handle different fluent type combinations
             (Type_Object t1, Type_Object t2)           -> all (`elem` values t1) (values t2)
             (Type_Num from1 to1, Type_Num from2 to2)   -> from2 >= from1 && to2 <= to1
             (Type_Num _ _, Type_NativeNum)             -> error $ "unrestricted fluent " ++ show fl2 ++ " incompatible to restricted fluent " ++ show fl1
             (Type_Num _ _, Type_Float)                 -> error $ "float fluent " ++ show fl2 ++ " incompatible to numeric fluent " ++ show fl1
             (Type_NativeNum, Type_Num _ _)             -> True
             (Type_NativeNum, Type_NativeNum)           -> True
             (Type_NativeNum, Type_Float)               -> error $ "float fluent " ++ show fl2 ++ " incompatible to numeric fluent " ++ show fl1
             (Type_Float, Type_Num _ _)                 -> True
             (Type_Float, Type_NativeNum)               -> True
             (Type_Float, Type_Float)                   -> True
             _                                                      -> error $ "numeric and object fluent types " ++ show fl1 ++ " and " ++ show fl2 ++ " are incompatible"

          checkFuncall fl c = case (flt fl, c) of
            (Type_Object _, _) -> error $ "Cannot assign numeric value to object fluent " ++ show fl
            _                  -> True

          values t = get $ lookup t (tmData $ eeTypes ee)
          flt s = get . Map.lookup s $ eeFluentTypes ee
          get x = case x of Just v -> v; Nothing -> error "normalizeEffect: Key not found"

{- simplifying effects:
   + group effects by preconditions
   + compute conditions
   + remove static false
   + zap identical effects
   (- convert static true to unconditionals) ==> not really necessary
-}

simplifyEffects :: EvalEnv -> [CEffect] -> [CEffect]
simplifyEffects ee effs =
   foldr reExpand [] [zapFunc ee $ Conditional c' es
                     | (c,es) <- Map.toList . Map.fromListWith union . map toKV $ effs
                     , let c' = simplifyDNF ee c
                     , not(isFalse c')] -- eval undefined c' /= TernaryFalse ]
   where
   reExpand ce@(Conditional c es) cs = let badTypes = filter badType es
                                       in if null badTypes
                                            then if isTrue c then es ++ cs else ce:cs
                                            else error $ "Assignment to fluent have wrong types: " ++ show badTypes
   reExpand (Unconditional _) _      = undefined
   badType (Unconditional (EAssignment f1 f2)) = not $ checkFluentType ee f1 f2
   badType _ = False
   toKV :: CEffect -> (DNF, [CEffect])
   toKV (Unconditional e) = (trueDNF, [Unconditional e])
   toKV (Conditional c es) = (c, nub $ simplifyEffects ee es)

----------------------------------------------------------------------
-- * Normalization of Precondition Formulae

-- CK: converts a formula into DNF (a disjunction of conjunctions). Quantifications are unfolded and thus removed.
--     additionaly, a simplification of the resulting DNF is performed using simplify function

normalize :: EvalEnv -> Formula -> DNF
normalize ee  = MkDNF . normalize1
  -- Note: The usage of normalize1 in all recursive cases implies two things:
  -- * As an advantage, the sub-formulas can be considerably smaller when expanding (forall/exists)
  -- * As a disadvantage, the whole simplification process cannot rely on instantiated variables
  where
  normalize1 = simplify ee . normalize2
  normalize2 :: Formula -> RawDNF
  normalize2 f = case f of
                   FAtom s t    -> [[Literal True (Atom s t)]]
                   FAnd as      -> map concat . sequence . map normalize1 $ as
                   FOr os       -> concat . map normalize1 $ os
                   FNot f       -> sequence . map (map lnot) $ (normalize1 $ f)
                   FImply a b   -> normalize1 (FOr [FNot a,b])
                   FIff a b     -> normalize1 (FOr [FAnd [a,b], FAnd [FNot a, FNot b]])
                   FForall d f' -> map concat . sequence . dnfs d $ f'
                   FExists d f' -> concat . dnfs d $ f'
                   FIsType v t  -> resolveIsType (zapFunc ee v) t
                   FLitForm f   -> [[LiteralF f]]
  dnfs d f = let (types,vars) = splitDecl d
             in map (\vals -> instantiate (zip vars vals) (normalize1 f)) (bindings (eeTypes ee) types)
  resolveIsType (Atomic a) t     = resolveIsTypeAtomic a t
  resolveIsType (Function _ _) _ = error $ "resolveIsType: Unexpected function" -- all functions should be "zapped"
  resolveIsType (FFun _) _       = error $ "resolveIsType: cannot check type of function calls" -- Some specialised handling could be done here, evaluating the function call
  resolveIsTypeAtomic (Constant c) t   = if c `elem` tassoc t (eeTypes ee) then ors trueDNF else ors falseDNF
  resolveIsTypeAtomic (Variable _) _   = error $ "resolveIsType: cannot check type of variables" -- all variables should be bound
  resolveIsTypeAtomic (PrevFunVar _) _ = error $ "resolveIsType: cannot check type of variables" -- all variables should be bound

dnf2formula :: DNF -> Formula
dnf2formula (MkDNF ors) = FOr $ map (\ands -> FAnd $ map lit2f ands) ors
  where
  lit2f (Literal True (Atom s t)) = FAtom s t
  lit2f (Literal False (Atom s t)) = FNot (FAtom s t)
  lit2f (LiteralF f) = FLitForm f

----------------------------------------------------------------------
-- * Simplifying preconditions


handleLiteral :: EvalEnv -> [Literal] -> Literal -> Maybe [Literal]
handleLiteral ee ls l1 = let l = zapFunc ee l1
                         in case eval ee l of
                              TernaryTrue            -> return ls
                              TernaryFalse           -> fail "false"
                              _ | l `elem` ls        -> return ls
                                | (lnot l) `elem` ls -> fail "contradiction"
                                | otherwise          -> return (l:ls)

type HandleFluentsResult = ([Literal], Map.Map Term (Either Term [Term]), Map.Map Term [Term])

-- conjunctions of comparisons involving the same fluent are always false, if compared to different constants
-- The same test is also applied to variables for two reasons:
-- * Variables, like fluents, can also have one distinct value. So conjunctions requiring two different values are statically false.
-- * Without tracking variables, expressions such as (and (= ?v c) (= (fl-x) ?v) (= (fl-x) c)) would wrongly classified as false,
--   and expressions such as (and (= ?v c) (= (fl-x) ?v) (not (= (fl-x) c))) would not be recognised as false.
-- Note: while the builtinEqual function operates on the terms within single literal, this function operates on all literals in one conjunction.
handleFluents :: [Literal] -> Maybe [Literal]
handleFluents ls = do
                    (ls', _, _) <- foldM hf ([], Map.empty, Map.empty) ls
                    return ls'
    where
    -- Takes a list of collected fluents to return, a Map of equalities (already encountered comparisons), a Map of inequalities (already encountered negative comparisons), and a next literal.
    -- The first map is of type Map.Map Term (Either Term [Term]). It maps fluent/variable -> values compared against (Left: constant, Right: list of other fluents and variables)
    -- That is, we track for each fluent/variable either the constant value, or (if no constant is known) all other fluents/variables that have the same value.
    -- The second map is of type Map.Map Term [Term]. It maps fluent/variable -> all other fluents/variables and constants known to be different
    -- The key elimination strategy is:
    -- * If a fluent/variable is compared against different values, the conjunction is statically false
    -- * If a fluent/value is compared against a value v1, and tested against inequality of a value v2, and v1 == v2, the conjunction is false
    hf :: HandleFluentsResult -> Literal -> Maybe HandleFluentsResult
    hf (ls, eql, ineql) l = case l of
            Literal _ _ -> res (l:ls) -- a simple atomic assertion without fluents / comparisons
            LiteralF f  -> case f of
                FEqual t1 t2    -> hfeql True t1 t2
                FNEqual t1 t2   -> hfeql False t1 t2
                _               -> res (l:ls) -- fluent comparisons are not handled here because this would be too messy, probably. Most statically false comparisons should be removed by the eval function
        where
            hfeql p t1 t2
              | isFlVar t1 && isConst t2 = let f = t1; c = t2 in -- all variable renaming for convenience
                case (Map.lookup f eql, Map.lookup f ineql) of
                    (Nothing, Nothing)   -> resIns p (l:ls) f c     -- Fluent/Variable not previously compared against
                    (Just (Right _), _)  -> resIns p (l:ls) f c     -- No constant values known before -> resIns propagates the equalities (if p) or adds the inequality (if not p)
                    (Just (Left c'), _)  | p && c /= c'      -> fail "contradiction" -- Compared against two different values
                                         | p && c == c'      -> res ls               -- The same equality test again, do not need to add the test
                                         | not p && c /= c'  -> res ls               -- We already have an equality test, so this inequality is useless
                                         | otherwise         -> fail "contradiction" -- Must be equal c' and not equal c' at the same time
                    (Nothing, Just vs)   | p && c `notElem` vs     -> resIns p (l: filter (not.isIneql f) ls) f c    -- The previous inequality tests are not required, remove
                                         | p && c `elem` vs        -> fail "contradiction"   -- Must be equal and not equal v at the same time
                                         | not p && c `notElem` vs -> resIns p (l:ls) f c    -- Another inequality test
                                         | otherwise               -> res ls                 -- The same inequality test again, do not need to add the test
              | isConst t1 && isFlVar t2 = hfeql p t2 t1   -- simply swap variables
              | isFlVar t1 && isFlVar t2 = let f1 = t1; f2 = t2 in
                -- In contrast to comparisons fluent/variable against constant, we consider only the equalities among both fluents/variables.
                -- We gain no additional information about some other fluents/variables that are inequal to one or both fluents/variables.
                case (Map.lookup f1 eql, Map.lookup f2 eql) of
                    (Just (Left c1), Just (Left c2))     | p == (c1 /= c2) -> fail "contradiction"  -- both fluents/variables either a) have different values, but must be equal, or b) have the same value, but must be different
                                                         | otherwise       -> res ls                -- both fluents/variables are guaranteed to have the same value -> do not need this literal
                    (Just (Left c), _)                   -> resIns p (l:ls) f2 c  -- We already know a constant for f1 -> nothing new for f1. Keep this literal and a) if p: insert/propagate equality f2 == c or b) if not p: add inequality f2/=c
                    (_, Just (Left c))                   -> resIns p (l:ls) f1 c  -- Just as above
                    _                                    -> resIns p (l:ls) f1 f2 -- No special equalities known. -> Add the (in)equalities (based on p), propagate them, and keep the literal
              | isConst t1 && isConst t2 = res (l:ls) -- this case should not occur, as it should have been simplified by builtinEqual already
              | isFunCall t1 || isFunCall t2 = res (l:ls) -- for now, do not try to evaluate function calls
              | otherwise = undefined -- all cases should be covered

            isFlVar (Function _ _) = True
            isFlVar (Atomic (Variable _)) = True
            isFlVar _ = False
            isConst (Atomic (Constant _)) = True
            isConst _ = False
            isFunCall (FFun _) = True
            isFunCall _ = False

            res ls = return (ls, eql, ineql)
            -- return a result and add (in)equality (depending on p) of f to v to the maps
            resIns p ls f v = return (ls, if p then insEql f v else eql, if p then ineql else insIneql f v)
            insIneql f c@(Atomic _) = Map.insertWith (++) f [c] ineql
            insIneql f1 f2@(Function _ _) = Map.insertWith (++) f2 [f1] . Map.insertWith (++) f1 [f2] $ ineql -- add inequalities to both fluents
            insIneql _ (FFun _) = undefined
            -- Insert a new value of f into the eql map.
            -- If the new value is a constant, just add this constant, potentially replacing old values and propagating the constant
            -- If the new value is a fluent, combine all fluents equal to f1 or f2
            insEql f c@(Atomic _) = insEql' c (propConst f c eql) f
            insEql f1 f2@(Function _ _) = let fs1 = lukup f1 eql
                                              fs2 = lukup f2 eql
                                              fs = f1 : f2 : union fs1 fs2 -- all fluents equal to each other
                                          in Map.mapWithKey (replaceAll fs) eql -- make all fluents equal
                where
                lukup k m = case Map.lookup k m of Nothing -> []; Just (Right fs) -> fs; _ -> undefined
                replaceAll _ _ v@(Left _)   = v -- do not replace constant values, but should not happen anyway
                replaceAll fs _ (Right _) = Right fs -- keep the self-equality for simplicity
            insEql _ (FFun _) = undefined
            -- Just add the value to the map m
            insEql' c m f = Map.insert f (Left c) m
            -- propagate constant equalities
            propConst f c m = case Map.lookup f m of
                    Nothing -> m       -- No previous equality known
                    Just (Left _) -> m -- Cannot propagate constants
                    Just (Right fs) -> foldl' (insEql' c) m fs -- for all fluents f' equal to f, add the equality f' = v

            -- return if the literal (2nd parameter) is an inequality test involving the Fluent (1st parameter)
            -- this is used above to simplify (and (not (= (f1) c1)) (= (f1) c2)) to (= (f1) (c2)), removing previous inequalities
            isIneql :: Term -> Literal -> Bool
            isIneql _ (Literal _ _) = False
            isIneql fl (LiteralF f) = case f of
                        FNEqual t1 t2 -> fl == t1 || fl == t2
                        _ -> False

-- CK: simplifies conjunctive list of literals by removing statically true literals or even the whole conjunction in case it is unsatisfiable.

simplifyAnd :: EvalEnv -> [Literal] -> Maybe [Literal]
simplifyAnd ee ls = foldM (handleLiteral ee) [] ls >>= handleFluents


-- CK: simplifies a raw DNF ([[Literal]]) by simplifying each conjunction separately first (with simplifyAnd method) and combining the results afterwards (with mergeClause method).
--     If one of the clauses is statically true, the whole DNF is. The third parameter is used as accumulator and should be set to [] initially.

--simplifyOrs :: StaticPredicates -> RawDNF -> RawDNF -> RawDNF
simplifyOrs _ [] os = os
simplifyOrs ee (as:ass) os = case simplifyAnd ee as of
                               Nothing -> simplifyOrs ee ass os -- False AND clause: ignore
                               Just [] -> [[]]                  -- True AND clause: DNF is true
                               Just as' -> simplifyOrs ee ass (mergeClause as' os)

--mergeClause as os = as:os


mergeClause as os = merge as os []


-- simplify :: StaticPredicates -> RawDNF -> RawDNF
simplifyDNF ee (MkDNF dnf) = MkDNF (simplify ee dnf)

simplify ee rdnf = simplifyOrs ee rdnf []

-- CK: merges the clauses (conjunction) 'a' with the clauses in 'bbs'. Three different strategies are used:
--     1. if a literal in 'bbs' subsumes 'a', then a is not used at all and the resulting clauses are not altered
--     2. if 'a' subsumes a literal in 'bbs', that literal is deleted from the list (the process continues)
--     3. if 'a' and some 'b' in 'bbs' differ only by a single literal pair ('al','bl') one is the negation of the other, leaving out the literal coveres both clauses 'a' and 'b' at once. This new literal is used instead of 'a' and 'b' (a new merge process is started with it)

merge a [] os = a:os
merge a bbs@(b:bs) os = let la = length a
                            lb = length b
                            br = b \\ a
                            ar = a \\ b
                        in if la >= lb && null br then bbs++os -- 'a' subsumed by existing literal
                           else if la <= lb && null ar then merge a bs os -- 'b' subsumed by new literal
                           else if la == lb then case (ar,br) of
                                                   ([al],[bl]) -> if al == lnot bl
                                                                  then merge (delete al a) (os++bs) [] -- found new simpler clause, redo
                                                                  else merge a bs (b:os)
                                                   _ -> merge a bs (b:os)
                           else merge a bs (b:os)

----------------------------------------------------------------------
-- * Finding relevant and possible operators

-- | 'doIter' iterates over a list of operators and selects
--   operators relevant to a current evaluation environment.
--
--   The evaluation environment is then updated by applying
--   the operator to it

-- CK: general-purpose iteration to filter a list of relevant operators using some (arbitrary) environment. For all actions a relevance value (val::<arbitrary>) is calculated (env->GAction->val) and tested (val->Bool) for relevance. All relevant actions are updated (Int->val->GAction->GAction). If some actions were relevant, the environment will be updated sequentially with each of them (GAction->env->env), and a new round is started, in which all remaining actions are tested again. All four of these functions are given as parameters. The Int value in the GAction update function reflects the round the action was added (starting with 0)
iterOps :: (env -> GAction -> val)        -- ^ relevance test
           -> (val -> Bool)
           -> (GAction -> env -> env)     -- ^ environment updater
           -> (Int -> val -> GAction -> GAction) -- ^ action updater
           -> [GAction]                   -- ^ candidate actions
           -> env                         -- ^ initial environment
           -> [GAction]                   -- ^ resulting list of relevant actions
iterOps fval ftest fupdate fupda ops env =  iter 0 ops [] [] env []
  where
  --iter :: [GAction] -> [GAction] -> env1 -> Bool -> [GAction] -> [GAction]
  iter _ [] _   []  _   ras = ras
  iter n [] ias nas env ras = iter (n+1) ias [] [] (foldr fupdate env nas) (nas ++ ras)
  iter n (ca:cas) ias nas env ras =
    let v = fval env ca
    in if ftest v
       then iter n cas ias (fupda n v ca:nas) env ras
       else iter n cas (ca:ias) nas env ras

-- CK: determine all actions which are applicable in the environment, possibly after the application of some other actions. Every action not appearing in the result has no chance at all to be used.
possibleOps :: [GAction] -> EvalEnv -> [GAction]
possibleOps = iterOps pval ptest pupd pupa
  where
  pval ee ca = eval ee (gpre ca)
  ptest v =  v /= TernaryFalse
  -- Add effects of action to environment, but never remove previous effects, this is a coarse over-approximation, but we do not need to expand the full state space.
  -- As a result, a Literal may be True and False in the environment.
  -- Also, for every effect (x) which was not true in the environment, add (not x): (not x) was true in the initial state (under closed world assumption), and must still be true after this action.
  pupd ca ee = ee{eeLits=eeLits ee `union` (efflits ca ++ negateLits litsToNegate)}
    where
    -- Determine literals which must be added in negated form: only those, which are not already true in the environment.
    -- If they were true, the literal may be true in the initial state, and (not x) may thus never be true.
    litsToNegate = (positiveLits $ efflits ca) \\ (positiveLits $ eeLits ee)
    positiveLits = filter (\(Literal b _) -> b)
    negateLits = map (\(Literal True a) -> (Literal False a))
  pupa _ _ = id

-- CK: determine all actions which are (directly or indiractly) useful for achieving the stated list of literals. Useful actions are all action that directly make one of the Literals true and all actions that are useful to achieve the precondition of useful actions.
relevantOps :: [GAction] -> [Literal] -> [GAction]
relevantOps = iterOps rval rtest rupd rupa
  where
  rval gls ca = efflits ca `intersect` gls
  rtest v = not . null $ v
  rupd ca gls = gls `union` prelits ca
  rupa n v ca = ca{glevel=n, gachieve=length v}

----------------------------------------------------------------------
-- * Simplifying against a herbrand universe listing all possible atoms and fluents
-- Idea:
--    * conjuncts that contain positive literals where atom
--      not in universe are definitely false -> remove
--    * negative literals where atom not in universe are
--      definitely true -> remove conjunct from literal
--    * otherwise: keep literal


hSimplifyDNF uni uniFl = MkDNF . map unjust . filter isJust . map hSimplifyAnd . ors
             where unjust (Just a)  = a
                   unjust _         = undefined
                   isJust (Just {}) = True
                   isJust _         = False
                   hSimplifyAnd     = foldr hsa (Just [])
                      where hsa lit = liftM2 (++) (hSimplifyLit lit)
                   hSimplifyLit l@(Literal p a) | not (a `elem` uni) = if p then Nothing else Just []
                                                | otherwise = Just [l]
                   hSimplifyLit l@(LiteralF f)  = litfapply (\t1 t2 -> check [t1, t2]) f
                                                  where check ts = if null (badfls ts)
                                                            then Just [l]
                                                            else error $ "Comparing fluent-value which is never assigned a value: " ++ show (badfls ts)
                   badfls ts = foldl' (\bs t -> badfls' t ++ bs) [] ts
                   badfls' fl@(Function _ _) = [fl | not.isJust $ Map.lookup fl uniFl]
                   badfls' (Atomic _)        = []
                   badfls' (FFun c)          = funcallapply (\t1 t2 -> badfls [t1, t2]) c


hSimplifyGA uni uniFl ga = ga{gpre=hSimplifyDNF uni uniFl (gpre ga),
                              geff=hSimplifyEff uni uniFl (geff ga)}

hSimplifyEff uni uniFl = MkENF . hSimplifyEff2 . effs
  where hSimplifyEff1 (Conditional c es) rest = let c' = hSimplifyDNF uni uniFl c
                                                    es' = hSimplifyEff2 es
                                                in if isTrue c' then es'++rest
                                                  else if isFalse c' then rest
                                                  else Conditional c' es' : rest
        hSimplifyEff1 uce rest = uce:rest
        hSimplifyEff2 :: [CEffect] -> [CEffect]
        hSimplifyEff2 = foldr hSimplifyEff1 []
