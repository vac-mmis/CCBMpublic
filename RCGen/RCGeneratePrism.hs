-- Generates a model file for the PRISM model checker (http://www.prismmodelchecker.org/)
-- A simple MDP without durations will be generated.
module RCGeneratePrism (generatePrism) where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Maybe
import Data.Char
import Debug.Trace

import Util
import RCGenerateUtil
import RCData hiding (crnl)
import RCCompile


showState :: CompileResult -> Luts -> ShowS
showState CompileResult{initialStates=istates} luts@(Luts{luFlTypes=flTypes, luAtoms=nlist, luFluents=flist, luObjConstIDs=objConstIDs}) =
    let
        -- get a single initial state
        (ils, ifl) = if Map.size istates == 1
                         then head . Map.elems $ istates
                         else error $ "Prism supports only one initial state, but there are: " ++ show (Map.keys istates)
        -- TODO PRISM supports multiple initial states: http://www.prismmodelchecker.org/manual/ThePRISMLanguage/MultipleInitialStates

        showel ils (a, name) = ps "S" . ps name . ps " : bool init " . ps (if Literal True a `elem` ils then "true" else "false")
                          . ps "; // " . shows a . ps "\n"

        -- in PRISM, we use one domain for all object fluents
        -- Using individual domains of object fluents complicates assigning and comparing types which overlap
        -- We could also use chains of (cond ? a : b) expressions to convert one type into another
        -- But this blows up the code, and BDDs should be gould at compressing the domain anyway and have lightweight transitions
        showfl ifl (f, name) = let flType = flTypes `lukup` f in case flType of
                                Type_Object _    -> ps "F" . ps name . ps " : [0.." . shows (Map.size objConstIDs - 1) . ps "]"
                                Type_Num from to -> ps "F" . ps name . ps " : [0.." . shows (to - from) . ps "]"
                                Type_NativeNum   -> ps "F" . ps name . ps " : int"
                                Type_Float       -> error $ "PRISM does not support floats as used by fluent " ++ show f
                          . ps " init "
                            -- analogue to the initial literals, the initial fluents do not necessarily contain all fluents from the universe
                            -- but instead of adding the Constvalue, we simply use 0 as the initial value below
                          . fluentValue luts f (Atomic . Constant . fromJust . Map.lookup f $ ifl)
                          . ps "; // " . shows f . ps "\n"

    in
      folds (showel ils) nlist
    . folds (showfl ifl) flist

showsOffset offset = case compare offset 0 of
        LT -> shows offset
        EQ -> id
        GT -> ps "+" . shows offset

-- for a given fluent and constant value, return the assigned Int (or Float) value as String
fluentValue :: Luts -> Term -> Term -> ShowS
fluentValue (Luts{luFlTypes=flTypes, luObjConstIDs=objConstIDs}) f v = value v
    where
      flType = flTypes `lukup` f
      value (Atomic (Constant c)) = case flType of
                Type_Object _   -> shows $ objConstIDs `lukup` c -- in PRISM, all object fluents share the same domain
                Type_Num from _ -> shows $ ((\(Intconst i)->i) c) - from
                Type_NativeNum  -> shows $ (\(Intconst i)->i) c
                Type_Float      -> shows $ (\(Numconst f)->f) c
      value v = error $ "fluentValue: unexpected non-constant " ++ show v

-- print access to the raw fluent value
showFl Luts{luFluent=fllut} f = ps "F" . ps (fllut `lukup` f)

showDNF :: Luts -> DNF -> ShowS
showDNF luts@(Luts{luAtom=lut, luFlTypes=fluentTypes}) dnf | ors dnf == [] = ps "false" -- empty disjuntion evaluates to false
                 | otherwise = foldr (.) id . concat . List.intersperse [ps " | "] . map showAnd . ors $ dnf
  where
    showAnd [] = [ps "true"] -- empty conjunction evaluates to true
    showAnd lits = (\s -> if length s > 1 then ps "(" :s++[ps ")"] else s) . List.intersperse (ps " & ") . map showLit $ lits
    showLit (Literal p a) = ps "S" . ps (lut `lukup` a) . ps "=" . ps (if p then "true" else "false")
    showLit (LiteralF f)  = ps "(" . case f of
                                      FEqual t1 t2        -> showFluentCmp "=" t1 t2
                                      FNEqual t1 t2       -> showFluentCmp "!=" t1 t2
                                      FGreaterThan t1 t2  -> showFluentCmp ">" t1 t2
                                      FGreaterEqual t1 t2 -> showFluentCmp ">=" t1 t2
                                      FLessThan t1 t2     -> showFluentCmp "<" t1 t2
                                      FLessEqual t1 t2    -> showFluentCmp "<=" t1 t2
                            . ps ")"
    showFluentCmp _ (Atomic _) (Atomic _)                = error $ "Unexpected const-expression"
    showFluentCmp op f1@(Function _ _) f2@(Atomic _)     = showFl luts f1 . ps op . fluentValue luts f1 f2
    showFluentCmp op f1@(Function _ _) f2@(Function _ _) = showFl luts f1 . ps op .
        case (typef1, typef2) of
            _ | typef1 == typef2 -> showFl luts f2-- exactly same type: raw comparison
            (Type_Object _,     Type_Object _   ) -> showFl luts f2 -- all object fluents share the same domain: raw comparison
            (Type_Object _,     _               ) -> mkError
            (Type_Num from _,   Type_Num from' _) -> showFl luts f2 . showsOffset (from' - from)
            (Type_Num from _,   Type_NativeNum  ) -> showFl luts f2 . showsOffset (- from)
            (Type_Num _ _,      _               ) -> mkError
            (Type_NativeNum,    Type_Num from _ ) -> showFl luts f2 . showsOffset from
            (Type_NativeNum,    _               ) -> mkError
            (Type_Float,        Type_Object _   ) -> mkError
            (Type_Float,        Type_Num from _ ) -> showFl luts f2 . showsOffset from
            (Type_Float,        Type_NativeNum  ) -> showFl luts f2
            _ -> mkError
      where typef1 = fluentTypes `lukup` f1
            typef2 = fluentTypes `lukup` f2
            mkError = error $ "Unexpected comparison of fluent " ++ show f1 ++ " (type " ++ show typef1 ++ ") to fluent " ++ show f2 ++ " (type " ++ show typef2 ++ ")"
    showFluentCmp op (FFun c) f2@(Atomic _)              = showFunCall luts c . ps op . shows f2
    showFluentCmp op (FFun c) f2@(Function _ _)          = showFunCall luts c . ps op .
        case typef2 of
            Type_Object _   -> error $ "Unexpected comparison of (arithmetic) function call " ++ show c ++ " to object fluent " ++ show f2
            Type_Num from _ -> showFl luts f2 . showsOffset from
            Type_NativeNum  -> showFl luts f2
            Type_Float      -> showFl luts f2
      where
        typef2 = fluentTypes `lukup` f2
    showFluentCmp op (FFun c1) (FFun c2)                 = showFunCall luts c1 . ps op . showFunCall luts c2
    showFluentCmp op f1 f2                               = showFluentCmp (swapOp op) f2 f1
    swapOp op | op == ">"  = "<"
                | op == ">=" = "<="
                | op == "<"  = ">"
                | op == "<=" = ">="
                | op == "="  = "="
                | op == "!=" = "!="
                | otherwise  = error $ "Unsupported operator " ++ op

showEff luts@(Luts{luFlTypes=fluentTypes, luAtom=lut}) e =
  case e of
    Unconditional e ->  ps "\n\t\t" . showEff e
    Conditional _ _ -> error $ "Cannot generate conditional effects: " ++ show e
  where
    showEff (EPredicate (Literal b a)) = ps "(S" . ps (lut `lukup` a) . ps "' = "
                                         . ps (if b then "true)" else "false)")
    showEff (EPredicate (LiteralF f))  = error $ "Cannot set a formula " ++ show f
    showEff (EAssignment f t)          = ps "(" . showFl luts f . ps "' = " . showTerm f t . ps ")"
    showTerm f v@(Atomic _)       = fluentValue luts f v
    showTerm f f'@(Function _ _)  =
      case (typef, typef') of
          _ | typef == typef'                  -> showFl luts f' -- exactly same type: raw copy
          (Type_Object _,   Type_Object _    ) -> showFl luts f' -- all object fluents have the same domain: raw copy
          (Type_Object _,   _                ) -> mkError
          (Type_Num from _, Type_Num from' _ ) -> showFl luts f' . showsOffset (from' - from)
          (Type_Num from _, Type_NativeNum   ) -> showFl luts f' . showsOffset (- from)
          (Type_Num _ _,    _                ) -> mkError
          (Type_NativeNum,  Type_Num from _  ) -> showFl luts f' . showsOffset from
          (Type_NativeNum,  _                ) -> mkError
          (Type_Float,      Type_Object _    ) -> mkError
          (Type_Float,      Type_Num from _  ) -> showFl luts f' . showsOffset from
          (Type_Float,      Type_NativeNum   ) -> showFl luts f'
          _ -> mkError
      where typef = fluentTypes `lukup` f
            typef' = fluentTypes `lukup` f'
            mkError = error $ "Unexpected assignment of fluent " ++ show f' ++ " (type " ++ show typef' ++ ") to fluent " ++ show f ++ " (type " ++ show typef ++ ")"
    showTerm f (FFun c)           = showFunCall luts c . showsOffset (- (fluentOffset fluentTypes f))

-- separate handling for function calls / evaluations
-- Here, we do not print the value of a constant wrt. a fluent, but its raw value
showFunCall luts@(Luts{luFlTypes=fluentTypes}) c = ps "(" . case c of
                           FPlus t1 t2  -> showTerm t1 . ps "+" . showTerm t2
                           FMinus t1 t2 -> showTerm t1 . ps "-" . showTerm t2
                           FMult t1 t2  -> showTerm t1 . ps "*" . showTerm t2
                           FDiv t1 t2   -> showTerm t1 . ps "/" . showTerm t2
                . ps ")"
  where
    showTerm v@(Atomic _)      = shows v
    showTerm (FFun c)          = showFunCall luts c
    showTerm f@(Function _ _)  =
        case typef of
            Type_Object _   -> error $ "Unexpected object fluent " ++ show f ++ " in a function call " ++ show c
            Type_Num from _ -> ps "(" . showFl luts f . showsOffset from . ps ")"
            Type_NativeNum  -> showFl luts f
            Type_Float      -> showFl luts f
      where
        typef = fluentTypes `lukup` f


showDoAction luts ga | null es = ps "true"
                     | otherwise = fst $ foldr addEff (id, True) es
    where es = effs (geff ga)
          addEff eff (rest, first) = (showEff luts eff . (if first then id else ps " &") . rest, False)

showAction luts ga =
  let precond = showDNF luts (gpre ga)
  in ps "\n" . precond . ps "\n\t-> "
     . showDoAction luts ga


showGA :: Luts
          -> GAction -> (String, Int)
          -> (ShowS, Int)
          -> (ShowS, Int)
showGA luts ga (gaName, _gid) (afuncs,nops) =
       (if (not.null.cantOccurAfter) ga then trace "Warning: :non-repeating actions not supported in PRISM, ignoring. This may alter your state space" else id)
       (ps "\n\n// " . shows (gname ga)
                     . ps "\n[" . ps gaName . ps "] " . showAction luts ga . ps ";" . afuncs
       ,nops+1)

showGoal luts name dnf rest =
  ps "\nlabel \"goal_" . shows name . ps "\" = "
  . showDNF luts dnf
  . ps ";"
  . rest

escape :: String -> String
escape =
      snd . foldr zap ('\0', "") -- replace repeated _ by a single _ - → "action_b_"
    . map (\c -> if isLetter c || isDigit c || c `elem` allowed then c else '_') -- escape invalid - → "action__b_"
    . filter (not . (`elem` strip)) -- remove unwanted - "(action *b*)" → "action *b*"
  where
    zap '_' r@('_', _) = r
    zap c (_, result) = (c, c:result)
    allowed = "_"
    strip = "()" -- remove parantheses completely

generatePrism :: CompileResult -> ShowS
generatePrism cr =
   let luts = buildLuts cr compare escape
       sstr = showState cr luts
       (afuncs,_nops) = Map.foldrWithKey (showGA luts) (id, 0) (luActions luts)
   in ps "// Domain " . shows (fst (dpid cr)) . ps "\n"
      . ps "// Problem " . shows (snd (dpid cr)) . ps "\n"
      . ps "mdp\n"
      . ps "module m\n"
      . sstr
      . afuncs
      . ps "\nendmodule"
      . ps "\n" . Map.foldrWithKey (showGoal luts) id (goalDNFs cr)
      . ps "\n"
