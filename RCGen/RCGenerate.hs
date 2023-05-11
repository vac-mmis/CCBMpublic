module RCGenerate (generatePF) where

import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Maybe
import Text.Printf
import Data.Char

import Util
import RCGenerateUtil
import RCData hiding (crnl)
import RCCompile

data CompileOpts = CO { boundCheck :: Bool }

-- compute the number of bits required to represent the value of a fluent
bits :: Int -> Int
bits n = ceiling $ logBase 2 (fromIntegral n)

-- Returns (impl::ShowS, header::ShowS), where
-- impl: C code for the model implementation
-- header: C code for the model header
showState :: Luts -> (ShowS, ShowS)
showState (Luts{luFlTypes=fluentTypes, luObjFlMap=objFlMap, luAtom=atomMap, luAtomIdx=atomIdx, luFluent=fluentMap, luFluentIdx=fluentIdx}) =
   let
       sstr  = ps "struct StateRec {\n"
               . Map.foldrWithKey showel id atomMap
               . Map.foldrWithKey showfl id fluentMap
               . ps "\n\tStateRec() {bzero(this,sizeof(StateRec));}\n};\n"
       pstr  = ps "std::ostream& operator<<(std::ostream &o, StatePtr x) {"
               . Map.foldrWithKey prstel id atomMap
               . Map.foldrWithKey prstfl id fluentMap
               . ps "\n\treturn o;\n}\n"
       wrstr = ps "void writeState(std::ostream &o, StatePtr x, double wt) {"
               -- for each state variable: 1 byte space and 1 byte value + weight
               -- for each fluent: 1 byte space and (bits * log(2) / log(10)) digits
               . ps "\n\tchar buf[16 + 3*" . shows (Map.size fluentMap) . ps " + " . shows digits . ps "]; char *s = (char*) buf;"
               . ps "\n\tif (std::isinf(wt)) strcpy(s, \"inf\"), s += 3; else s += sprintf(s, \"%g\", wt);"
               . Map.foldrWithKey writel id atomMap
               . Map.foldrWithKey writfl id fluentMap
               . ps "\n\t*s = '\\n'; *(++s) = '\\0'; o << (char*) buf;\n}\n"
         where digits = sum . map digs $ Map.elems fluentTypes
               digs (Type_Object s) = toDigs . Map.size . lukup objFlMap $ s -- sum individiual bit sizes for every "finite" fluent (Object and Num)
               digs (Type_Num from to) = toDigs $ to - from
               digs Type_NativeNum = 20 -- for every NativeNum and Float reserve 20 bytes
               digs Type_Float = 20
               toDigs = ceiling . (/ log 10) . (* log 2) . fromIntegral
       rdstr = ps "bool readState(std::istream &i, StateRec *x, double &wt) {"
               . ps "\n\tchar buf[4096]; char *s = (char*) buf;"
               . ps "\n\tdo i.getline(s, 4096); while (i.gcount() < 2 && i.good());\n\tif (!i.good()) return false;"
               . ps "\n\tif (!memcmp(s, \"inf\", 3)) { wt = infinity, s+=3; } else wt = strtod(s, &s);"
               . (if Map.null atomMap then id else
                    ps "\n\tchar c;"
                  . Map.foldrWithKey readel id atomMap
                 )
               . (if Map.null fluentMap then id else
                    ps "\n\tint n; unsigned int v; float vf; (void) v; (void) vf;"
                  . Map.foldrWithKey readfl id fluentMap
                 )
               . ps "\n\treturn true;\n}\n"
       cstr  = ps "void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr) { // rt: Recency Vector (ACT-R Heuristic); t: ist the global time"
               . Map.foldrWithKey compel id (Map.mapWithKey (\a name -> (name, fromJust $ Map.lookup a atomIdx)) atomMap)
               . Map.foldrWithKey compfl id (Map.mapWithKey (\a name -> (name, fromJust $ Map.lookup a fluentIdx)) fluentMap)
               . ps "\n}\n"
       rstr  = ps "double getRefract(StatePtr a1, StatePtr a2) { // compare two action preconditions and returns the rerfractoriness \n\tdouble r=0; int n=0;"
               . Map.foldrWithKey refrl id atomMap
               . Map.foldrWithKey refrfl id fluentMap
               . ps "\n\treturn (n==0) ? 0 : r/n;\n}\n"
   in (pstr . wrstr . rdstr . cstr . rstr, sstr)
   where
         flformat :: Term -> Bool -> String
         flformat f write = case fluentTypes `lukup` f of
            Type_Object _  -> "%u"
            Type_Num _ _   -> "%u"
            Type_NativeNum -> "%d"
            Type_Float | write -> "%.8e"
                       | True  -> "%f"
         flReadVal :: Term -> String
         flReadVal f = case fluentTypes `lukup` f of { Type_Float -> "vf"; _ -> "v" }

         showel a name rest = ps "\t unsigned int S" . ps name . ps ":1; /* " . shows a . ps " */\n" . rest
         showfl f name rest =
              case fluentTypes `lukup` f of
                Type_Object s    -> showflfin f name . Map.size . lukup objFlMap $ s
                Type_Num from to -> showflfin f name $ to - from + 1
                Type_NativeNum   -> ps "\t int F" . ps name
                Type_Float       -> ps "\t float F" . ps name
            . ps "; /* " . shows f . ps " */\n" . rest
            where
                showflfin _ name n = ps "\t unsigned int F" . ps name . (':':) . shows (bits n)
         prstel a name rest = ps "\n\tif(x->S" . ps name . ps ") o << \" " . shows a . ps "\"; else o<< \" (not " .shows a . ps ")\"; ". rest
         prstfl f name rest = ps "\n\to << \" " . shows f . ps "=\" << "
                             . case fluentTypes `lukup` f of
                                 Type_NativeNum  -> ps "x->F" . ps name
                                 Type_Float      -> ps "x->F" . ps name
                                 Type_Num from _ -> ps "(x->F" . ps name . showsOffset from . ps ")"
                                 Type_Object t   -> ps "objnames_" . cidentifier t . ps "[x->F" . ps name . ps "]"
                             . ps ";" . rest
         writel _ name rest = ps "\n\t*s = ' '; s++; *s =x->S" . ps name . ps " + '0'; s++;" . rest
         writfl f name rest = ps "\n\t*s = ' '; s++; s += sprintf(s, \"" . ps (flformat f True) . ps"\", x->F" . ps name . ps ");" . rest
         readel _ name rest = ps "\n\twhile (isspace(*s)) {s++;} c = *s - '0'; if (c == 0 || c == 1)  x->S" . ps name . ps " = c, s++; else return false;" . rest
         readfl f name rest = ps "\n\twhile (isspace(*s)) {s++;} sscanf(s, \"". ps (flformat f False) . ps "%n\", &" . ps (flReadVal f) . ps ",&n); s+=n; x->F" . ps name . ps " = " . ps (flReadVal f) . ps ";" . rest
         compel _ (name, i) rest = ps "\n\tif (prev->S" . ps name . ps " ^ curr->S" . ps name . ps ") rT[" . shows i . ps "] = t;" . rest
         compfl _ (name, i) rest = ps "\n\tif (prev->F" . ps name . ps " != curr->F" . ps name . ps ") rT[" . shows i . ps "] = t;" . rest
         refrl  _ name rest = ps "\n\tif (a1->S" . ps name . ps ") {n++; if (a2->S" . ps name . ps ") r++;}" . rest
         -- note: as by the getRecOrRef function, the fluent fields will always be 0 or 1 (just indicating whether this fluent has been checked in the precondition)
         refrfl _ name rest = ps "\n\tif (a1->F" . ps name . ps ") {n++; if (a2->F" . ps name . ps ") r++;}" . rest

showsOffset offset = case compare offset 0 of
        LT -> shows offset
        EQ -> id
        GT -> ps "+" . shows offset

-- for a given fluent and constant value, return the assigned Int (or Float) value as String
fluentValue :: Luts -> Term -> Term -> ShowS
fluentValue (Luts{luFlTypes=flTypes, luObjFlMap=objFlMap}) f v = value v
    where
      flType = flTypes `lukup` f
      value (Atomic (Constant c)) = case flType of
                Type_Object s   -> shows $ (objFlMap `lukup` s) `lukup` c -- values of object fluents are numbered 0..(num-1)
                Type_Num from _ -> shows $ ((\(Intconst i)->i) c) - from
                Type_NativeNum  -> shows $ (\(Intconst i)->i) c
                Type_Float      -> shows $ (\(Numconst f)->f) c
      value v = error $ "fluentValue: unexpected non-constant " ++ show v

-- print access to the raw fluent value
showFl Luts{luFluent=fllut} x f = ps x . ps "->F" . ps (fllut `lukup` f)

showDNF :: Luts -> DNF -> ShowS
showDNF luts@(Luts{luAtom=lut, luFlTypes=fluentTypes}) dnf | ors dnf == [] = ps "false" -- empty disjuntion evaluates to false
                | otherwise = foldr (.) id . concat . List.intersperse [ps " || "] . map showAnd . ors $ dnf
  where
    showAnd [] = [ps "true"] -- empty conjunction evaluates to true
    showAnd lits = (\s -> if length s > 1 then ps "(" :s++[ps ")"] else s) . List.intersperse (ps " && ") . map showLit $ lits
    showAtom a = ps "x->S" . ps (lut `lukup` a)
    showLit (Literal p a) = (if p then id else ps "!(") . showAtom a . (if p then id else ps ")")
    showLit (LiteralF f)  = case f of
              FEqual t1 t2        -> showFluentCmp "==" t1 t2
              FNEqual t1 t2       -> showFluentCmp "!=" t1 t2
              FGreaterThan t1 t2  -> showFluentCmp ">" t1 t2
              FGreaterEqual t1 t2 -> showFluentCmp ">=" t1 t2
              FLessThan t1 t2     -> showFluentCmp "<" t1 t2
              FLessEqual t1 t2    -> showFluentCmp "<=" t1 t2
    showFluentCmp _ (Atomic _) (Atomic _)                = error $ "Unexpected const-expression"
    showFluentCmp op f1@(Function _ _) f2@(Atomic _)     = showFl luts "x" f1 . ps op . fluentValue luts f1 f2
    showFluentCmp op f1@(Function _ _) f2@(Function _ _) = showFl luts "x" f1 . ps op .
        case (typef1, typef2) of
            _ | typef1 == typef2                  -> showFl luts "x" f2-- exactly same type: raw comparison
            (Type_Object s1,    Type_Object s2  ) -> ps "objectIDs_" . cidentifier s1 . ps "[objectUIDs_" . cidentifier s2 . ps "[" . showFl luts "x" f2 . ps "]]" -- value (f2) → uid → value (f1)
            (Type_Object _,     _               ) -> mkError
            (Type_Num from _,   Type_Num from' _) -> showFl luts "x" f2 . showsOffset (from' - from)
            (Type_Num from _,   Type_NativeNum  ) -> showFl luts "x" f2 . showsOffset (- from)
            (Type_Num _ _,      _               ) -> mkError
            (Type_NativeNum,    Type_Num from _ ) -> showFl luts "x" f2 . showsOffset from
            (Type_NativeNum,    _               ) -> mkError
            (Type_Float,        Type_Object _   ) -> mkError
            (Type_Float,        Type_Num from _ ) -> showFl luts "x" f2 . showsOffset from
            (Type_Float,        Type_NativeNum  ) -> showFl luts "x" f2
            _ -> mkError
      where typef1 = fluentTypes `lukup` f1
            typef2 = fluentTypes `lukup` f2
            mkError = error $ "Unexpected comparison of fluent " ++ show f1 ++ " (type " ++ show typef1 ++ ") to fluent " ++ show f2 ++ " (type " ++ show typef2 ++ ")"
    showFluentCmp op (FFun c) f2@(Atomic _)              = showFunCall luts c . ps op . shows f2
    showFluentCmp op (FFun c) f2@(Function _ _)          = showFunCall luts c . ps op .
        case typef2 of
            Type_Object _   -> error $ "Unexpected comparison of (arithmetic) function call " ++ show c ++ " to object fluent " ++ show f2
            Type_Num from _ -> showFl luts "x" f2 . showsOffset from
            Type_NativeNum  -> showFl luts "x" f2
            Type_Float      -> showFl luts "x" f2
      where
        typef2 = fluentTypes `lukup` f2
    showFluentCmp op (FFun c1) (FFun c2)                 = showFunCall luts c1 . ps op . showFunCall luts c2
    showFluentCmp op f1 f2                               = showFluentCmp (swapOp op) f2 f1
    swapOp op | op == ">"  = "<"
                | op == ">=" = "<="
                | op == "<"  = ">"
                | op == "<=" = ">="
                | op == "==" = "=="
                | op == "!=" = "!="
                | otherwise  = error $ "Unsupported operator " ++ op

showEff luts@(Luts{luFlTypes=fluentTypes, luAtom=lut}) (CO{boundCheck=bc}) adbgname e = showEff1 (ps "\n\t\t") e
  where
    showEff1 indent e =
      case e of
        Unconditional e ->  indent . showEff' e
        Conditional c es -> indent . ps "if("
                            . showDNF luts c . ps ") {"
                            . folds (showEff1 (indent . ps "\t")) es
                            . ps "}"
    showEff' (EPredicate (Literal b a)) = ps "x1->S" . ps (lut `lukup` a)
                                         . ps (if b then " = 1;" else " = 0;")
    showEff' (EPredicate (LiteralF f))  = error $ "Cannot set a formula " ++ show f
    showEff' (EAssignment f t)          = showFl luts "x1" f . ps " = " . showTerm f t . ps ";"
    showTerm f v@(Atomic _)       = fluentValue luts f v
    showTerm f f'@(Function _ _)  =
      case (typef, typef') of
          _ | typef == typef' -> showFl luts "x" f' -- exactly same type: raw copy
          (Type_Object s,   Type_Object s'  ) -> ps "objectIDs_" . cidentifier s . ps "[objectUIDs_" . cidentifier s' . ps "[" . showFl luts "x" f' . ps "]]" -- value (f') → uid → value (f)
          (Type_Object _,   _               ) -> mkError
          (Type_Num from _, Type_Num from' _) -> showFl luts "x" f' . showsOffset (from' - from)
          (Type_Num from to, Type_NativeNum ) ->
                if bc then
                    ps "boundCheck(" . showFl luts "x" f' . ps ", " . shows from . ps ", ". shows to . ps ", \"" . shows f . ps " in " . ps adbgname . ps " while assigning fluent " . shows f' . ps "\")" . showsOffset (- from)
                else
                    showFl luts "x" f' . showsOffset (- from)
          (Type_Num _ _,    _               ) -> mkError
          (Type_NativeNum,  Type_Num from _ ) -> showFl luts "x" f' . showsOffset from
          (Type_NativeNum,  _               ) -> mkError
          (Type_Float,      Type_Object _   ) -> mkError
          (Type_Float,      Type_Num from _ ) -> showFl luts "x" f' . showsOffset from
          (Type_Float,      Type_NativeNum  ) -> showFl luts "x" f'
          _ -> mkError
      where typef = fluentTypes `lukup` f
            typef' = fluentTypes `lukup` f'
            mkError = error $ "Unexpected assignment of fluent " ++ show f' ++ " (type " ++ show typef' ++ ") to fluent " ++ show f ++ " (type " ++ show typef ++ ")"
    showTerm f (FFun c)           = case fluentTypes `lukup` f of
                                        Type_Object _    -> error $ "Unexpected object fluent " ++ show f ++ " as assignment-target of (arithmetic) function call " ++ show c
                                        Type_Float       -> showFunCall luts c   -- floats can be computed directly, no offset adjustment or bound checks required
                                        Type_NativeNum   -> showFunCall luts c   -- integers can be computed directly, no offset adjustment or bound checks required
                                        -- the boundCheck function returns the value if it is in the given range, then we subtract the offset from the value
                                        Type_Num from to -> if bc then
                                                                ps "boundCheck(" . showFunCall luts c . ps ", " . shows from . ps ", ". shows to . ps ", \"" . shows f . ps " in " . ps adbgname . ps "\")" . showsOffset (- from)
                                                            else
                                                                showFunCall luts c . showsOffset (- from)

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
            Type_Num from _ -> ps "(" . showFl luts "x" f . showsOffset from . ps ")"
            Type_NativeNum  -> showFl luts "x" f
            Type_Float      -> showFl luts "x" f
      where
        typef = fluentTypes `lukup` f

showDoAction luts opts ga adbgname = ps "\n\t\t*x1 = *x;"
                      . folds (showEff luts opts adbgname) (effs (geff ga))
                      . ps "\n\t\treturn " . sal (gsal ga) . ps ";"
   where sal Nothing  = ps "1"
         sal (Just t) = shows t

-- generate landmarks for all initial/goal state combinations
showLandmarks cr (luts@Luts{luAtom=lut}) =
        ps "\n\nint landmarkCountHeuristic(size_t initialState, size_t goal, StatePtr s, StateRec *parentAccepted) {"
      . ps "\n\t(void) s;"
      . ps "\n\tStateRec accepted;\n\tint count = 0;"
      . ps "\n\tswitch(initialState) {"
      . folds showInit (luInitEffects luts)
      . ps "\n\tdefault: throw std::runtime_error(\"landmarkCountHeuristic: Unknown initial state\");"
      . ps "\n\t}"
      . ps "\n\t*parentAccepted = accepted;\n\treturn count;"
      . ps "\n}"
  where
    showInit (idx, (name, init)) =
          ps "\n\tcase " . shows idx . ps ": /*" . shows name . ps "*/"
        . ps "\n\t\tswitch(goal) {"
        . folds (showLandmark (getEffLits init)) (luGoals luts)
        . ps "\n\t\tdefault: throw std::runtime_error(\"landmarkCountHeuristic: Unknown goal state\");"
        . ps "\n\t\t}"
        . ps "\n\t\tbreak;"

    showLandmark initL (gidx, (gname, goal)) =
      let
         (lgvs,lges) = landmarkAlg cr goal initL luts -- add predefined or user defined landmarks instead of []
         lgvsr = [ (Literal p a , lukup lut a) | (Literal p a) <- lgvs]--, p == True]
         lgesr = lges
      in
          ps "\n\t\tcase ". shows gidx . ps ": /*" . shows gname . ps "*/"
        . ps "\n\t\t\tif (parentAccepted == 0) {\n\t\t\t\t//accepted = StateRec();accepted.S1=1;"
        . folds (initl lgesr initL) lgvsr
        . ps "\n\t\t\t} else {"
        . ps "\n\t\t\t\taccepted= *parentAccepted;\n\t\t"
        . folds (contl lgesr) lgvsr
        . ps "\n\t\t\t} // compute the heuristic"
        . folds nacc lgvsr
        . folds (reqgoal (literals goal)) lgvsr
        . folds (reqprecon lgesr) lgvsr
        . ps "\n\t\t\tbreak;"

    -- extracts all literals from the effects; currently, the landmarks only support predicates and not fluents
    getEffLits :: [CEffect] -> [Literal]
    getEffLits = foldr (\e ls -> case e of {Unconditional (EPredicate l) -> l:ls; _ -> ls}) []

    initl lgesr initL (a,i) = case List.find (==a) initL of
                                          Nothing -> id
                                          Just _ -> ps (orderings a i)
                                              where
                                                  orderings a i | [ x | (x,y) <- lgesr, y==a] == [] = "\n\t\t\t\taccepted.S"++ i ++"=1;"
                                                                | otherwise = ""
    contl lgesr (a,i) = ps (corderings a i)
                          where
                              accepted a i = let list = [ x | (Literal _ x,y) <- lgesr, y==a]
                                             in acc list ++" accepted.S"++ i ++"=1;"
                                             where
                                             acc [] = ""
                                             acc (x:xs) = " if (parentAccepted->S"++ (lukup lut x) ++"==1) "++ acc xs
                              corderings a i = "\n\t\t\t\tif (s->S"++ i ++"==1)" ++ accepted a i
    nacc (_,i) = ps "\n\t\t\tif (" . ps "accepted.S" . ps i . ps "== 0) count++;"
    reqgoal goallits (a,i) | List.find (==a) goallits == Nothing = id
                           | otherwise  = ps "\n\t\t\t\tif (accepted.S" . ps i . ps "== 1 && s->S" . ps i . ps "== 0) count++;"
    reqprecon lgesr (a,i) | (reqprechelp lgesr a) == [] = id -- there is no ordering which allows this situation
                          | otherwise = ps "\n\t\t\tif (accepted.S" . ps i . ps "== 1 && s->S" . ps i . ps "== 0 " . ps (reqprechelp lgesr a) .ps ") count++;"
    reqprechelp lgesr x                | evalu == [] = []
                                       | otherwise   = " && (" ++ evalu ++ ")"
                                                   where evalu = List.concat $ List.intersperse (" || ") [ "accepted.S" ++ (lukup lut y) ++"==0" | (z,Literal _ y) <- lgesr, z==x] -- need only one element out of O

-- Landmark Heuristics end

-- showSpecs :: Map.Map Int Int -> Map.Map Atom Int -> DNF -> [([Char],[Int])]
showSpecs specLut luts dnf  = (pred,specs)
  where
  pred = map (\o -> showDNF luts (MkDNF [o]) "") $ ors dnf
  specs = map showAnds . ors $ dnf
  showAnds lits = printf "%.2f" $ ((foldr (+) 0 $ map showLits $ lits)::Float)
  showLits l = 1 / fromIntegral (specLookup specLut l)

showActionSpec1 specLut luts ga | True =
  let precond = showSpecs specLut luts (gpre ga)
      (p,s) = precond
      prec = List.concat [ a++b | (a,b)<-(zip ["\n\tif(" ++ pp | pp <- p ] [ ") *specificity="++ ss ++"; \n" | ss <- s])]
  in ps prec


showAction luts opts ga adbgname | isTrue (gpre ga) = showDoAction luts opts ga adbgname . ps "\n"
showAction luts opts ga adbgname | True =
  let precond = showDNF luts (gpre ga)
  in ps "\n\tif(" . precond . ps ") {"
     . showDoAction luts opts ga adbgname
     . ps "\n\t} else {\n\t\treturn 0;\n\t}\n"


showStateObservations _ _ (MkENF []) = ps "\n\nvoid stateObservation(StatePtr) { }\n" -- id
showStateObservations pfx luts enf   = ps "\n\nvoid stateObservation(StatePtr x) {" . showObservations pfx luts enf .  ps "\n}"

showObservations pfx luts (MkENF obs) = showObservations' obs
  where
  showObservations' obs =  \s->foldr showObservation s obs
  showObservation (Unconditional e)   = showObsCall e
  showObservation (Conditional c es)  = ps "\n\tif(" . showDNF luts c . ps ") { " . showObservations' es . ps " }"
  showObsCall (EPredicate (Literal True (Atom a as)))  = shows a . ps pfx . ((List.intercalate "," (map show as)) ++) . ps ");"
  showObsCall c = error $ "Malformed observation call "++show c


-- another way to approximate the goal distance
--subgoalstr lut dnf= ps "\n\nint countUnreachedSubgoals(StateRec x) {\n\tint all = 0;\n\tint n=0;\n\t"
--               . subgoall dnf
--               . ps "\n\treturn all-n;\n}"
--           where
--            subgoall c = showSubGoals lut c
--            showSubGoals lut dnf  = foldr (.) id . concat . List.intersperse [(" || "++)] . map showAnd . ors $ dnf
--                where
--                showAnd lits =  map showLit $ lits
--                showLit (Literal p a) = (if p then id else ps "!") . showAtom a
--                showAtom a   = ("\n\tif( x.S"++) . ps (lut `lukup` a) . ps (") n++;\n\tall++;")


-- supports the act-r heuristic the specificity
-- from every literal of the precondition compute the number of literals and the depth of each literal in the hierarchie
showActionSpec (Luts{luAtom=luAtom, luFluent=fllut, luAtomIdx=atomIdx, luFluentIdx=fluentIdx}) gid ga = ps "double getRecOrRefr" . shows gid
                            . ps " (int rT[ELEMENTS_UNIVERSE], int t, StateRec *r) {"
                            . (if ors (gpre ga) == [[]] then ps "\n\t(void) rT; (void) t;" else id) -- prevent "unused variable" warning if precondition is empty
                            . ps "\n\tif (r != NULL) {\n\t\t"
                            . showRef (gpre ga)
                            . ps "\n\t} else \n\t\t return ("
                            . showRec  (gpre ga)
                            . ps ")/" . shows (showLength (gpre ga))
                            . ps ";\n\treturn -1;\n}\n"
    where
        showLength dnf = mySum . map length . ors $ dnf -- maybe differentiate between the ors (see specificity)
                where
                mySum l = if (s==0) then 1 else s where s = sum l

        showRec dnf = foldr (.) id . concat . List.intersperse [ps " +"] . map showAnd . ors $ dnf -- maybe differentiate between the ors (see specificity)
                where
                showAnd [] = [ps "0"]
                showAnd lits = List.intersperse (ps "+") . concatMap showLit $ lits
                showLit :: Literal -> [ShowS]
                showLit (Literal _ a) = showAtom a
                -- This results in two terms if two fluents are compared, e.g. (= (fluent-1 x) (fluent-2 y z))
                showLit (LiteralF f)  = litfapply (\t1 t2 -> showFluent t1 ++ showFluent t2) f
                showAtom a = [ps "exp(rT[" . shows (atomIdx `lukup` a) . ps "]-t)"]
                showFluent (Atomic _) = []
                showFluent f@(Function _ _) = [ps "exp(rT[" . shows (fluentIdx `lukup` f) . ps "]-t)"]
                showFluent (FFun c) = funcallapply (\t1 t2 -> showFluent t1 ++ showFluent t2) c
        showRef dnf = foldr (.) id . concat . List.intersperse [ps " "] . map showAndR . ors $ dnf -- maybe differentiate between the ors (see specificity)
                where
                showAndR lits =  map showLitR $ lits
                showAtomR a = ps "r->S" . ps (luAtom `lukup` a) . ps "=1; "
                showLitR (Literal _ a) = showAtomR a
                showLitR (LiteralF f)  = litfapply (\t1 t2 -> showFluentR t1 . showFluentR t2) f
                -- This results in two bits set if two fluents are compared, e.g. (= (fluent-1 x) (fluent-2 y z)) → bits for both fluents will be 1
                showFluentR (Atomic _) = id
                showFluentR f@(Function _ _) = ps "r->F" . ps (fllut `lukup` f) . ps "=1; "
                showFluentR (FFun c) = funcallapply (\t1 t2 -> showFluentR t1 . showFluentR t2) c

                -- supports the act-r heuristic the specificity
                -- from every literal of the precondition compute the number of literals and the depth of each literal in the hierarchie

-- showActionPrecondition lut gid ga = ps "\n\nvoid getPrec" . shows gid
--                             . ps " (StatePtr r) {"
--                             . ps "\n\tif (r != NULL) {\n\t\t"
--                             . showPre (gpre ga)
--                             . ps "\n\t}\n}"
--  where
--         showPre dnf = foldr (.) id . concat . List.intersperse [(" "++)] . map showAndR . ors $ dnf -- maybe differentiate between the ors (see specificity)
--                 where
--                 showAndR lits =  map showLitR $ lits
--                 showLitR (Literal p a) = showAtomR a
--                 showAtomR a   = ("r->S"++) . ps (lut `lukup` a) . ps "=1; "


-- different implementations for single and multi agent models
showCbk _ _ (MkENF []) = id
showCbk luts gaName enf = ps "void callback_" . ps gaName . ps "(StatePtr x, int _agid) { (void) x; "
                      . showObservations "(_agid," luts enf
                      . ps "}\n"

-- print all action scheme variables and structs (these will be inside the ActionSchemes namespace)
-- the variables will be named simply after the action name, possibly added with numbers (e.g. put, put2, put3)
-- action: the action scheme
-- header, model: the header / model string to which this action scheme shall be concatenated
-- actionMap: a map from action name -> number of occurences
-- actionNameMap: a map from action -> generated action Name
showActionScheme :: Action -> (ShowS, ShowS, ShowS, Map String Int, Map Action ShowS) -> (ShowS, ShowS, ShowS, Map String Int, Map Action ShowS)
showActionScheme action (header, headerStructs, model, actionMap, actionNameMap) =
  let
    varBaseName = cidentifier (asym action) ""
    actionNum = Map.findWithDefault 1 varBaseName actionMap
    -- prefix all variable names with A_ to avoid conflicts with reserved keywords or numbers
    varName = ps varBaseName . if actionNum == 1 then id else shows actionNum
    params :: [(Symbol, Type)]
    params = case adecl action of
                Nothing -> []
                Just (VDecls ps) -> concatMap (\(t, names) -> zip names (repeat t)) ps
  in
   ( -- header, action scheme variables
      ps "extern const ActionScheme s_" . varName . ps ";\n"
    . header
   , -- action scheme structs
      -- create a struct for every action scheme that inherits from Action
      -- TODO adds specialised fields for parameter values
      ps "struct S_" . varName . ps " : Action {\n"
        -- constructor
        . ps "\tS_" . varName . ps "(int id, const std::string &name, "
            . ps "ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, "
            . ps "DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :\n"
        . ps "\t\tAction(id, name, &s_" . varName . ps ", func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}"
    . ps "}; // struct S_" . varName . ps "\n"
    . headerStructs
   , -- model
      -- first create an array of variable names
      ps "boost::array<paramType," . shows (length params) . ps "> __params_" . varName . ps " = {"
    . foldsIntersperse (ps ",") (\(name, t) -> ps "std::make_pair(\"" . shows name . ps "\",\"" . shows t . ps "\")") params
    . ps "};\n"
    . ps "const ActionScheme s_" . varName . ps "(\"" . shows (asym action) . ps "\", std::vector<paramType>(__params_" . varName . ps ".begin(), __params_" . varName . ps ".end()));\n"
    . model
   , Map.insert varBaseName (actionNum+1) actionMap
   , Map.insert action varName actionNameMap
   )

showGA :: Map.Map Literal Int
          -> Luts
          -> Map Action ShowS
          -> CompileOpts
          -> GAction -> (String, Int)
          -> (ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, Int)
          -> (ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, ShowS, Int)
showGA specLut luts actionSchemeNames opts ga (gaName, gid) (afuncs,anames,avarNames,abindis,adurfns,adurcdffns,adurpdffns,acbkfns,aspec, actionHeader, actionModel, anpfx,nops) =
  let
    cbtag (MkENF []) = ps "NULL"
    cbtag _          = ps "callback_" . ps gaName
    adbgname         = "action_" ++ gaName ++ " " ++ show (gname ga)
    aschname a       = ps "(:action " . shows (asym a) . ps " " . case adecl a of {Nothing -> id; Just d -> shows d } . ps ")"
    -- prefix all variable names with A to avoid conflicts with reserved keywords or numbers
    varName = ps "a_" . ps gaName
    schemeStruct = ps "ActionSchemes::S_" . Map.findWithDefault (error "action name not found") (baseAction ga) actionSchemeNames
  in
    (
      -- action function
      ps "\n/* " . shows (gname ga) . ps ", id " . shows gid . ps " */\n"
        . ps "double action_" . ps gaName . ps "(StatePtr x, StateRec *x1, double *specificity) {" . showActionSpec1 specLut luts ga . showAction luts opts ga adbgname . ps "}\n" . afuncs
    , -- action names array
      ps "\n\t\"" . shows (gname ga) . ps "\"" . anpfx . ps " // " . aschname (baseAction ga) . anames
    , -- action variable pointers (inserted into actions array)
      ps "\n\t&Actions::" . varName . anpfx . avarNames
    , shows (bindingIndex ga) . anpfx . abindis -- write binding indices on one line
    , durationModelFunc (gdur ga) gid . adurfns
    , durationModelCDFFunc (gdur ga) gid . adurcdffns
    , durationModelPDFFunc (gdur ga) gid . adurpdffns
    , showCbk luts gaName (gcbk ga) . acbkfns
    , -- action heuristic function
      showActionSpec luts gid ga . aspec
    , -- action variable in header
      ps "extern const " . schemeStruct . ps " " . varName . ps ";\n" . actionHeader
    , -- action variable definition in model
      ps "const " . schemeStruct . ps " " . varName . ps "("
        . shows gid . ps ", " -- id
        . ps "\"" . shows (gname ga) . ps "\", " -- name, leave it readable here instead of actionNames[gid]
        . ps "action_" . ps gaName . ps ", " -- action function
        . cbtag (gcbk ga) . ps ", " -- callback function
        . ps "getRecOrRefr" . shows gid . ps ", " -- heuristic function
        . durationModelId (gdur ga) gid . ps ", " -- duration model function
        . durationModelCDFId (gdur ga) gid . ps ", " -- duration model CDF function
        . durationModelPDFId (gdur ga) gid . ps ", " -- duration model PDF function
        . durationModelTime (gdur ga) gid  -- duration model parameter (only if exponentially distributed, otherwise -1)
        . ps ");\n"
      . actionModel
    , ps ","
    , nops+1)

showsgid (-1) = ps "Initial"
showsgid gid = shows gid

durationModelTime (Just aa@(Atom a [t])) gid = case show a of
                                                 "exponential" -> ps "1-(" . shows t . ps ") /* " . showsgid gid . ps " */"
                                                 _             -> ps "-1 /* bad time model " . shows aa . ps " in " . showsgid gid . ps " */"
durationModelTime Nothing gid                = ps "0.0 /* " . showsgid gid . ps " */"
durationModelTime (Just aa) gid              = ps "-1 /* bad time model " . shows aa . ps " in " . showsgid gid . ps " */"

durationModelId dur gid = case dur of
                                 Nothing -> ps "finishedImmediate"
                                 Just _  -> ps "finished" . showsgid gid
durationModelFunc dur gid = case dur of
                                   Nothing         -> id
                                   Just (Atom a l) -> ps "bool finished" . showsgid gid . ps "(double _1, double *p=NULL) { return "
                                                      . shows a . ps "Model(_1" . folds (\e -> ps "," . shows e) l
                                                      . ps ",p); }\n"

durationModelCDFId dur gid = case dur of
                                    Nothing -> ps "pStopcdfImmediate"
                                    Just _  -> ps "pStopcdf" . showsgid gid
durationModelCDFFunc dur gid = case dur of
                                    Nothing         -> id
                                    Just (Atom a l) -> ps "double pStopcdf" . showsgid gid . ps "(double startTime, double curTime) { return "
                                                       . shows a . ps "cdf(curTime- startTime" . folds (\e -> ps "," . shows e ) l
                                                       . ps "); }\n"

durationModelPDFId dur gid = case dur of
                                    Nothing -> ps "pStoppdfImmediate"
                                    Just _  -> ps "pStoppdf" . showsgid gid

durationModelPDFFunc dur gid = case dur of
                                    Nothing         -> id
                                    Just (Atom a l) -> ps "double pStoppdf" . showsgid gid . ps "(double startTime, double curTime) { return "
                                                        . shows a . ps "pdf(curTime - startTime" . folds (\e -> ps "," . shows e) l
                                                        . ps "); }\n"

showInitialStates luts opts initEffects =
  ps "\n\nvoid sampleInitial(size_t initialState, StateRec *x1) {"
  . ps "\n\tbzero(x1,sizeof(StateRec));"
  . ps "\n\tswitch(initialState) {"
  . folds (\(idx, (name, effs)) ->
        ps "\n\tcase " . shows idx . ps ": /*" . shows name . ps "*/"
      . folds (showEff luts opts ("initialEffect" ++ show idx ++ ": " ++ show name)) effs
      . ps "\n\t\tbreak;"
    ) initEffects
  . ps "\n\tdefault: throw std::runtime_error(\"sampleInitial: Unknown initial state\"); break;"
  . ps "\n\t}\n}"

showGoals luts =
  ps "\n\nbool isGoalState(size_t goal, StatePtr x) { (void) x;"
  . ps "\n\tswitch(goal) {"
  . folds (\(idx, (name, dnf)) ->
        ps "\n\tcase " . shows idx . ps ": /*". shows name . ps "*/"
      . ps "\n\t\treturn "
      . showDNF luts dnf
      . ps ";"
    ) (luGoals luts)
  . ps "\n\tdefault: throw std::runtime_error(\"isGoalState: Unknown goal\"); break;"
  . ps "\n\t}\n}"

-- generate a valid C identifier - this may not be unique (i.e. different names may get the same string)
cidentifier :: Show a => a -> ShowS
cidentifier = ps . escape . show

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

-- returns the model implementation, and two header parts: C preprocessor defines and structures / variables
generatePF :: Bool -> CompileResult -> (ShowS, ShowS, ShowS)
generatePF boundCheck cr =
  let
    opts = CO{boundCheck=boundCheck}

    -- sort fluents to reduce padding, by increasing number of bits, with NativeNum and Float at the end
    fluentTypes = univFluents cr
    flcompare :: Term -> Term -> Ordering
    flcompare f1 f2 = let
                        objBits s = bits . Map.size $ luObjFlMap luts `lukup` s
                        t1 = fluentTypes `lukup` f1
                        t2 = fluentTypes `lukup` f2
                      in if t1 == t2 then EQ else case (t1, t2) of
        (Type_Object s1, Type_Object s2)  -> compare (objBits s1) (objBits s2)
        (Type_Num from to, Type_Object s) -> compare (bits $ to - from) (objBits s)
        (Type_Object s, Type_Num from to) -> compare (objBits s) (bits $ to - from)
        (Type_Num from1 to1, Type_Num from2 to2) -> compare (bits $ to1 - from1) (bits $ to2 - from2)
        (Type_Object _, _) -> LT
        (Type_Num _ _, _) -> LT
        (Type_NativeNum, Type_Float) -> LT
        (Type_NativeNum, _) -> GT
        (Type_Float, _) -> GT

    luts = buildLuts cr flcompare escape

    (sstr_impl, sstr_header) = showState luts
    objNameStr = Map.foldrWithKey (\t os -> (.) $ objNames t os) id (luTypeMap luts)
      where
        objNames t os = ps "char const * const objnames_" . cidentifier t . ps "[] = {" . foldr (.) id (List.intersperse (ps ",") $ map obj os) . ps "};\n"
        obj s = ps "\"" . shows s . ps "\""

    pops       = possops cr
    agentMap   = Map.fromListWith (flip (++)) $ zip (map gagt pops) (map (:[]) [0..])
    (agentModel, agentDefines, agentHeader) = showAgentMap agentMap
    specs=specLut cr

    actionSchemes = Set.fromList $ map baseAction pops
    (actionSchemeHeader, actionSchemeStructs, actionSchemeModel, _, actionSchemeNames) = Set.foldr showActionScheme (id, id, id, Map.empty, Map.empty) actionSchemes
    (afuncs,anames,avarNames,abindis,adurfns,adurcdffns,adurpdffns,acbkfns,aspec, actionHeader, actionModel, _,nops) =
        Map.foldrWithKey (showGA specs luts actionSchemeNames opts) (id,id,id,id,id,id,id,id,id,id,id,ps "", 0) (luActions luts)
   in
    ( -- model definition
      ps "const char * DOMAIN_NAME = \"" . shows (fst (dpid cr)) . ps "\";\n"
    . ps "const char * PROBLEM_NAME = \"" . shows (snd (dpid cr)) . ps "\";\n"
    . crnl
    . showObjectUIDs luts
    . objNameStr
    . crnl
    . sstr_impl
    . crnl
    . ps "char const * const initialStateNames[] = {" . ( foldr (.) id . List.intersperse (ps ", ") . map (\(_, (n, _)) -> ps "\"" . shows n . ps "\"") $ luInitEffects luts) . ps "};\n"
    . ps "char const * const goalNames[] = {" . ( foldr (.) id . List.intersperse (ps ", ") . map (\(_, (n, _)) -> ps "\"" . shows n . ps "\"") $ luGoals luts) . ps "};\n"
    . (if boundCheck then
          ps "\n// Runtime check of bound for numeric fluents\n"
        . ps "int boundCheck(int v, int from, int to, std::string fluent) { if (v >= from && v <= to) return v; throw std::runtime_error(\"Bounds error for \" + fluent); return 0;}\n"
      else
          id)
    . crnl
    . (if any (not.null.cantOccurAfter) pops then
            ps "// association from action id to bindingIndex for canFollowAfter\n"
          . ps "int const nonRepeatBindIndex[NOPS] = {\n\t" . abindis . ps "\n};\n"
          . showRepeatTable pops
          . ps "\n// function for accessing canFollowAfterT with action id\n"
          . ps "bool canFollowAfter(int next, int prev) {\n\tif (0 > next || 0 > prev) { return true; }\n\treturn canFollowAfterT[nonRepeatBindIndex[next]][nonRepeatBindIndex[prev]];\n}\n"
       else
            ps "// no :non-repeating has been used\n"
          . ps "bool canFollowAfter(int next, int prev) { (void)next; (void)prev; return true; }\n"
      )

    . crnl
    . ps "int const FinishedOpId = -3;\nint const NoOpId = -2;\nint const InitOpId = -1;\n"
    . ps "char const * const __actionNames[NOPS+3] = {\n\t\"(FINISHED)\",\n\t\"(BLOCKED)\",\n\t\"(INITIALIZE)\"," . anames . ps "\n};\n"
    . ps "char const * const * const actionNames = &__actionNames[3];\n"
    . crnl
    . afuncs
    . crnl
    . durationModelFunc (initialDuration cr) (-1) . adurfns
    . ps "bool finishedImmediate(double, double* p) { if (p)*p=1.0; return true; }\n"
    . durationModelCDFFunc (initialDuration cr) (-1) . adurcdffns
    . ps "double pStopcdfImmediate(double startTime, double curTime){ return (curTime > startTime) ? 1.0 : 0.0; }\n"
    . durationModelPDFFunc (initialDuration cr) (-1) . adurpdffns
    . ps "double pStoppdfImmediate(double startTime, double curTime){ return (curTime > startTime) ? 1.0 : 0.0; }\n"
    . crnl
    . ps "// Implementations for the Refractoriness and Recency (ACT-R Heuristics)\n"
    . ps "// The first paramter is the current state and the second one is the state for the Refractoriness\n"
    . aspec

    . crnl

    . (if (all (== MkENF []) . map gcbk . Map.keys $ luActions luts) then id else (
          -- only define action callbacks if at least one was defined
          ps "/*** BEGIN CALLBACKS ***/\n"
        . acbkfns
        . ps "/*** END CALLBACKS ****/\n" ))

    . crnl

    . ps "namespace ActionSchemes {\n"
    . ps "const ActionScheme s_FINISHED(\"FINISHED\", std::vector<paramType>());\n"
    . ps "const ActionScheme s_BLOCKED(\"BLOCKED\", std::vector<paramType>());\n"
    . ps "const ActionScheme s_INITIALIZE(\"INITIALIZE\", std::vector<paramType>());\n"
    . actionSchemeModel
    . ps "} // namespace ActionSchemes\n"
    . crnl
    . ps "ActionScheme const * __actionSchemes[" . shows (Map.size actionSchemeNames) . ps " + 3] = {\n"
    . ps "\t&ActionSchemes::s_FINISHED,\n\t&ActionSchemes::s_BLOCKED,\n\t&ActionSchemes::s_INITIALIZE,\n"
    . foldsIntersperse (ps ",\n") (ps "\t&ActionSchemes::s_" .) actionSchemeNames
    . ps "\n};\n"
    . ps "ActionScheme const * *actionSchemes = &__actionSchemes[3];\n"

    . crnl

    . ps "namespace Actions {\n"
    . ps "const ActionSchemes::S_FINISHED a_FINISHED(FinishedOpId, \"FINISHED\", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);\n"
    . ps "const ActionSchemes::S_BLOCKED a_BLOCKED(NoOpId, \"BLOCKED\", NULL, NULL, NULL, finishedImmediate, pStopcdfImmediate, pStoppdfImmediate, 0.0);\n"
    . ps "const ActionSchemes::S_INITIALIZE a_INITIALIZE(InitOpId, \"INITIALIZE\", NULL, NULL, NULL, " . durationModelId (initialDuration cr) (-1) . ps " , " . durationModelCDFId (initialDuration cr) (-1) . ps ", " . durationModelPDFId (initialDuration cr) (-1) . ps ", " . durationModelTime  (initialDuration cr) (-1) . ps ");\n"
    . actionModel
    . ps "} // namespace Actions\n"
    . crnl
    . ps "Action const * __actions[NOPS+3] = {\n"
    . ps "\t&Actions::a_FINISHED,\n\t&Actions::a_BLOCKED,\n\t&Actions::a_INITIALIZE,"
    . avarNames
    . ps "\n};\n"
    . ps "Action const * *actions = &__actions[3];\n"

    . agentModel
    . showGoals luts
    . showInitialStates luts opts (luInitEffects luts)
    . showStateObservations "(" luts (observe cr)
    . showLandmarks cr luts
    . ps "\n"
    , -- header file (defines)
      ps "#pragma once\n"
    . ps "#define NOPS " . shows nops . crnl
    . ps "#define NUM_INITIAL_STATES " . shows (length $ luInitEffects luts) . crnl
    . ps "#define NUM_GOALS " . shows (Map.size $ goalDNFs cr) . crnl
    . ps "#define ELEMENTS_UNIVERSE (" . shows (length (universe cr)) . ps "+" . shows (Map.size (univFluents cr)) . ps ")\n"
    . (if (all (== MkENF []) . map gcbk . Map.keys $ luActions luts)
        then id else
          -- only define action callbacks if at least one was defined
          ps "#define HAS_CALLBACKS 1\n")
    . agentDefines
    , -- header file (structures and variables)
      ps "#pragma once\n"
    . agentHeader
    . crnl
    . sstr_header
    . crnl

    . ps "namespace ActionSchemes {\n"
    -- action scheme variables
    . ps "extern const ActionScheme s_FINISHED;\nextern const ActionScheme s_BLOCKED;\nextern const ActionScheme s_INITIALIZE;\n"
    . actionSchemeHeader
    -- action scheme structs
    . ps "struct S_FINISHED : Action {\n\tS_FINISHED(int id, const std::string &name, "
            . ps "ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, "
            . ps "DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :\n"
        . ps "\t\tAction(id, name, &s_FINISHED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}"
    . ps "}; // struct S_FINISHED\n"
    . ps "struct S_BLOCKED : Action {\n\tS_BLOCKED(int id, const std::string &name, "
            . ps "ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, "
            . ps "DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :\n"
        . ps "\t\tAction(id, name, &s_BLOCKED, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}"
    . ps "}; // struct S_BLOCKED\n"
    . ps "struct S_INITIALIZE : Action {\n\tS_INITIALIZE(int id, const std::string &name, "
            . ps "ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic, "
            . ps "DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :\n"
        . ps "\t\tAction(id, name, &s_INITIALIZE, func, callbackFunc, heuristic, durationModel, cdfModel, pdfModel, durationProb) {}"
    . ps "}; // struct S_INITIALIZE\n"
    . actionSchemeStructs
    . ps "} // namespace ActionSchemes\n"

    . crnl
    . ps "namespace Actions {\n"
    . ps "extern const ActionSchemes::S_FINISHED a_FINISHED;\nextern const ActionSchemes::S_BLOCKED a_BLOCKED;\nextern const ActionSchemes::S_INITIALIZE a_INITIALIZE;\n"
    . actionHeader
    . ps "} // namespace ActionSchemes\n"
    )



showAgentMap am =
  case Map.toList am of
    []   -> error "Impossible: no agent"
    kvs  -> ( -- model
              folds showAgentOps (zip3 agids nops (map snd kvs))
            . ps "\nchar const * const agentNames[NAGENTS] = {"
            . kvshow (shows . aname . fst) kvs
            . ps "\nint const nOpsForAgent[NAGENTS] = {"
            . kvshow shows nops
            . ps "\nint const * const opsForAgent[NAGENTS] = {"
            . kvshow (\i -> ps "agentOps" . shows i) agids
            , -- header (defines)
              ps "#define NAGENTS " . shows na . crnl
            , -- header (variables)
              ps "extern char const * const agentNames[NAGENTS];\n"
            . ps "extern int const nOpsForAgent[NAGENTS];\n"
            . ps "extern int const * const opsForAgent[NAGENTS];\n"
            )
           where na = length kvs
                 nops = map (length . snd) kvs
                 agids = [0..na-1]
                 kvshow how = snd . foldr (\kv (s,r) -> (",", (how kv) . (s++) . r)) ("};",id)
                 aname Nothing  = show (Symconst nullSymbol)
                 aname (Just s) = show s
                 showAgentOps (id,n,ops) = ps "\nconst int agentOps" . shows id . ps "[" . shows n . ps "] = {"
                                           . kvshow shows ops

-- creates a table of actions that can't occur after each other
showRepeatTable :: [GAction] -> ShowS
showRepeatTable gas = ps "// canFollowAfterT[a][b] is true if action with bindIndex a may follow after action with bindIndex b\n"
                    . ps "bool canFollowAfterT[" . shows nbis . ps "][" . shows nbis . ps "] = {\n"
                    . showRepeatLines
                    . ps "};\n"
  where
    nbis = length.List.nub.map bindingIndex $ gas
    -- build a map bindIndex -> cantOccurAfter-indices
    bimap :: [(Int, [Int])]
    bimap = List.sortBy (\a b -> compare (fst a) (fst b)) $ foldr (\ga m -> let bi = bindingIndex ga in if isJust (bi `lookup` m) then m else (bi, cantOccurAfter ga):m) [] gas
    showRepeatLines = ps $ List.intercalate "," $ map (showRepeatLine) bimap
    showRepeatLine (bi, coa) = "\t{" ++ List.intercalate "," (map (canOccurAfter coa) [0..nbis-1]) ++ "} /* " ++ show bi ++ " */\n"
    canOccurAfter coa bi' = if bi' `elem` coa then "false" else "true" -- transform cantOccurAfter to canOccurAfter

-- generate an array for each object fluent type, where
-- - the index = the ("compressed") value of an object for this fluent type
-- - the value = the unique id
-- Additionally, generate the inverse array from unique id → value
-- This is required to compare (and assign) object fluents of different types (where one may be a subset of another)
showObjectUIDs :: Luts -> ShowS
showObjectUIDs (Luts{luObjFlMap = objFlMap, luObjConstIDs = objConstIDs}) =
  let
    -- the list of all constants in objConstIDs in increasing order of index
    sortedAllConsts :: [Constval]
    sortedAllConsts = map fst . List.sortBy (\a b -> compare (snd a) (snd b)) . Map.toList $ objConstIDs

    -- generate an array for a specific object type
    showObjects :: (Symbol, Map.Map Constval Int) -> ShowS
    showObjects (typeSym, objMap) =
        -- type-value → UID
          ps "unsigned int objectUIDs_" . cidentifier typeSym . ps "[] = {"
        . foldsIntersperse (ps ",") (\c -> shows $ objConstIDs `lukup` c) sortedConsts
        . ps "};\n"
        -- UID → type-value
        . ps "unsigned int objectIDs_" . cidentifier typeSym . ps "[] = {"
        . foldsIntersperse (ps ",") (\c -> case Map.lookup c objMap of
                                        Nothing -> ps "UINT_MAX"
                                        Just objid -> shows objid
                                    ) sortedAllConsts
        . ps "};\n"
      where
        -- For this type, the constants in order of the numeric representation (0 .. (length - 1))
        sortedConsts :: [Constval]
        sortedConsts = map fst . List.sortBy (\a b -> compare (snd a) (snd b)) . Map.toList $ objMap
  in folds showObjects . Map.toList $ objFlMap
