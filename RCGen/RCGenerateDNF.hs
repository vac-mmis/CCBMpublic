-- Generates a DNF representation of the initial states, goal formulas and actions
module RCGenerateDNF (generateDNF) where

import qualified Data.Map.Strict as Map
import qualified Data.List as List

import Util
import RCData hiding (crnl)
import RCCompile


showsFun pre name elem rest = ps pre . shows name . ps ": " . shows elem . rest

makeDNF = foldr (.) (ps "\n") . List.intersperse (ps ":") . flip map [shows . gname, shows . gpre , shows . geff] . flip ($)

generateDNF :: CompileResult -> ShowS
generateDNF cr = inits . goals . ops
    where
        inits = Map.foldrWithKey (showsFun "INITIAL_") (ps "\n") (Map.map mkInitDNF $ initialStates cr)
        mkInitDNF (ils, ifl) = MkDNF [
                                   ils
                                ++ Map.foldrWithKey (\t v -> (:) $ LiteralF . FEqual t . Atomic . Constant $ v) [] ifl
                               ]
        goals = Map.foldrWithKey (showsFun "GOAL_") (ps "\n") (goalDNFs cr)
        ops = foldr (.) id . map makeDNF . possops $ cr
