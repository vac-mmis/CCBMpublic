-- Generates a DNF representation of the initial states, goal formulas and actions
module RCGenerateTypeHierarchy (generateTypeHierarchy) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe
import Control.Applicative

import Util
import RCData hiding (crnl)
import RCCompile


type Object = Constval
type TypeHierarchy = Map.Map Symbol (Set.Set Symbol)
type ObjectTypes = Map.Map Object (Set.Set Symbol)


renderThAsGraphviz :: [Symbol] -> [(Symbol,[Symbol])] -> [(Symbol,[Object])] -> Bool -> String
renderThAsGraphviz tns ths tm wObjs =
   "digraph typehierarchy {\n" ++
   gvTypeNodes tns ++
   (if wObjs then gvObjectNodes objs ++ gvObjectEdges tm th objs else "") ++
   gvTypeEdges th ++
   "\n}" where
    th = prepareTh tns ths
    objs = List.nub $ concatMap snd tm

-- makes Object parent of all types that have no other parent
-- (doesn't happen in RCCompile for some reason)
prepareTh :: [Symbol] -> [(Symbol,[Symbol])] -> TypeHierarchy
prepareTh tns th = th''' where
    --construct a map type -> [type] that contains object as key
    th' = Map.alter (<|> Just []) objectSymbol (Map.fromList th)
    --construct a map type -> Set type
    th'' = Map.map Set.fromList th'
    --add all parentless types as children of object (except object)
    th''' = foldl (\th t -> if parentless t th then Map.update (return . Set.insert t) objectSymbol th else th) th'' tns
    -- a type is parentless if it's not object and not the child of any other type
    parentless t th = t /= objectSymbol && all (Set.notMember t) (Map.elems th)

-- converts a map type -> [object] to object -> [type]
objectTypes :: [(Symbol,[Object])] -> [Object] -> ObjectTypes
objectTypes tm objs = foldl insertType initialObjs tm where
    initialObjs = Map.fromList $ map (flip (,) Set.empty) objs
    insertType oT (t,os) = foldl (\objTs obj-> Map.update (return . Set.insert t) obj objTs) oT os

gvTypeNodes = unlines . map (show.show)
gvObjectNodes = unlines . map ((++" [shape=box]").show.show)

subtypeOf th t1 t2 = fromJust $
    or <$> mapM (<$> (Set.elems <$> Map.lookup t2 th)) [any (t1==), any (subtypeOf th t1)] <|> Just False

gvTypeEdges :: TypeHierarchy -> String
gvTypeEdges th = Map.foldlWithKey renderType "" th
    where renderType s t childs = concat [s,"\n",
                                      unlines (map (\t' -> concat [(show.show) t, " -> ", (show.show) t']) (Set.toList childs))]

gvObjectEdges :: [(Symbol,[Object])] -> TypeHierarchy -> [Object] -> String
gvObjectEdges tm th objs = objsToGv $ filterRelated $ objectTypes tm objs where
    filterRelated m = Map.map removeSupts m
    removeSupts ts = Set.filter (not . supTypeOfAny ts) ts
    supTypeOfAny ts t1 = Set.foldl (\b t2 -> b || subtypeOf th t2 t1) False ts
    objsToGv = unlines . Map.foldl (flip (:)) [] . Map.mapWithKey objToGv
    objToGv o ts = unlines $ map (\t -> (show.show) o ++ " -> " ++ (show.show) t) $ Set.elems ts

generateTypeHierarchy :: Bool -> CompileResult -> ShowS
generateTypeHierarchy withObj cr = ps $ renderThAsGraphviz (typeNames cr) (typeHierarchy cr) (tmData $ tmap cr) withObj
