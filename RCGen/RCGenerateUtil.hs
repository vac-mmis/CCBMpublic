module RCGenerateUtil where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.List as List

import Util
import RCData hiding (crnl)
import RCCompile

crnl = ('\n':)

lukup lut a = case Map.lookup a lut of
                Nothing -> error ("unable to find "++show a++" in map")
                Just x  -> x

-- four lookup tables are used: Fluent -> Type, Predicate → Int, Fluent → Int, Fluentname → (value → Int)
data Luts = Luts {
    -- For every type name, the list of possible values
    luTypeMap :: Map Symbol [Constval],
    -- for every constant (of some type), a uniqe id
    luObjConstIDs :: Map Constval Int,

    -- unique name for every atom (used to generate state variable names)
    luAtoms :: [(Atom, String)], -- we need to preserve the order for generating state struct, therefore as list and Map
    luAtom :: Map Atom String,
    -- unique name for every fluent (used to generate state variable names)
    luFluents :: [(Term, String)], -- also need to preserve the order
    luFluent :: Map Term String,
    -- unique indices for both atoms and fluents (order of state variables, used e.g. for the rT array)
    luAtomIdx :: Map Atom Int,
    luFluentIdx :: Map Term Int,

    -- For every fluent, its associated type
    luFlTypes :: Map Term Type,
    -- for every object fluent type (its symbol), a map from possible value (atom constant) to int representing this constant
    luObjFlMap :: Map Symbol (Map Constval Int),

    -- a name and id for every ground action
    luActions :: Map GAction (String, Int),
    -- "labeled", i.e. indexed list of all named initial effects
    luInitEffects :: [(Int, (Symbol, [CEffect]))],
    -- indexed list of all named goal states / DNFs
    luGoals :: [(Int, (Symbol, DNF))]
}

-- build maps and lists for use in the code generation process
-- cr: the CompileResult
-- flcompare: function passed to List.sortBy to determine the order of the fluents
-- escape: a function to escape atom and action names for the target language (e.g. "(at object location)" to "at_object_location")
--          The escape function is not required to produce a unique escape string, possible clashes are resolved by adding unique numbers
buildLuts :: CompileResult -> (Term -> Term -> Ordering) -> (String -> String) -> Luts
buildLuts cr flcompare escape = Luts {
    luTypeMap   = typemap,
    luObjConstIDs = objConstIDs,
    luAtoms     = atoms,
    luAtom      = atomLut,
    luFluents   = fluents,
    luFluent    = fllut,
    luAtomIdx   = atomIdx,
    luFluentIdx = fluentIdx,
    luFlTypes   = flTypes,
    luObjFlMap  = objFlMap,
    luActions   = actionNames,
    luInitEffects = initEffects,
    luGoals     = goals
    }
  where
    typemap = Map.fromList . tmData . tmap $ cr

    -- assign each used constant a unique id
    objConstIDs = Map.fromList $ zip (List.nub . concat . Map.elems $ typemap) [0..]

    -- build Map a -> unique escaped string, while using a Map string -> count to identify possible clashes during escaping
    -- used to escape atom/fluent names and generate unique names
    buildNameMap :: Ord a => (a -> String) -> (Map a String, Map String Int) -> a -> (Map a String, Map String Int)
    buildNameMap show_a (lut, stringmap) a = let
        name = escape $ show_a a -- use user-provided show function
        nameCount = Map.findWithDefault 0 name stringmap
        name' = if nameCount == 0 then name else name ++ show (nameCount + 1)
      in
        (Map.insert a name' lut, Map.insert name (nameCount + 1) stringmap)

    -- named map/list of all boolean atoms / fluents
    atomLut = fst $ List.foldl' (buildNameMap show) (Map.empty, Map.empty) (universe cr)
    atoms = Map.toList atomLut

    flTypes = univFluents cr

    fllut = fst $ List.foldl' (buildNameMap show) (Map.empty, Map.empty) (List.sortBy flcompare $ Map.keys flTypes)
    fluents = Map.toList fllut

    -- assign unique ids to predicates and fluents, so the rT array, for instance, can be easily used
    (lastAtomIdx, atomIdx) = Map.mapAccum(\idx _ -> (idx + 1, idx)) 0 atomLut 
    fluentIdx = snd $ Map.mapAccum(\idx _ -> (idx + 1, idx)) lastAtomIdx fllut 

    -- for every object fluent type, generate a map from possible value (atom constant) to int representing this constant
    objFlMap :: Map Symbol (Map Constval Int)
    objFlMap = Map.fromList $ zip types (map flTypeMap types)
      where
            types = List.nub . foldr collectObjType [] . Map.elems $ flTypes
            collectObjType (Type_Object s) = (s:)
            collectObjType _ = id
            -- for a specific type, generate a lut from constant to int
            flTypeMap :: Symbol -> Map Constval Int
            flTypeMap s = Map.fromList $ zip (values s) [0..]
            values s = case Map.lookup s typemap of
                          Nothing -> error $ "Type not found: " ++ show s
                          Just vs -> vs

    actionNames =
        let names = fst $ List.foldl' (buildNameMap (($"") . foldsIntersperse (ps " ") shows . gname)) (Map.empty, Map.empty) (possops cr)
        in  snd $ Map.mapAccum(\idx name -> (idx + 1, (name, idx))) 0 names

    -- "labeled", i.e. indexed list of all named initial effects
    initEffects = zip [0..] (Map.toList effMap)
     where effMap = Map.map (\(ils, ifl) -> map Unconditional $ mapLits ils ++ mapFluents ifl) (initialStates cr)
           mapLits = map EPredicate
           mapFluents = map (\(f, v) -> EAssignment f . Atomic . Constant $ v) . Map.assocs
    -- indexed list of all named goal states / DNFs
    goals = zip [0..] . Map.toList $ goalDNFs cr


-- returns the specificity of a predicate in the universe
specLookup :: Map Literal Int -> Literal -> Int
specLookup lut a = case Map.lookup  a lut of
                    Nothing -> 1
                    Just x -> x


-- when comparing numeric fluents with constants or each other, we have to keep in mind that
-- restricted numeric fluents -- e.g. (number -2 6) -- are saved as values starting from 0 -- e.g. 0..8
-- this functions computes the offset to zero-based fluents, esp. NativeNum and Float
fluentOffset :: Map Term Type -> Term -> Int
fluentOffset flTypes f = case flTypes `lukup` f of
    Type_Object s   -> error $ "Trying to get offset of object fluent of type " ++ show s
    Type_Num from _ -> from
    Type_NativeNum  -> 0
    Type_Float      -> 0


-- The Landmark Algorithm
-- First we will implement the base algorithm. There are many optimizations possible.
-- The base algorithm only finds necessary orderings

-- main function: LG:=(Nodes,Edges) where Nodes are Literals/Landmarks, LG is the Landmarkgraph
-- the function returns the Landmarkgraph for the given problem
-- the algorithm starts from all known Landmarks ls (at first only the goal literals without literals which are also in the initial state)

landmarkAlg cr goal initLits (Luts{luAtom=lut}) = let
                        userl = [Literal True a | a<- (userLandmarks cr)]
                        starts = filter (not.isfluent) $ userl ++ (literals goal) -- currently ignore fluents for landmarks
                      in evalL starts (starts,[]) initLits
                      where
                       isfluent (Literal _ _) = False
                       isfluent (LiteralF _)  = True
                       evalL [] lg _ = lg
                       evalL (n:ns) (lgvs,lges) initLits = case List.find (==n) initLits of
                                                            Just _ -> evalL ns (lgvs,lges) initLits    -- next one
                                                            Nothing -> let     -- landmark, handle it
                                                                    gops = [ gop |  gop <- possops cr ,  findL n $ filter (not.isfluent) $ efflits gop ] -- grounded actions which have n as effect
                                                                        where
                                                                            findL n g = case List.find (==n) g of
                                                                                            Nothing -> False
                                                                                            Just _ -> True
                                                                    hgops | gops == [] = []
                                                                          | otherwise  = filter (not.isfluent) $ prelits $ head gops
                                                                    sgops = drop 1 gops
                                                                    sharedPrec = filter (filterUniv) (getshared hgops [ filter (not.isfluent) $ prelits g | g <- sgops] ) -- maybe there are duplicates, because of the DNF
                                                                            where
                                                                                filterUniv (Literal b x) | b==False = False
                                                                                                         | otherwise = case Map.lookup x lut of -- only positive literals
                                                                                                                            Nothing -> False
                                                                                                                            Just _ -> True
                                                                                filterUniv _ = undefined
                                                                    lgesnew = List.nub $ lges ++ [ (l,n) | l <- sharedPrec ]
                                                                    (lgvsnew,nsnew) = notin sharedPrec (lgvs,[])
                                                                   in evalL (ns++nsnew) (lgvsnew,lgesnew) initLits
                                                                   where
                                                                    getshared [] _ = []
                                                                    getshared is [] = is
                                                                    getshared is (x:xs)= getshared (List.intersect is x) xs
                                                                    notin [] (ls,nes) = (ls,nes)
                                                                    notin (x:xs) (ls,nes) = case List.find (==x) ls of
                                                                                                Nothing -> notin xs ((x:ls),(x:nes))-- add it
                                                                                                Just _ -> notin xs (ls,nes)
-- Landmark Algorithm end
