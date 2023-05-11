{
{-# OPTIONS_HADDOCK prune #-}
module RCParser where

import Control.Monad.State
import Control.Arrow
import qualified Data.Map.Strict as Map

import RCData
import RCPreProc
}

%name parseDomain Domain
%name parseProblem Problem
%name parseFormula Formula

%tokentype { Token }
%monad {RCParserMonad} {>>=} {return}
%lexer {lexerM} {TEOF}

%token
        sym                {TSym $$}
        num                {TNum $$}
        int                {TInt $$}
        '('                {TOpen}
        ')'                {TClose}
        '??'               {TPrevVar}
        '?'                {TVar}
        -----------------------------
        '='                {TEquals}
        '-'                {THyphen}
        -----------------------------
        'define'           {TDefine}
        'domain'           {TDomain}
        'problem'          {TProblem}
        -----------------------------
        ':types'           {TTypes}
        ':constants'       {TConstants}
        ':predicates'      {TPredicates}
        ':functions'       {TFunctions}
        ':action'          {TAction}
        ':durative-action' {TDurativeAction}
        ':observation'     {TObservation}
        ':saliency'        {TSaliency}
        ':agent'           {TAgent}
        ':duration'        {TDuration}
        ':parameters'      {TParams}
        ':precondition'    {TPrecond}
        ':effect'          {TEffect}
        ':domain'          {TDDomain}
        ':objects'         {TObjects}
        ':init'            {TInit}
        ':inits'           {TInits}
        ':goal'            {TGoal}
        ':goals'           {TGoals}
        ':landmarks'       {TLandmarks}
        ':irrational'      {TIrrational}
        ':non-repeating'   {TNonRepeating}
        -----------------------------
        'at-start'         {TAtStart}
        'at-end'           {TAtEnd}
        'over-all'         {TOverAll}
        -----------------------------
        ':is-type'         {TIsType}
        -----------------------------
        'and'              {TAnd}
        'or'               {TOr}
        'not'              {TNot}
        'iff'              {TIff}
        'imply'            {TImply}
        'forall'           {TForall}
        'exists'           {TExists}
        'when'             {TWhen}
        '>'                {TGreaterThan}
        '>='               {TGreaterEqual}
        '<'                {TLessThan}
        '<='               {TLessEqual}
        '+'                {TPlus}
        '*'                {TMult}
        '/'                {TDiv}
        'assign'           {TAssign}
        'increase'         {TIncrease}
        'decrease'         {TDecrease}
        'scale'            {TScale}


%%
----------------------------------------------------------------------
-- Domains

Domain : '(' 'define' '(' 'domain' sym ')' Entries ')' {ASTDomain $5 (reverse $7)}


Entries : {[]}
        | Entries Entry {$2:$1}

Entry : Operator                         {$1}
      | '(' ':types' TypeDecls ')'          {ASTTypesEntry $3}
      | '(' ':predicates' PDecls ')'     {ASTPredicatesEntry (reverse $3)}
      | '(' ':constants' ConstDecls ')'  {ASTConstantsEntry $3}
      | '(' ':functions' FluentDecls ')' {ASTFluentsEntry (reverse $3)}
      | '(' ':observation' Observations ')'   {ASTObservationEntry (reverse $3) }

----------------------------------------------------------------------
-- Problems

Problem : '(' 'define' '(' 'problem' sym ')' PEntries ')' {ASTProblem $5 (reverse $7)}

PEntries : {[]} | PEntries PEntry {$2:$1}
 
PEntry : '(' ':domain' sym ')'                        {ASTDomainEntry $3}
       | '(' ':objects' ConstDecls ')'                {ASTObjectsEntry $3}
       | '(' ':inits' MaybeDuration InitListItems ')' {ASTInitEntries $3 (Map.map (EFAnd . reverse) (fst $4)) (Map.map reverse (snd $4))}
       | '(' ':init' MaybeDuration InitItems ')'      {let
                                                        (is, ifun) = $4
                                                        is' = EFAnd . reverse $ is
                                                        if' = reverse ifun
                                                       in ASTInitEntries $3 (Map.singleton allSymbol is') (Map.singleton allSymbol if')}
       | '(' ':goals' GoalList ')'                    {ASTGoalEntries $3}
       | '(' ':goal' Formula ')'                      {ASTGoalEntries $ Map.singleton allSymbol $3}
       | '(' ':landmarks' Formula ')' 			      {ASTLandmarksEntry $3}

-- single :init or :goal entries are named :all, which must exist at least for static function assignments

----------------------------------------------------------------------
-- InitValues

InitItem : CEffect    {Left $1}
         | FunAssign  {Right $1}

FunAssign :: {(Term,Constval)}
FunAssign :  '(' '=' ConstFun Constant ')' {($3,$4)}

-- One initial state
InitItems :: {([EFormula], [(Term,Constval)])}
InitItems : {([],[])}
          | InitItems InitItem { let (cs,fs) = $1 in case $2 of {Left c' -> (c':cs,fs); Right f' -> (cs,f':fs)} }

-- The list of all (named) initial states
InitListItems :: {(Map.Map Symbol [EFormula], Map.Map Symbol [(Term,Constval)])}
InitListItems : '(' '=' sym InitItems ')'               {(Map.singleton $3 (fst $4), Map.singleton $3 (snd $4))}
              | InitListItems '(' '=' sym InitItems ')' {(Map.insertWith (++) $4 (fst $5) (fst $1), Map.insertWith (++) $4 (snd $5) (snd $1))} -- combine declarations with same name

----------------------------------------------------------------------
-- Type Declarations

TypeDecls : TDecls {reverse $1}
          | TDecls Names {reverse ((objectSymbol,reverse $2):$1)}

TDecls : {[]}
       | TDecls TDecl {$2:$1}

TDecl : Names '-' sym {($3,reverse $1)}

Names : sym {[$1]}
      | Names sym {$2:$1}

-- type of variables and fluents

Type : sym { case () of
                    () | $1 == numberTypeSymbol -> Type_NativeNum
                       | $1 == floatTypeSymbol  -> Type_Float
                       | otherwise              -> Type_Object $1 }    -- object fluent with constants of the specific type as value, or numeric fluent when sym="number" or "float"
           | '(' sym int ')' {    -- numeric fluent from 0 to number
                    if $2 == numberTypeSymbol
                        then if $3 <= 0
                            then error "Invalid numeric range"
                            else Type_Num 0 $3
                        else error "only number allowed within (number [from] to) fluent type" }
           | '(' sym int int ')' {    -- numeric fluent from number to number
                    if $2 == numberTypeSymbol
                        then if $3 >= $4
                            then error "Invalid numeric range"
                            else Type_Num $3 $4
                        else error "only number allowed within (number [from] to) fluent type" }

----------------------------------------------------------------------
-- Objects & Constants Declarations

ConstDecls : CDecls {reverse $1}
           | CDecls Constants {reverse ((objectSymbol,reverse $2):$1)}

CDecls : {[]}
       | CDecls CDecl {$2:$1}

CDecl : Constants '-' sym {($3,reverse $1)}

Constants : Constant {[$1]}
          | Constants Constant {$2:$1}

----------------------------------------------------------------------
-- Predicates Declarations

PDecls : {[]} | PDecls PDecl {$2:$1}
PDecl : '(' sym Decls ')' {PDecl $2 (VDecls (reverse $3))}
      | '(' sym Decls Vars ')' {PDecl $2 (VDecls (reverse ((Type_Object objectSymbol,reverse $4):$3)))}

----------------------------------------------------------------------
-- Fluents

FluentDecls : {[]}
            | FluentDecls FluentDecl { $2:$1 }

FluentDecl : Functions '-' Type {($3, reverse $1)}

Functions : {[]}
          | Functions Function {$2:$1}

Function : '(' sym FunctionVarDecls ')' {Fluent{flsym=$2, fldecl=$3}}

FunctionVarDecls : Decls {VDecls (reverse $1)}
       | Decls Vars {VDecls (reverse ((Type_Object objectSymbol, reverse $2):$1))}

----------------------------------------------------------------------
-- Operators

Operator : '(' ':action' sym
                IsIrrational
                MaybeParams
                MaybeNonRepeating
                MaybeSaliency
                MaybeAgent
                MaybeDuration
                MaybePrecondition
                MaybeEffect
		MaybeObservations
            ')'                             {ASTActionEntry (Action{asym=$3,airr=$4, adecl=$5,apff=$6,asal=$7,aagnt=$8,adur=$9,apre=$10,aeff=$11,acbk=$12})}
         | '(' ':durative-action' sym
                IsIrrational
                MaybeParams
                MaybeNonRepeating
                MaybeSaliency
                MaybeAgent
                MaybeDuration
                MaybeTimedPrecondition
                MaybeTimedEffect
		MaybeObservations
            ')'                             {ASTActionEntry (DurativeAction{asym=$3,airr=$4,adecl=$5,apff=$6,asal=$7,aagnt=$8,adur=$9,dapre=$10,daeff=$11,acbk=$12})}

MaybeSaliency :                             {Nothing}
              | ':saliency' Term {Just $2}

MaybeAgent :                                {Nothing}
           | ':agent' AtomicExpression      {Just (Atomic $2)}

MaybeParams :                               {Nothing}
            | ':parameters' XDecls          {Just $2} 

MaybeDuration :                             {Nothing}
              | ':duration' NumAtom         {Just $2}

MaybePrecondition :                         {Nothing} 
                  | ':precondition' Formula {Just $2}
                  | ':precondition' '(' ')' {Nothing}

MaybeEffect :                               {Nothing}
            | ':effect' Effect              {Just $2}
            | ':effect' '(' ')'             {Nothing}

MaybeTimedPrecondition :                              {Nothing} 
                       | ':precondition' TimedFormula {Just $2}
                       | ':precondition' '(' ')'      {Nothing}

MaybeTimedEffect :                          {Nothing}
                 | ':effect' TimedEffect    {Just $2}
                 | ':effect' '(' ')'        {Nothing}

MaybeObservations :						{Nothing}
		  | ':observation' Callback			{Just $2}
		  | ':observation' '(' 'and' Callbacks ')'	{Just (EFAnd (reverse $4))}

IsIrrational :              { False }
          | ':irrational'   { True }

MaybeNonRepeating :           {Nothing}
          | ':non-repeating' MaybePrevFunFormula {Just $2}

MaybePrevFunFormula : {Nothing}
  | Formula {Just $1}


----------------------------------------------------------------------
-- Parameter Declarations

XDecls : '(' Decls ')' {VDecls (reverse $2)}
       | '(' Decls Vars ')' {VDecls (reverse ((Type_Object objectSymbol,reverse $3):$2))}

Decls : {[]}
      | Decls Decl {$2:$1}

Decl : Vars '-' Type {($3,reverse $1)}

Vars : Var {[$1]}
     | Vars Var {$2:$1}

Var : '?' sym {$2}

PrevFunVar: '??' sym {$2}

----------------------------------------------------------------------
-- Precondition Formulae

-- named list of formulas
-- note: during compilation, the formula in :all (if any) is added as conjunction to all other formulas
GoalList :: {Map.Map Symbol Formula}
GoalList : '(' '=' sym Formula ')'          {Map.singleton $3 $4}
         | GoalList '(' '=' sym Formula ')' {Map.insertWith (\g1 g2 -> FAnd [g1, g2]) $4 $5 $1} -- conjunct goal formulas with same name

Formula : Atom                              {$1}
        | '(' SpecialFormula ')'            {$2}
        | '(' 'and' Formulae ')'            {FAnd (reverse $3)}
        | '(' 'or' Formulae ')'             {FOr (reverse $3)}
        | '(' 'not' Formula ')'             {FNot $3}
        | '(' 'imply' Formula Formula ')'   {FImply $3 $4}
        | '(' 'iff' Formula Formula ')'     {FIff $3 $4}
        | '(' 'forall' XDecls Formula ')'   {FForall $3 $4}
        | '(' 'exists' XDecls Formula ')'   {FExists $3 $4}
        | LitFormula                        {FLitForm $1}

LitFormula : '(' '=' Term Term ')'             {FEqual $3 $4}
           | '(' '>' Term Term ')'             {FGreaterThan $3 $4}
           | '(' '>=' Term Term ')'            {FGreaterEqual $3 $4}
           | '(' '<' Term Term ')'             {FLessThan $3 $4}
           | '(' '<=' Term Term ')'            {FLessEqual $3 $4}

Formulae : {[]} | Formulae Formula {$2:$1}

TimeSpecifier : 'at-start' {AtStart}
              | 'at-end' {AtEnd}
              | 'over-all' {OverAll}

TimedFormula : '(' TimeSpecifier Formula ')' {TimedF $2 $3}
             | '(' 'and' TimedFormulae ')'   {TFAnd (reverse $3)}

TimedFormulae : {[]} | TimedFormulae TimedFormula {$2:$1}
 
Atom : '(' sym Terms ')' {% pushPred $2 $3}
     | sym               {% pushPred $1 []}

AtomicExpression : PrevFunVar {PrevFunVar $1}
                 | Var {Variable $1}
                 | Constant {Constant $1}

Term : AtomicExpression              {Atomic $1}
     | '(' sym Terms ')'             {Function $2 (reverse $3)}
     | '(' '+' Term Term ')'         {FFun $ FPlus $3 $4}
     | '(' '-' Term Term ')'         {FFun $ FMinus $3 $4}
     | '(' '*' Term Term ')'         {FFun $ FMult $3 $4}
     | '(' '/' Term Term ')'         {FFun $ FDiv $3 $4}

Terms: {[]} | Terms Term {$2:$1}

Constant : sym {Symconst $1}
         | num {Numconst $1}
         | int {Intconst $1}

Consts : {[]} | Consts Constant { Constant $2:$1 }

ConstFun :: {Term}
ConstFun : sym {Function $1 []}
         | '(' sym Consts ')' {Function $2 (map Atomic (reverse $3))}


NumAtom : '(' sym Terms ')' {Atom $2 (reverse $3)}

SpecialFormula : TypeCheck  {$1}

TypeCheck : ':is-type' Term Type {FIsType $2 $3}

----------------------------------------------------------------------
-- Effect Formulae

TimedEffect : '(' TimeSpecifier Effect ')' {TimedEF $2 $3}
            | '(' 'and' TimedEffects ')'   {TEFAnd (reverse $3)}

TimedEffects : {[]} | TimedEffects TimedEffect {$2:$1}
 
Effect : '(' 'and' CEffects ')' {EFAnd (reverse $3)}
       | CEffect {$1}

CEffects : {[]} | CEffects CEffect {$2:$1}

CEffect : EAtom { $1 }
        | '(' 'not' EAtom ')'            { EFNot $3 }
        | '(' 'assign' Term Term ')'     { EFAssign $3 $4 }
        | '(' 'increase' Term Term ')'   { EFAssign $3 (FFun $ FPlus $3 $4) }
        | '(' 'decrease' Term Term ')'   { EFAssign $3 (FFun $ FMinus $3 $4) }
        | '(' 'scale' Term Term ')'      { EFAssign $3 (FFun $ FMult $3 $4) }
        | '(' 'forall' XDecls Effect ')' { EFForall $3 $4 }
        | '(' 'when' Formula Effect ')'  { EFWhen $3 $4 }

-- note: pushDPred is required for enumertaing all dynamic predicates, i.e. those changed by effects
-- therefore, we need to distinguish between Atom and EAtom
EAtom : '(' sym Terms ')' {% pushDPred $2 $3}
      | sym               {% pushDPred $1 []}

----------------------------------------------------------------------
-- Observation Formula

Observation : '(' 'forall' XDecls Observation ')' {EFForall $3 $4}
            | '(' 'forall' XDecls '(' 'and' Observations ')' ')' {EFForall $3 (EFAnd (reverse $6)) }
            | '(' 'when'  Formula Obsbody ')' {EFWhen $3 $4}

Obsbody : ObsAtom                {$1}
        | '(' 'and' ObsAtoms ')' {EFAnd (reverse $3)}

ObsAtoms : {[]} | ObsAtoms ObsAtom {$2:$1}

ObsAtom : '(' sym Terms ')' {EFAtom $2 (reverse $3)}

Observations : {[]} | Observations Observation {$2:$1}

Callback  : ObsAtom   		   {EFWhen (FAnd[]) $1}
	      | Observation        {$1}
Callbacks : Callback   	       {[$1]}
	      | Callbacks Callback {$2:$1}

----------------------------------------------------------------------


{

-- CK: doParse uses the generated monadic (State monad) happy parser to parse the domain and the problem file (functions: parseDomain/parseProblem)
--     into an abstract syntax tree (the entries of the domain/problem are in the order given in the input file)
--     In a second step, the abstract syntax trees are converted to a Domain and a Problem structure, respectively, by the consolidateFile function.
--     In these structures, the order of the elements is fixed and AST elements of the same type (e.g. constant declarations) are consolidated.
--     Besides these two structures, a list of all used predicates and a list of all dynamic predicates (predicates used in action effects) are returned.

-- | 'doParse': parses two strings as Domain and Problem
doParse :: String -> String -> RCParserMonad DomainInfo
doParse doms probs =
        do state <- get
	   put state {input=doms,file="domain"}
           dom <- parseDomain
           state <- get
	   let dp = dpred state -- taking dynamics only from Domain makes sure we don't count inits as dynamic
           put state {input=probs,file="problem",line=1}
           prob <- parseProblem
	   state' <- get
	   let ap = preds state' -- taking statics from Domain & Problem makes sure that we also include predicates required by goal
               Right domi = consolidateFile dom
               Left  probi = consolidateFile prob
           return DomainInfo{diDomain=domi,
                             diProblem=probi,
                             diAllPreds=ap,
                             diDynPreds=dp,
                             diWeedOut=True}


-- CK: doParseIO encapsulates the IO operations. It reads the input files and hands them over (as Strings) to the doParse method

doParseIO dompath probpath
        = do doms <- preproc dompath
             probs <- preproc probpath
             return $ evalState (doParse doms probs) initialState

doParseFormula s = evalState parseFormula (initialState{input=s})

instance Read Formula where
         readsPrec p s = [(doParseFormula s,"")]

}
