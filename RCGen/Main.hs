module Main where

--import Control.Monad

import Control.Monad.State
import System.Environment
import System.IO
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import RCData
import RCParser
import RCCompile
import RCGenerate
import RCGeneratePrism
import RCGenerateDNF
import RCGenerateLoLA
import RCGenerateTypeHierarchy


-- CK: tryit is the main function consisting of multiple stages:
--     1. RCParser.doParseIO: parsing the input domain and problem file stated in the command line options, exit on errors. Result: DomainInfo
--     2. RCCompile.compile: converts DomainInfo, simplifies and expands Domain, exit on errors. Result: CompileResult
--     3. RCGenerate.generatePF: converts CompileResult to C++ code (headers) for compilation with the code in lib directory (prependation)
--     4. print the result

-- | tryit just tries it
tryit :: Options -> IO ()
tryit opts
     = do di <- doParseIO (domainFile opts) (problemFile opts)
          when (not (isValid di)) $ error ("domain is \'"++show(diDomainName di)++"\' but problem wants \'"++show(diPDomainName di)++"\'")
          mapM_ (compileModel opts di) (outForms opts)


compileModel :: Options -> DomainInfo -> OutputFormat -> IO ()
compileModel opts di outf = do
    let ci   = compile . applyDIOpts $ di
        info = ("(:problemInfo ; BEGIN PROBLEM INFO"++)
               . (if extendedInfo opts then shows di . crnl1 else id)
               . case ci of
                   Left msg -> (";;;; Error: "++) . (msg++)
                   Right cr -> if (extendedInfo opts)
                               then shows cr
                               else ("; goal is: "++) . goalInfo cr . crnl1
               . (") ; END PROBLEM INFO"++)
    when (notQuiet opts) $ hPutStrLn stderr (info "")
    case ci of
      Left _   -> return ()
      Right cr -> writeModel opts cr outf
  where crnl1 = ('\n':)
        -- pass required Options to DomainInfo
        applyDIOpts di = di {diWeedOut = weedOut opts, -- weed out actions
                             diResolveCondEff = outf `elem` [Out_PRISM, Out_DNF, Out_LoLA], -- these cannot handle conditional effects
                             diSelectedInit = selectedInit opts,
                             diSelectedGoal = selectedGoal opts
                         }

writeModel :: Options -> CompileResult -> OutputFormat -> IO ()
writeModel opts cr outf = case outf of
    Out_C -> do
        (model, header_defines, header) <- return $ generatePF (boundCheck opts) cr
        writeFile (f ++ ".cpp") (model "")
        writeFile (f ++ ".h") (header "")
        writeFile (f ++ "-defines.h") (header_defines "")
    Out_PRISM ->
        writeFile (f ++ ".prism") (generatePrism cr "")
    Out_DNF ->
        writeFile (f ++ ".dnf") (generateDNF cr "")
    Out_LoLA -> do
        let (model, goals, moreTasks) = generateLoLA cr
        writeFile (f ++ ".lola.hl") (model "")
        mapM_ (\(name, goal) -> writeFile (f ++ ".lola.goal_" ++ show name ++ ".task") (goal "")) $ Map.toList goals
        mapM_ (\(name, formula) -> writeFile (f ++ ".lola." ++ name ++ ".task") (formula "")) $ Map.toList moreTasks
    Out_TypeHierarchy ->
        writeFile (f ++ ".dot") (generateTypeHierarchy (withObjects opts) cr "")
  where
    f = modelFile opts

data OutputFormat = Out_C | Out_PRISM | Out_DNF | Out_LoLA | Out_TypeHierarchy
    deriving (Eq)

getOutputFormat :: Char -> Maybe OutputFormat
getOutputFormat 'C' = Just Out_C
getOutputFormat 'P' = Just Out_PRISM
getOutputFormat 'D' = Just Out_DNF
getOutputFormat 'L' = Just Out_LoLA
getOutputFormat 'T' = Just Out_TypeHierarchy
getOutputFormat _   = Nothing

data Options = Options{
    domainFile :: FilePath,
    problemFile :: FilePath,
    extendedInfo :: Bool,
    outForms :: [OutputFormat],
    notQuiet :: Bool,
    weedOut :: Bool,
    boundCheck :: Bool, -- check bounds of (numeric) fluents in assignments during model run-time
    modelFile :: FilePath, -- where to write the generated model
    withObjects :: Bool,
    selectedInit :: Maybe String, -- only compile using a single initial state / goal
    selectedGoal :: Maybe String
}

initialOptions = Options{
    domainFile = "",
    problemFile = "",
    extendedInfo = False,
    outForms = [Out_C],
    notQuiet = True,
    weedOut = True,
    boundCheck = False,
    modelFile = "",
    withObjects = False,
    selectedInit = Nothing,
    selectedGoal = Nothing
}

checkArgs :: Options -> Either String Options
checkArgs Options{domainFile=""} = Left "missing domain file"
checkArgs Options{problemFile=""} = Left "missing problem file"
checkArgs Options{modelFile=""} = Left "missing model output file"
checkArgs eo = Right eo

parseArgs :: [String] -> Options -> Either String Options
parseArgs [] o = checkArgs o
parseArgs ("-d":f:r) o = parseArgs r o{domainFile=f}
parseArgs ("-p":f:r) o = parseArgs r o{problemFile=f}
parseArgs ("-o":f:r) o = parseArgs r o{modelFile=f}
parseArgs ("-F":f:r) o = let outfs = nub . map getOutputFormat $ f
                         in if Nothing `elem` outfs
                            then Left ("Unknown formats: " ++ f)
                            else parseArgs r o{outForms = map fromJust outfs}
parseArgs ("-q":r) o   = parseArgs r o{notQuiet=False}
parseArgs ("-x":r) o   = parseArgs r o{extendedInfo=True}
parseArgs ("-w":r) o   = parseArgs r o{weedOut=False}
parseArgs ("-b":r) o   = parseArgs r o{boundCheck=True}
parseArgs ("--withObjects":r) o = parseArgs r o{withObjects=True}
parseArgs ("--init":s:r) o = parseArgs r o{selectedInit=Just s}
parseArgs ("--goal":s:r) o = parseArgs r o{selectedGoal=Just s}
parseArgs (s:_) _      = Left ("unrecognized option \'"++s++"\'")

usage pn msg = do putStrLn $ "Error: "++msg
                  putStrLn $ "usage: "++pn++" -d <domain-file> -p <problem-file> -o <output file base> [options...]"
                           ++ "\n\nOptions:"
                           ++ "\n -F        Set output formats, any subset of CPLDT (C, P: PRISM, L: LoLA, D: DNF, T: graphviz type hierarchy)"
                           ++ "\n -w        Do not weed out irrelevant actions"
                           ++ "\n -b        Add run-time bound checks to the generated file (C only)"
                           ++ "\n --withObjects   Include objects in the graphviz type hierarchy file"
                           ++ "\n -x        Print extended diagnostic information"
                           ++ "\n -q        Be quiet"
                           ++ "\n --init    Only compile the given initial state (by name)"
                           ++ "\n --goal    Only compile the given goal formula (by name)"
                           ++ "\n\nThe model output file must be given without a filename extension, which is added by the different formats."

main = do args <- getArgs
          prog <- getProgName
          case parseArgs args initialOptions of
            Left msg -> usage prog msg
            Right o  -> tryit o
