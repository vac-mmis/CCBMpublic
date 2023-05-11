-- PDDL pre-processor
module RCPreProc (preproc) where

import System.FilePath
import Data.List

data PPState = PPSt { text :: IO String, -- preprocessed lines
                      filename :: FilePath, -- current file name
                      line :: Int -- current line
                    }

newPPState fname = PPSt (return "") fname 1

preproc :: FilePath -> IO String
preproc fname = preproc' fname (readFile fname)

preproc' :: FilePath -> IO String -> IO String
preproc' fname file = file >>= \file -> text $ foldl' ppline (newPPState fname) (lines file)


-- ppline takes the file until now, the next line, and returns the combined result
ppline :: PPState -> String -> PPState
ppline st ('#':'i':'n':'c':'l':'u':'d':'e':' ':'"':rest) = st { text = news, line = line st + 1 }
    where
          fname = takeWhile (/= '"') rest
          -- makeIncLine produces a line in analogy to the C preprocessor when including a file: ";# line filename flag"
          -- flag: 1 = start of a new file, 2 = return to an old file, 3 = include system head, ignore warning
          makeIncLine line fname flag = ";# " ++ show line ++ " \"" ++ fname ++ "\" " ++ show flag
          news = do
                    -- interprete filename relative to source file
                    inc <- preproc $ replaceFileName (filename st) fname
                    cat (text st) $! makeIncLine 1 fname 1 ++ "\n" ++ inc ++ makeIncLine (line st + 1) (filename st) 2
ppline st x = st { text = cat (text st) x, line = line st + 1 }

cat :: IO String -> String -> IO String
cat s1 s2 = s1 >>= return . (++ (s2 ++ "\n"))
