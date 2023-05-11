import Control.Monad
import Text.Printf

lo=1
up=5

valid x = x >= lo && x <= up

positions = [(x,y) | x <- [lo..up], y <- [lo..up]]

neighbors (x,y) = [(x',y') | x'<-[x-1..x+1]
                           , y'<-[y-1..y+1]
                           , valid x' && valid y'
                           , (x',y') /= (x,y) 
                           , x==x' || y==y']

matrix = [(p,p') | p <- positions, p' <- neighbors p] 


showspos (x,y) = shows x . shows y

genprob = do putStr "(define (problem xyrouter)\n    (:domain xyrouter)"
             putStr "\n    (:objects\n\t"
             putStr $ foldr (\e -> showspos e . (' ':)) "" positions
             putStr "- position\n\t"
             putStr $ foldr (\e -> shows e . (' ':)) "" [lo..up]
             putStr "- coordinate)"
             putStr "\n    (:init\n\t(at 11)"
             putStr $ foldr (\(p,q) -> ("\n\t(connected "++) . showspos p . (' ':) . showspos q . (')':)) "" matrix
             putStr $ foldr (\(x,y) -> ("\n\t(xy-pos "++) . shows x . shows y . (' ':) . shows x . (' ':) . shows y .  (')':)) "" positions
             putStr ")"
             putStr "\n    (:goal (at 55))"
             putStrLn ")"

main = genprob