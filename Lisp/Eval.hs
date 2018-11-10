module Eval where 

import Prelude hiding (lookup)
import Reader 
import Rep 
import StdLib
import Data.Map.Strict hiding (map)
import Data.Maybe
import Debug.Trace 

runProgram :: String -> ([Type], [Term])
runProgram str = do 
    case parseProgram str of 
        Left e -> error e 
        Right prog -> 
            (map (typeof stdlib) prog, map (eval stdlib) prog)

eval :: Bindings -> Term -> Term 
eval b (Atom name) 
    | name `member` b = evalF (b ! name) []
eval b (List ((Atom fname):args)) 
    | fname == "let" = 
        let newBindings = insert (show (args !! 0)) (constF "" (eval b (args !! 1)) Symbol) b in 
            eval newBindings (args !! 2)
    | fname `member` b = 
        let fdef = b ! fname in 
            evalF fdef (map (eval b) args)
eval b x = x