module Main where 

import Eval 
import Rep 
import System.IO

repl :: IO () 
repl = do
    putStr "> "
    hFlush stdout 
    str <- getLine 
    case str of 
        'q':_ -> putStrLn "\n"
        _ -> do
            let (types, values) = runProgram str 
            mapM_ showVal (zip types values)
            repl


showVal :: (Type, Term) -> IO ()
showVal (typ, val) = do
        putStr (show val) 
        putStr " :: "
        putStrLn (show typ)

main = repl