module StdLib where 

import Prelude hiding (div)
import Rep
import Data.Map.Strict hiding (map)

stdlib = fromList . map (\fdef -> (name fdef, fdef)) $
    [ andF
    , orF
    , add
    , sub
    , mult 
    , div
    , constF "pi" (Num pi) Number
    , constF "true" (Atom "true") Boolean
    , constF "false" (Atom "false") Boolean
    , ifF
    , eqF]

constF :: String -> Term -> Type -> Function 
constF str term typ = Func str 
    (FuncT [] typ)
    (\[] -> term)

typeF :: Type -> Function 
typeF t = Func "" 
    (FuncT [] t)
    (\_ -> undefined)

ifF :: Function 
ifF = Func "if"
    (FuncT [Boolean, Gen "a", Gen "a"] (Gen "a"))
    (\(cond:b1:b2:[]) -> if cond == Atom "true" then b1 else b2)

eqF :: Function 
eqF = Func "="
    (FuncT [Gen "a", Gen "a"] Boolean)
    (\(x:y:[]) -> Atom . show $ x == y)

andF :: Function 
andF = Func "and" 
    (FuncT [Boolean, Boolean] Boolean)
    (\(t1:t2:[]) -> if t1 == Atom "true" then t2 else t1)

orF :: Function 
orF = Func "or"
    (FuncT [Boolean, Boolean] Boolean)
    (\(t1:t2:[]) -> if t1 == Atom "false" then t2 else t1)

add :: Function 
add = Func "+"
    (FuncT [Number, Number] Number)
    (\((Num x):(Num y):[]) -> Num $ x + y)

sub :: Function 
sub = Func "-"
    (FuncT [Number, Number] Number)
    (\((Num x):(Num y):[]) -> Num $ x - y)

mult :: Function 
mult = Func "*"
    (FuncT [Number, Number] Number)
    (\((Num x):(Num y):[]) -> Num $ x * y)

div :: Function 
div = Func "/"
    (FuncT [Number, Number] Number)
    (\((Num x):(Num y):[]) -> Num $ x / y)