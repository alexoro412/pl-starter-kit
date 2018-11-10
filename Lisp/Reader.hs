{-# LANGUAGE LambdaCase #-}
module Reader where 

import Rep
import Prelude hiding (lookup)
import Parser
import Control.Applicative
import Data.Char 
import Data.Maybe (isJust)
import Data.Map.Strict hiding (map, foldr)
import StdLib
import Debug.Trace

-- Type checker -- 

typeCheck :: Bindings -> Term -> Bool 
typeCheck b prog = (typeof b prog) `seq` True

typeof :: Bindings -> Term -> Type 
typeof b (Lit _) = error "Woah... woops. My bad."
typeof b (Num _) = Number 
typeof b (Atom name) 
    | name `member` b = returnType $ typeSig (b ! name) 
typeof b (Atom _) = Symbol
typeof b (List ((Atom name):args)) 
    | name == "let" = 
        let newBindings = insert (show (args !! 0)) (typeF (typeof b (args !! 1))) b in
                typeof newBindings (args !! 2)
    | name `member` b = 
            let fType = typeSig (b ! name) in 
                checkFType fType (map (typeof b) args)
                -- && and (zipWith (==) types (map (typeof b) args))
                    -- then returnType
                    -- else errorWithoutStackTrace $ "Incorrect arity in function call: " ++ name
typeof b (List terms) = let types = map (typeof b) terms in 
    if all (== head types) (tail types) 
        then ListT $ head types 
        else errorWithoutStackTrace "Type error: Non-homogeneous list"

checkFType :: Type -> [Type] -> Type   
checkFType (FuncT inputTs outputT) argTs = 
    if any isGen inputTs || isGen outputT 
        then let pairs = zip inputTs argTs 
                 tmap = foldr (\(itype, atype) typeMap -> 
                        if itype `member` typeMap 
                            then if typeMap ! itype == atype
                                then typeMap 
                                else errorWithoutStackTrace $ "Type check in fcall"
                        else insert itype atype typeMap) Data.Map.Strict.empty pairs
                in if isGen outputT then tmap ! outputT else outputT
        else if inputTs == argTs 
            then outputT 
            else error "Type check in fcall"

-- this thing! -- 

replaceLits :: Term -> Term 
-- replaceLits (LetList terms) = LetList $ map (\(x,y) -> (replaceLits x, replaceLits y)) terms
replaceLits (List terms)
    | head terms == (Atom "let") = 
        if length terms /= 4 
        then error "Parse Error: Let does not have enough args"
        else Let (terms !! 1) (terms !! 2) (terms !! 3)
    | otherwise = List $ (map replaceLits terms)
replaceLits (Lit str)
    | isDigit $ head str = Num $ read str 
    | otherwise = Atom str 

-------------------------------------------
                -- PARSING --

-- does runParser always return a list?
parseProgram :: String -> Either String [Term]
parseProgram str = (headMaybe $ runParser (many (skipSpaces *> parseTerm)) str) & (\case 
    Just (tokens, leftover) -> case leftover of 
                "" -> Right $ map replaceLits tokens 
                x  -> Left $ "Error parsing this part: " ++ x
    Nothing -> Left $ "It couldn't parse that at all...")

parseTerm :: Parser Term
parseTerm = parseUnit <++ parseList <++ parseLit

-- blatantly copied from Parsec
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
    optional sep 
    x <- p
    xs <- many (sep >> p)
    optional sep 
    return (x:xs)
    
parseUnit :: Parser Term 
parseUnit = string "()" *> pure (Lit "()")

parseList :: Parser Term 
parseList = List <$> (char '(' *> parseTerm `sepBy1` space <* char ')')

-- parseLetList :: Parser Term 
-- parseLetList = do 
--     char '[' 
--     terms <- parseTerm `sepBy1` space 
--     char ']'
--     pure . LetList . pairUp $ terms

pairUp :: [Term] -> [(Term, Term)]
pairUp (x:y:rs) = (x,y) : pairUp rs 
pairUp [] = []

isParen :: Char -> Bool 
isParen '(' = True 
isParen ')' = True 
isParen _   = False 

parseLit :: Parser Term 
parseLit = Lit <$> some (satisfy (not . ((||) <$> isSpace <*> isParen)))

headMaybe :: [a] -> Maybe a 
headMaybe (a:_) = Just a 
headMaybe [] = Nothing

(&) = flip ($) 

space :: Parser Char 
space = satisfy isSpace 

skipSpaces :: Parser () 
skipSpaces = many space *> pure ()
