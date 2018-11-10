module Rep where 

import Data.Map.Strict hiding (map)
import Data.List

data Term = Lit String
    | List [Term] 
    | Num Double 
    | Atom String 
    | Let Term Term Term 
    deriving (Eq)

instance Show Term where 
    show (Lit s) = s 
    show (Num x) = show x 
    show (Atom s) = s 
    show (List terms) = "(" ++ (intercalate " " . map show $ terms) ++ ")"

data Type = Boolean 
    | Number
    | ListT Type 
    | FuncT [Type] Type
    | Symbol 
    | Gen String
    deriving (Show, Eq, Ord)

isGen :: Type -> Bool 
isGen (Gen _) = True 
isGen _ = False 

returnType :: Type -> Type 
returnType (FuncT _ r) = r 
returnType x = x

data Function = Func 
  { name :: String 
  , typeSig :: Type -- (output, inputs)
  , evalF :: [Term] -> Term
  }

instance Show Function where 
    show f = name f

type Bindings = Map String Function 