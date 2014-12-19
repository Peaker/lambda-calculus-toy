{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (and, succ)

import Data.String (IsString(..))
import qualified Data.Set as Set

infixl 9 :@
data LC = Lam String LC
        | Var String
        | LC :@ LC
        deriving (Show)

instance IsString LC where
    fromString = Var

freeVars :: LC -> Set.Set String
freeVars (Lam name body) = Set.delete name (freeVars body)
freeVars (Var name) = Set.singleton name
freeVars (func :@ arg) = freeVars func `Set.union` freeVars arg

subst :: String -> LC -> LC -> LC
subst src dest = go Set.empty
  where
    destVars = freeVars dest
    go shadows (func :@ arg) = go shadows func :@ go shadows arg
    go shadows (Lam name res)
      | name == src = Lam name res -- Shadowed
      | otherwise = Lam name (go (Set.insert name shadows) res)
    go shadows (Var name)
      | name == src =
        if (not . Set.null) (destVars `Set.intersection` shadows)
        then error "Capture-avoiding substituion encountered capture!"
        else dest
      | otherwise = Var name

eval :: LC -> LC
eval (Lam name body) = Lam name (eval body)
eval (Var x) = Var x
eval (func :@ arg) =
    case evalFunc of
    Lam name body -> eval (subst name evalArg body)
    _ -> evalFunc :@ evalArg
    where
        evalFunc = eval func
        evalArg = eval arg

true :: LC
true  = Lam "a" (Lam "b" "a")

false :: LC
false = Lam "a" (Lam "b" "b")

and :: LC
and = Lam "b1" (Lam "b2" ("b1" :@ "b2" :@ false))

zero :: LC
zero = Lam "a" (Lam "b" "a")

succ :: LC
succ = Lam "n1" (Lam "a" (Lam "b" ("b" :@ ("n1" :@ "a" :@ "b"))))

num :: Int -> LC
num = eval . loop
  where
    loop 0 = zero
    loop n = succ :@ loop (n-1)

plus :: LC
plus = Lam "n1" (Lam "n2" ("n1" :@ "n2" :@ succ))

main :: IO ()
main = do
    putStrLn $ show true ++ " == True"
    print (eval (and :@ true :@ true))
    putStrLn ""
    putStrLn $ show false ++ " == False"
    print (eval (and :@ false :@ true))
    print (eval (and :@ true :@ false))
    putStrLn ""
    putStrLn $ show (num 5) ++ " == 5"
    print (eval (plus :@ num 2 :@ num 3))
