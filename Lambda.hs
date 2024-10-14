module Lambda where

import Data.List (nub, (\\), sort, intersect, union)
import Data.Foldable (find)

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var x) = [x]
vars (App x y) = nub (vars x ++ vars y)
vars (Abs x y) = nub (x : vars y)
vars _ = []

-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (App x y) = nub (freeVars x ++ freeVars y)
freeVars (Abs x y) = filter (/= x) (freeVars y)
freeVars _ = []

aux :: String -> String
aux [] = ['a']
aux ('z' : xs) = 'a' : aux xs
aux (x : xs) = succ x : xs

inc :: String -> String
inc = reverse . aux . reverse

help :: [String] -> String -> String
help xs x = case find (== x) xs of
  Just _ -> help xs (inc x)
  _ -> x

-- 1.3.
newVar :: [String] -> String
newVar l = help (sort l) "a"

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var x) = True
isNormalForm (Abs x y) = isNormalForm y
isNormalForm (App x y) = case x of
  Abs _ _-> False
  _ -> isNormalForm x && isNormalForm y

replaceVar :: String -> String -> Lambda -> Lambda
replaceVar old new (Var x)
  |x == old =  Var new
  |otherwise = Var x
replaceVar old new (Abs x y)
  |x == old = Abs new (replaceVar old new y)
  |otherwise = Abs x (replaceVar old  new y)
replaceVar old new (App x y) = App (replaceVar old new x) (replaceVar old new y)

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
-- find all bound vars from e1 that are free in e2
reduce x e1 e2 = case  ((vars e1 \\ freeVars e1) `intersect` freeVars e2) \\ [x] of 
  [] -> case e1 of
    Var y
      | y == x -> e2
      | otherwise -> Var y
    Abs a b
        |a == x -> e1
        | otherwise -> Abs a (reduce x b e2)
    App a b -> App (reduce  x a e2) (reduce x b e2)

-- replace the first var with a new var that isnt in e1 and e2 and reduce again
  y : ys -> reduce x (replaceVar y (newVar (vars e1 `union` vars e2)) e1) e2

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (Var x) = Var x
normalStep (Abs x y) = Abs x (normalStep y)
-- reduce big if aplication of funtion, else left first
normalStep (App x y) = case x of
  Abs a b -> reduce a b y
  _ -> if isNormalForm x
    then App x (normalStep y) else App (normalStep x) y

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (Var x) = Var x
applicativeStep (Abs x y) = Abs x (applicativeStep y) 
-- reduce big aplication only if cant reduce argument, else left first
applicativeStep (App x y) = case x of
  Abs a b -> if isNormalForm y
    then reduce a b y else App x (applicativeStep y)
  _ -> if isNormalForm x
    then App x (applicativeStep y) else App (applicativeStep x) y

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify f x
  | isNormalForm x = [x]
  | otherwise = x : simplify f (f x)


normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
