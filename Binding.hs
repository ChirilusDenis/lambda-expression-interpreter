module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- either e monad, left ~ nothing && right ~ just x
replaceMacro :: Context -> Lambda -> Either String Lambda
replaceMacro ctx (Var x) = do return (Var x)
replaceMacro ctx (Abs v l) = do 
    lamba <- replaceMacro ctx l
    return (Abs v lamba)
replaceMacro ctx (App l1 l2) = do
    lambda1 <- replaceMacro ctx l1
    lambda2 <- replaceMacro ctx l2
    return (App lambda1 lambda2)
replaceMacro ctx (Macro m) = case lookup m ctx of
    Just l -> Right l
    Nothing -> Left "ERROR"


-- 3.1.
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx strat e = do
    lambda <- replaceMacro ctx e
    return (simplify strat lambda)
   
normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
