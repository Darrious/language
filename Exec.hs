module Exec where

import Types

lookUp :: Vars -> Env -> Values
lookUp x [] = VNull
lookUp x ((a, IntPrim num) : b)   = if (a == x) then IntPrim num else (lookUp x b)
lookUp x ((a, BoolPrim bool) : b) = if (a == x) then BoolPrim bool else (lookUp x b)


-- update x v e sets the values of x to v and keeps other variables in e the same
-- todo: check if updated value matches type of value
update :: Vars -> Values -> Env -> Env
update x y []  = [(x,y)]
update x prim env = if (length(filterEnv x env) > 0)
                 then updateEnv x prim env
                 else env ++ [(x, prim)]
filterEnv :: Vars -> Env -> Env
filterEnv x env = filter(\(a, _) -> (x==a)) env

updateEnv :: Vars -> Values -> Env -> Env
updateEnv x (IntPrim prim) env = [if (a == x) then (x, IntPrim prim) else (a, b) | (a, b) <- env]
updateEnv x (BoolPrim prim) env = [if (a == x) then (x, BoolPrim prim) else (a, b) | (a, b) <- env]


extractInt :: Values -> Vars -> Integer
extractInt (IntPrim x) _ = x
extractInt x var = error ("Actual value (" ++ show x ++
                          ") - Expected IntPrim -  Make sure var "
                         ++ (show var) ++ " has been initialized.")

extractBool :: Values -> Vars -> Bool
extractBool (BoolPrim x) _ = x
extractBool x var = error ("Actual value (" ++ show x ++
                       ") - Expected BoolPrim -  Make sure var "
                      ++ (show var) ++ " has been initialized.")

-- Evaluate
evala :: Env -> AExpr -> Integer
evala env (Const (IntPrim a)) = a
evala env (Var v)   = extractInt(lookUp v env) v
evala env (Add a b) = evala env a + evala env b
evala env (Sub a b) = evala env a - evala env b
evala env (Mul a b) = evala env a * evala env b
evala env (Div a b) = evala env a `div` evala env b
evala env (Mod a b) = evala env a `mod` evala env b

testA = Add (Var "X") (Const (IntPrim 9))

evalb :: Env -> BExpr -> Bool
evalb env TT        = True
evalb env FF        = False
evalb env (And a b) = (evalb env a) && (evalb env b)
evalb env (Or  a b) = (evalb env a) || (evalb env b)
evalb env (Eql a b) = (evala env a) == (evala env b)
evalb env (Lt a b)  = (evala env a) < (evala env b)
evalb env (Gt a b)  = (evala env a) > (evala env b)
evalb env (Not b)   = (not (evalb env b))

boolToExpr :: Bool -> BExpr
boolToExpr False = FF
boolToExpr True = TT

-- Execution
exec :: Instr -> Env -> Env
exec (Nop) env                = env
exec (Assign a (Const prim)) env = update a prim env
exec (Assign a b) env         = update a (IntPrim (evala env b)) env
exec (IfThenElse (BVar v) b c) env   = if (evalb env (boolToExpr(extractBool(lookUp v env) v)))
                                       then (exec b env)
                                       else (exec c env)
exec (IfThenElse a b c) env   = if (evalb env a)
                                then (exec b env)
                                else (exec c env)
exec (While (BVar v) b) env   = if (evalb env ( boolToExpr(extractBool(lookUp v env) v)))
                                then (exec (While (BVar v) b) (exec b env))
                                else env
exec (While a b) env          = if (evalb env a)
                                then (exec (While a b) (exec b env))
                                else env
exec (Do []) env              = env
exec (Do (i:is)) env          = exec (Do is) (exec i env)
