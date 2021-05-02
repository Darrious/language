module Exec where

import Types
import Debug.Trace

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
evala :: Defs -> Env -> AExpr -> Integer
evala defs env (Const (IntPrim a)) = a
evala defs env (Var v)   = extractInt(lookUp v env) v
evala defs env (Add a b) = evala defs env a + evala defs env b
evala defs env (Sub a b) = evala defs env a - evala defs env b
evala defs env (Mul a b) = evala defs env a * evala defs env b
evala defs env (Div a b) = evala defs env a `div` evala defs env b
evala defs env (Mod a b) = evala defs env a `mod` evala defs env b
evala defs env (FApply f args) =
  let evalArgs = map (evala defs env) args -- :: [Integer]
      (Function _ vars body) = lookupDef f defs
      bindings = zip vars [IntPrim x | x <-evalArgs]
      execBody = exec defs (Do body) bindings
   in extractInt (fromJust (lookup ("retVal") execBody)) "retVal"



evalb :: Defs -> Env -> BExpr -> Bool
evalb defs env TT        = True
evalb defs env FF        = False
evalb defs env (And a b) = (evalb defs env a) && (evalb defs env b)
evalb defs env (Or  a b) = (evalb defs env a) || (evalb defs env b)
evalb defs env (Eql a b) = (evala defs env a) == (evala defs env b)
evalb defs env (Lt a b)  = (evala defs env a) < (evala defs env b)
evalb defs env (Gt a b)  = (evala defs env a) > (evala defs  env b)
evalb defs env (Not b)   = (not (evalb defs env b))

boolToExpr :: Bool -> BExpr
boolToExpr False = FF
boolToExpr True = TT

lookupDef :: FName -> Defs -> FunDefn
lookupDef f [] = error $ "No such function found: " ++ f
lookupDef f (fg@(Function g vars insts):gs) | f == g = fg
                                            | otherwise = lookupDef f gs


fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "nothing to return in fromJust"

-- Execution
exec :: Defs -> Instr -> Env -> Env
exec defs (Nop) env                = env
exec defs (Assign a (Const prim)) env = update a prim env
exec defs (Assign a b) env         = update a (IntPrim (evala defs env b)) env
exec defs (IfThenElse (BVar v) b c) env   = if (evalb defs env (boolToExpr(extractBool(lookUp v env) v)))
                                       then (exec defs b env)
                                       else (exec defs c env)
exec defs (IfThenElse a b c) env   = if (evalb defs env a)
                                then (exec defs b env)
                                else (exec defs c env)
exec defs i@(While (BVar v) b) env   = if (evalb defs env ( boolToExpr(extractBool(lookUp v env) v)))
                                then (exec defs i (exec defs b env))
                                else env
exec defs i@(While a b) env          = if (evalb defs  env a)
                                then (exec defs i (exec defs b env))
                                else env
exec defs (Do []) env              = env
exec defs (Do (i:is)) env          = exec defs (Do is) (exec defs i env)
