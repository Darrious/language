
-- Variables
type Vars = String

-- Arithmetic expressions
data AExpr = Var Vars | Const Integer
           | Add AExpr AExpr | Sub AExpr AExpr
           | Mul AExpr AExpr | Div AExpr AExpr
    deriving Show

-- Boolean expressions
data BExpr = TT | FF
           | And BExpr BExpr
           | Eql AExpr AExpr
           | Lt AExpr AExpr
    deriving Show

-- Instructions
data Instr = Assign Vars AExpr
           | IfThenElse BExpr Instr Instr
           | While BExpr Instr
           | Do [Instr]
           | Nop
    deriving Show

-- A program is a list of Instructions
type Program = [Instr]

-- Environment
type Env = [(Vars, Integer)]
env1 = [("X", 1), ("Y", 99)]
env2 = [("X", 23), ("Y", 3)]
env3 = [("X", 12), ("Y", 1)]
env4 = [("X", 5), ("Y", 6), ("Z", 11)]


-- lookUp x e returns the value assigned to x in environment e
lookUp :: Vars -> Env -> Integer
lookUp x []  = error "No variable has been initialized"
lookUp x env =  snd (head (filter (\(a, _) ->  a == x ) env))


-- update x v e sets the values of x to v and keeps other variables in e the same
update :: Vars -> Integer -> Env -> Env
update x y []  = [(x,y)]
update x y env = [if (a == x) then (x, y) else (a,b) | (a, b) <- env]

-- Evaluate
evala :: Env -> AExpr -> Integer
evala env (Const a) = a
evala env (Var v)   = lookUp v env
evala env (Add a b) = evala env a + evala env b
evala env (Sub a b) = evala env a - evala env b
evala env (Mul a b) = evala env a * evala env b
evala env (Div a b) = evala env a `div` evala env b

evalb :: Env -> BExpr -> Bool
evalb env TT        = True
evalb env FF        = False
evalb env (And a b) = (evalb env a) && (evalb env b)
evalb env (Eql a b) = (evala env a) == (evala env b)
evalb env (Lt a b)  = (evala env a) < (evala env b)

instr1 = (IfThenElse (Lt (Var "X") (Const 10)) (Assign ("X") (Add (Const 1)(Var "X"))) (Nop))
instr2 = (While (Lt (Var "X") (Const 5)) (Assign ("X")(Add (Var "X") (Const 1))))
instr3 = (While (Lt (Var "X") (Const 5)) (Do [(Assign ("X")(Add (Var "X") (Const 1))),
                                              (Assign ("Y")(Sub (Var "Y") (Const 1)))]))



-- Execution
exec :: Instr -> Env -> Env
-- exec (Nop) []               = []
-- exec (Assign a b) []        = update (a) (evala [] b) ([])
exec (Nop) env              = env
exec (Assign a b) env       = update a (evala env b) env
exec (IfThenElse a b c) env = if (evalb env a) then (exec b env) else (exec c env)
exec (While a b) env        = if (evalb env a) then (exec (While a b) (exec b env))
                              else env
exec (Do []) env            = env
exec (Do (i:is)) []         = exec (Do is) (exec (i) [])
exec (Do (i:is)) env        = exec (Do is) (exec (i) env)
-- exec i []                   = error "You messed up."


run :: Program -> Env
run p = exec (Do p) [("X", 0)]

sum100 :: Program
sum100 = [(Assign "X" (Const 0)),
           (Assign "C" (Const 1)),
           (While (Lt (Var "C") (Const 101))
                 (Do [(Assign "X" (Add (Var "X") (Var "C"))),
                      (Assign "C" (Add (Var "C") (Const 1)))
                     ]))]

sum100output = lookUp "X" (run sum100)
