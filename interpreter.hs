
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
env1 = [("X", 1), ("Y", 5)]
env2 = [("X", 23), ("Y", 3)]
env3 = [("X", 12), ("Y", 1)]
env4 = [("X", 100), ("Y", 6), ("Z", 4)]


-- lookUp x e returns the value assigned to x in environment e
lookUp :: Vars -> Env -> Integer
lookUp x [] = 1
lookUp x env =  snd (head (filter (\(a, _) ->  a == x ) env))


-- update x v e sets the values of x to v and keeps other variables in e the same
update :: Vars -> Integer -> Env -> Env
update x y [] = []
update x y env = [if (a == x) then (x, y) else (a,b) | (a, b) <- env]

-- -- Question 1: Evaluate
evala :: Env -> AExpr -> Integer
evala env (Const a) = a
evala env (Var v) = lookUp v env
evala env (Add a b) = evala env a + evala env b
evala env (Sub a b) = evala env a - evala env b
evala env (Mul a b) = evala env a * evala env b
evala env (Div a b) = evala env a `div` evala env b

--evalb :: Env -> BExpr -> Integer
