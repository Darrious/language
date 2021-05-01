{-
    - Darrious Barger
    - Need to implement
    -   AndOp
    -   Syntax checking
    -   if then else
    -   greater than
-}

import Data.Typeable
import Data.Char

-- Variables
type Vars = String
type FName = String

-- Arithmetic expressions
data AExpr = Var Vars | Const Values
           | Add AExpr AExpr | Sub AExpr AExpr
           | Mul AExpr AExpr | Div AExpr AExpr
           | Mod AExpr AExpr| FApply FName [AExpr]
    deriving Show


-- Boolean expressions
data BExpr = TT | FF
           | And BExpr BExpr
           | Or BExpr BExpr  | Not BExpr
           | Eql AExpr AExpr
           | Lt AExpr AExpr
           | Gt AExpr AExpr
           | BVar Vars
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

data FunDefn = Function FName [Vars] [Instr]
type Defs = [FunDefn]

-- Environment
data Values = IntPrim Integer | BoolPrim Bool | VNull
    deriving Show
type Env = [(Vars, Values)]
env1 = [("X", IntPrim 1), ("Y", IntPrim  99)]
env2 = [("X", IntPrim 23), ("Y", IntPrim 3)]
env3 = [("X", IntPrim 12), ("Y", IntPrim 1)]
env4 = [("X", IntPrim 5), ("Y", IntPrim 6), ("Z", IntPrim 11)]
env5 = [("Y", IntPrim 6), ("Z", IntPrim 11)]
env6 = [("X", IntPrim 5),("flag", BoolPrim True)]
env7 = [("X", IntPrim 10)]
env8 = [("Z", BoolPrim True)]

-- lookUp x e returns the value assigned to x in environment e
-- lookUp :: Vars -> Env -> Integer
-- -- lookUp x [] = error "Variable has not be initialized"
-- lookUp x [] = 0
-- lookUp x ((a,as):b) = if (a == x) then as else (lookUp x b)

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


instr1 = IfThenElse (Lt (Const (IntPrim 15)) (Const (IntPrim 10)))
                    (Assign "X" (Add (Const (IntPrim 10)) (Const (IntPrim 15))))
                    (Assign "X" (Const (IntPrim 0)))

instr2 = Do [(Assign "X" (Const (IntPrim 0))), While (Lt (Var "X") (Const (IntPrim 10)))
               (Do [Assign "X" (Add (Const (IntPrim 1)) (Var "X"))])]
-- Execution
exec :: Instr -> Env -> Env
exec (Nop) env                = env
exec (Assign a (Const prim)) env = update a prim env
exec (Assign a b) env         = update a (IntPrim (evala env b)) env
exec (IfThenElse a b c) env   = if (evalb env a) then (exec b env) else (exec c env)
exec (While (BVar v) b) env   = if (evalb env ( boolToExpr(extractBool(lookUp v env) v)))
                                then (exec (While (BVar v) b) (exec b env))
                                else env
exec (While a b) env          = if (evalb env a) then (exec (While a b) (exec b env))
                                else env
exec (Do []) env              = env
exec (Do (i:is)) env          = exec (Do is) (exec i env)


run :: Program -> Env
run p = exec (Do p) []


sum100 :: Program
sum100 = [(Assign "X" (Const (IntPrim 0))),
           (Assign "C" (Const (IntPrim 1))),
           (While (Lt (Var "C") (Const (IntPrim 101)))
                 (Do [(Assign "X" (Add (Var "X") (Var "C"))),
                      (Assign "C" (Add (Var "C") (Const (IntPrim 1))))
                     ]))]

-- Lexical Analysis
data UOps = NotOp deriving Show
data BOps = AddOp | SubOp | MulOp | DivOp | AndOp | OrOp | EqlOp | LtOp | GtOp
          | AssignOp | ModOp
    deriving (Show,Enum,Eq,Ord)


data Types = IntType | DoubleType | BoolType | StringType
   deriving Show
--

--
--
--
-- data Array = Arr [Values]
-- data LList = Node Values LList | Null deriving Show
-- data Struct = Array | LList deriving Show
--
--
--
-- -- function vs var names
-- -- instr block
-- -- Decl
-- -- add method type to AExpr
-- -- data Method = Function FName Types Decl [Instr]
-- --
-- -- type Decl = [(Vars, Types)]
--
--
--
--
data Token = VSym Vars | CSym Integer | BSym Bool | FSym FName
           | UOp UOps | BOp BOps
           | LPar | RPar | LBra | RBra | Semi
           | Keyword String
           | Err | Comma | Type Types
           | PA AExpr | PB BExpr | PI Instr  | PDo [Instr]
           | PF FName [AExpr]
    deriving Show

--
-- getArrayInd :: Array -> Int -> Values
-- getArrayInd (Arr a) ind =  a!!ind
--
-- getListNext :: LList -> LList
-- getListNext Null = Null
-- getListNext (Node (a, b)) = b
--
-- array1 = Arr [IntPrim 0, IntPrim 1, IntPrim 2, IntPrim 3]
-- linkList1 = Node (IntPrim 5, Node (IntPrim 2, Node (IntPrim 10, Null)))
-- arrayIndex :: Array -> Prim
-- arrayIndex s = IntPrim 0

classify :: String -> Token
classify "int" = Type IntType
classify "double" = Type DoubleType
classify "bool" = Type BoolType

classify s@"while" = Keyword s
classify s@"if" = Keyword s
classify s@"then" = Keyword s
classify s@"else" = Keyword s
classify s@"nop" = Keyword s
classify s@"def" = Keyword s

classify s@"+" = BOp AddOp
classify s@"-" = BOp SubOp
classify s@"/" = BOp DivOp
classify s@"*" = BOp MulOp
classify s@"%" = BOp ModOp
classify s@"\\/" = BOp OrOp
classify s@"/\\" = BOp AndOp
classify s@"==" = BOp EqlOp
classify s@":=" = BOp AssignOp
classify s@"<" = BOp LtOp
classify s@">" = BOp GtOp
classify s@"!" = UOp NotOp

classify s@"{" =  LBra
classify s@"}" =  RBra
classify s@"(" =  LPar
classify s@")" =  RPar
classify s@";" =  Semi
classify s@"," = Comma
classify s@"true" = BSym True
classify s@"false" = BSym False
classify s | isVSym s = VSym s
classify s | isFun s = FSym s
classify s | isCSym s = CSym (read s)

classify _ = Err


isCSym :: String -> Bool
isCSym "" = False
isCSym (x:xs) = isDigit x && q1 xs
  where q1 "" = True
        q1 (y:ys) = (isDigit y && q1 ys) || (y == '.' && not (null ys) && q2 ys)
        q2 ys = all isDigit ys

isVSym :: String -> Bool
isVSym "" = False
isVSym (x:xs) = isLower x && q1 xs
  where q1 "" = True
        q1 (y:ys) = (isAlpha y || isDigit y || y `elem` "-_'") && q1 ys

isFun :: String -> Bool
isFun "" = False
isFun (x:xs) = isUpper x && q1 xs
  where q1 "" = True
        q1 (y:ys) = (isAlpha y || isDigit y || y `elem` "-_'") && q1 ys

-- Parsing
preproc :: String -> String
preproc [] = []
preproc ('/':'\\':xs) = " /\\ " ++ preproc xs
preproc ('\\':'/':xs) = " \\/ " ++ preproc xs

preproc ('(':xs) = " ( " ++ preproc xs
preproc (')':xs) = " ) " ++ preproc xs
preproc ('{':xs) = " { " ++ preproc xs
preproc ('}':xs) = " } " ++ preproc xs
preproc (';':xs) = " ; " ++ preproc xs
preproc ('+':xs) = " + " ++ preproc xs
preproc ('*':xs) = " * " ++ preproc xs
preproc ('%':xs) = " % " ++ preproc xs
preproc ('/':xs) = " / " ++ preproc xs
preproc ('<':xs) = " < " ++ preproc xs
preproc ('>':xs) = " > " ++ preproc xs
preproc ('=':'=':xs) = " == " ++ preproc xs
preproc (':':'=':xs) = " := " ++ preproc xs
preproc ('-':xs) = " - " ++ preproc xs
preproc (',':xs) = " , " ++ preproc xs
preproc (x:xs) = x : preproc xs

lexer :: String -> [Token]
lexer "" = []
lexer s = map classify (words (preproc s))

funTest = "Exp(5, c+1)"

-- Infinite loop unless x is included in the env

test1 :: String
test1 =  "int c := 1 ; int r := 1 ; while ( c < x ) { r := r * c ; c := c + 1 ; }"

-- test2 :: String
-- test2 =  "int c:=1 ; int r:= 3; while (c<r) {r := r*c; c := c+1 ; }"

test3 :: String
test3 =  "int c:=1 ; int r:= 3;"

test4 :: String
test4 =  "int x:=5; int c:=1; int r:= 3; while ( c < x ) { r := r * c ; c := c + 1; } "

test5 :: String
test5 = "fact:=1; c :=1 ; while (! (5 < c)) { c := (c+1); fact := (fact * c); } "

test6 :: String
test6 = "x:=3; if (x>5) then { x:=0; } else {x:=1;}"

test8 = "int x:=3; bool flag:=true;"


boolToExpr :: Bool -> BExpr
boolToExpr False = FF
boolToExpr True = TT


sr :: [Token] -> [Token] -> [Token]
sr (CSym c : stack)               input = sr (PA (Const (IntPrim c)) : stack) input
sr (BSym c : stack)               input = sr (PA (Const (BoolPrim c)) : stack) input
sr (VSym v : Type _  : stack )    input = sr (PA (Var v) : stack) input
sr (VSym b : stack)               input = sr (PA (Var b) : stack) input

sr s@(PA e2 : BOp  op1 : PA e1 : stack) (BOp op2 : input) | op1 < op2 = sr (BOp op2 : s) input
sr (PA e2 : BOp AddOp : PA e1 : stack) input = sr (PA (Add e1 e2) : stack) input -- AExpr -> AExpr (AddOp) AExpr
sr (PA e2 : BOp MulOp : PA e1 : stack) input = sr (PA (Mul e1 e2) : stack) input
sr (PA e2 : BOp DivOp : PA e1 : stack) input = sr (PA (Div e1 e2) : stack) input
sr (PA e2 : BOp SubOp : PA e1 : stack) input = sr (PA (Sub e1 e2) : stack) input
sr (PA e2 : BOp ModOp : PA e1 : stack) input = sr (PA (Mod e1 e2) : stack) input
sr (RPar : PA e : LPar : stack)        input = sr (PA e : stack) input
sr (RPar : PB e : LPar : stack)        input = sr (PB e : stack) input
sr (PA a2 : BOp EqlOp : PA a1 : stack)  input = sr (PB (Eql a1 a2) : stack) input -- BEXpr -> AExpr == AExpr
sr (PA a2 : BOp LtOp : PA a1 : stack)  input  = sr (PB (Lt a1 a2) : stack) input -- BExpr -> AExpr (BOp LtOp) AExpr
sr (PA a2 : BOp GtOp : PA a1 : stack)  input  = sr (PB (Gt a1 a2) : stack) input
sr (PB b2 : BOp OrOp : PB b1 : stack)  input  = sr (PB (Or b1 b2) : stack) input     -- BEXpr -> BExpr \/ BExpr
sr (PB b : UOp NotOp : stack) input           = sr (PB (Not b) : stack) input   -- BExpr -> Not BExpr

sr (Semi : PA a : BOp AssignOp : PA (Var c) : stack) input = sr (PI (Assign c a) : stack) input -- Instr -> Var AssignOp AExpr
sr (PI i : PB b : Keyword "while" : stack) input = sr (PI (While b i) : stack) input -- Instr -> While BExpr Instr
sr (PI i : PA (Var b) : Keyword "while" : stack) input = sr (PI (While (BVar b) i) : stack) input -- Instr -> While BExpr Instr

sr (PI i : Keyword "else" : PI i2 : Keyword "then" : PB b : Keyword "if" : stack) input
                                             = sr (PI (IfThenElse b i2 i) : stack) input

-- sr (FSym f : ts ) (LPar : RPar : is)         = sr (PA (FApply f []) : ts) is
-- sr (FSym f : ts ) (LPar : is)                = sr (PF f [] : ts) is
-- sr (RPar : PA a : PF f args : ts) i          = sr (PA (FApply f (reverse $ a:args)):ts) i
-- sr (Comma : PA a : PF f args : ts) i         = sr (PF f (a:args) : ts) i

sr (RBra : PI i : stack) input = sr (PDo [i] : stack) input
sr (RBra : stack) input = sr (PDo [] : stack) input
sr (PDo s  : PI i : stack) input = sr (PDo (i:s) : stack) input
sr (PDo s : LBra : stack) input = sr (PI (Do s) : stack) input
sr stack                           (i:input) = sr (i:stack) input
sr stack [] = stack


listInstr :: [Token] -> [Instr]
listInstr [] = []
listInstr (PI ins : ts) = [ins] ++ listInstr ts
listInstr (a:as) = listInstr as


test7 = (sr [] (lexer test5))

-- IO
main :: IO ()
main = do
  -- putStrLn "Enter a .imp file with code."
  -- filename <- getLine
  let filename = "testIf.imp"
  contents <- readFile filename

  let lexed = lexer contents
  putStrLn "Here is the result of lexical analysis:"
  putStrLn (show lexed)
  putStrLn "------------------------------------------"

  let parsed = sr [] lexed
  putStrLn "Here is the result of parsing:"
  putStrLn (show parsed)
  putStrLn "------------------------------------------"

  let answer = exec ( Do(reverse $ listInstr parsed)) []
  putStrLn "Here is the result of the program:"
  putStrLn (show answer)
