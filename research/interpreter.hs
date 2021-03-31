
import Data.Char

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
           | Or BExpr BExpr  | Not BExpr
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
env5 = [("Y", 6), ("Z", 11)]

-- lookUp x e returns the value assigned to x in environment e
lookUp :: Vars -> Env -> Integer
-- lookUp x [] = error "Variable has not be initialized"
lookUp x [] = 0
lookUp x ((a,as):b) = if (a == x) then as else (lookUp x b)

-- lookUp x env =  snd (find (\(a, _) ->  a == x ) env)


-- update x v e sets the values of x to v and keeps other variables in e the same
update :: Vars -> Integer -> Env -> Env
update x y []  = [(x,y)]
update x y env = if (length(checkedList) > 0)
                 then (updatedList)
                 else env ++ [(x,y)]
  where checkedList = filter (\(a,_) -> x == a) env  --Does list contain element?
        updatedList = [if (a == x) then (x, y) else (a,b) | (a, b) <- env] -- if contains element
                                                                           -- update it

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
instr4 = (Assign "X" (Const 3))
instr5 = Do [instr4]



-- Execution
exec :: Instr -> Env -> Env
exec (Nop) env                = env
exec (Assign a (Const x)) env = update a x env
exec (Assign a b) env         = update a (evala env b) env
exec (IfThenElse a b c) env   = if (evalb env a) then (exec b env) else (exec c env)
exec (While a b) env          = if (evalb env a) then (exec (While a b) (exec b env))
                                else env
exec (Do []) env              = env
exec (Do (i:is)) env          = exec (Do is) (exec i env)


run :: Program -> Env
run p = exec (Do p) [("X", 0)]


sum100 :: Program
sum100 = [ (Assign "X" (Const 0)),
           (Assign "C" (Const 1)),
           (While (Lt (Var "C") (Const 101))
                 (Do [(Assign "X" (Add (Var "X") (Var "C"))),
                      (Assign "C" (Add (Var "C") (Const 1)))
                     ]))]

sum100output = lookUp "C" (run sum100)
sum100output2 = lookUp "X" (run sum100)


-- Lexical Analysis
data UOps = NotOp deriving Show
data BOps = AddOp | SubOp | MulOp | DivOp | AndOp | OrOp | EqlOp | LtOp | AssignOp
   deriving Show

data Types = IntType | DoubleType | BoolType
   deriving Show

data Token = VSym String | CSym Integer | BSym Bool
           | UOp UOps | BOp BOps
           | LPar | RPar | LBra | RBra | Semi
           | Keyword String
           | Err
           | PA AExpr | PB BExpr | PI Instr | Type Types | PDo [Instr]
    deriving Show



classify :: String -> Token
classify "int" = Type IntType
classify "double" = Type DoubleType
classify "bool" = Type BoolType

classify s@"while" = Keyword s
classify s@"if" = Keyword s
classify s@"then" = Keyword s
classify s@"else" = Keyword s
classify s@"nop" = Keyword s

classify s@"+" = BOp AddOp
classify s@"-" = BOp SubOp
classify s@"/" = BOp DivOp
classify s@"*" = BOp MulOp
classify s@"\\/" = BOp OrOp
classify s@"/\\" = BOp AndOp
classify s@"==" = BOp EqlOp
classify s@":=" = BOp AssignOp
classify s@"<" = BOp LtOp
classify s@"!" = UOp NotOp

classify s@"{" =  LBra
classify s@"}" =  RBra
classify s@"(" =  LPar
classify s@")" =  RPar
classify s@";" =  Semi

classify s | isVSym s = VSym s
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
preproc ('=':'=':xs) = " == " ++ preproc xs
preproc (':':'=':xs) = " := " ++ preproc xs
preproc ('-':xs) = " - " ++ preproc xs
preproc (x:xs) = x : preproc xs

lexer :: String -> [Token]
lexer "" = []
lexer s = map classify (words (preproc s))

test1 :: String
test1 =  "int c := 1 ; int r := 1 ; while ( c < x ) { r := r * c ; c := c + 1 ; }"

test2 :: String
test2 =  "int c:=1 ; int r:= 1; while (c<x) {r := r*c; c := c+1 ; }"


-- data Instr = Assign Vars AExpr
--            | IfThenElse BExpr Instr Instr
--            | While BExpr Instr
--            | Do [Instr]
--            | Nop
--     deriving Show
-- data Token = VSym String | CSym Integer | BSym Bool
--            | UOp UOps | BOp BOps
--            | LPar | RPar | LBra | RBra | Semi
--            | Keyword String
--            | Err
--            | PA AExpr | PB BExpr | PI Instr
--     deriving Show
      -- data BOps = AddOp | SubOp | MulOp | DivOp | AndOp | OrOp | EqlOp | LtOp | AssignOp
      --     deriving Show
      --
      -- data BExpr = TT | FF
      --            | And BExpr BExpr
      --            | Or BExpr BExpr  | Not BExpr
      --            | Eql AExpr AExpr
      --            | Lt AExpr AExpr
      --     deriving Show
-- -- Parser
-- data AExpr = Var Vars | Const Integer
--            | Add AExpr AExpr | Sub AExpr AExpr
--            | Mul AExpr AExpr | Div AExpr AExpr
--     deriving Show


sr :: [Token] -> [Token] -> [Token]
sr (VSym v : ts)                            i = sr (PA (Var v) : ts) i
sr (CSym c : ts)                            i = sr (PA (Const c) : ts) i
--
-- --sr s@(PA e2 : BOp op1 : PA e1 : stack) (BOp op2 : input) | op1 < op2 = sr (BOp op2 : s) input
--
sr (BSym False : ts)                        i = sr (PB FF : ts) i
sr (BSym True : ts)                         i = sr (PB TT : ts) i

sr (RPar : PA p : LPar : ts)                i = sr (PA p : ts) i
sr (RPar : PB p : LPar : ts)                i = sr (PB p : ts) i

sr (UOp NotOp : PB p1 : ts)                 i = sr (PB (Not p1) : ts) i
sr (PA p2 : BOp LtOp : PA p1 : ts)          i = sr (PB (Lt p1 p2) : ts) i
sr (PA p2 : BOp EqlOp : PA p1 : ts)         i = sr (PB (Eql p1 p2) : ts) i
sr (PB p2 : BOp AndOp : PB p1 : ts)         i = sr (PB (And p1 p2) : ts) i
sr (PB p2 : BOp OrOp : PB p1 : ts)          i = sr (PB (Or p1 p2) : ts) i
sr (PA p2 : BOp AddOp : PA p1 : ts)         i = sr (PA (Add p1 p2) : ts) i
sr (PA p2 : BOp MulOp : PA p1 : ts)         i = sr (PA (Mul p1 p2) : ts) i
sr (PA p2 : BOp SubOp : PA p1 : ts)         i = sr (PA (Sub p1 p2) : ts) i
sr (PA p2 : BOp DivOp : PA p1 : ts)         i = sr (PA (Div p1 p2) : ts) i
-- sr (PA a : BOp AssignOp : PA (Var b) : ts ) i = sr (PI (Assign b a) : ts) i

sr (PI i2 : Keyword "else" : PI i1 : Keyword "then" : PB b : Keyword "if" : ts) i
                                    = sr (PI (IfThenElse b i1 i2) : ts) i
sr (PI ins : PB b : Keyword "while" : stack) i = sr (PI (While b ins) : stack) i

-- sr (RBra : Semi : PI ins : ts) i        = sr (PDo [ins] : ts) i
-- sr (RBra : stack) i              = sr (PDo [] : stack) i
-- sr (PDo s : Semi : PI ins : ts) i = sr (PDo (ins:s) : ts) i
-- sr (PDo s : LBra :ts) i          = sr (PI (Do s) : ts) i

sr stack (i:is) = sr (i:stack) is
sr stack []     = stack


processAssignment :: [Token] -> [Token]
processAssignment (PA a : BOp AssignOp : PA (Var b) : ts) = [(PI (Assign b a))] ++ processAssignment ts
processAssignment (a:ts) = [a] ++ processAssignment ts
processAssignment [] = [Err]


processBraces :: [Token]-> [Token]
processBraces (RBra : Semi : PI ins : ts) =  processBraces [(PDo [ins])]++ts
processBraces (PDo s : Semi : PI ins : ts)  = processBraces [(PDo (ins:s))]++ts

-- processBraces (RBra : stack)               = processBraces [(PDo [])] ++ processBraces stack
-- processBraces (PDo s : Semi : PI ins : ts)  = processBraces [(PDo (ins:s))] ++ processBraces ts
-- processBraces (PDo s : LBra :ts)           = processBraces [(PI (Do s))] ++ processBraces ts
processBraces (a:ts) = [a] ++ processBraces ts
processBraces [] = []

testBOp = [(VSym "r"),(BOp AssignOp),  (VSym "r"), (BOp MulOp), (VSym "c")]

--testParser = exec (parser (lexer test2)) []
processedTokens =  sr [] (lexer test2)
processedAssign = (processAssignment (processedTokens))
processedBraces =  (processBraces (processedAssign))
-- processedList = getList processBraces
-- parsed = exec (Do (processedList)) []





reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

getList :: [Token] -> [Instr]
getList [] = []
getList (PI i : ts) = [i] ++ getList ts
getList (a:as) = getList as




whileTest :: [Token] -> Instr
whileTest ((Keyword "While") : LBra : PB b : PDo p : RBra : ts) = (While b (Do p))

answer = exec (whileTest whileTestList) []

pdoList = PDo [(Assign "C" (Add (Var "C") (Const 1)))]
pdoCond = Lt (Var "C") (Const 5)

whileTestList :: [Token]
whileTestList = [(Keyword "While"), (LBra), (PB pdoCond), (pdoList), (RBra)]
-- parseAExpr :: [Token] -> AExpr
-- parseAExpr input = case sr [] input of
--   [PA a] -> a
--   ps -> error ("No parse:" ++ show ps)
--
-- parseBExpr :: [Token] -> BExpr
-- parseBExpr input = case sr [] input of
--   [PB a] -> a
--   ps -> error ("No parse:" ++ show ps)
--
-- parseInstrs :: [Token] -> [Instr]
-- parseInstrs ts = map tr $ sr [] ts
--   where getInstr (PI i) = i
--         getInstr _ = error "The list returned by the parser contains non-instructions"
