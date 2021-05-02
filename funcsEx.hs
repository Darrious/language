import Data.Char

-- Variables

type Vars = String
type FName = String

-- Arithmetic expressions
data AExpr = Var Vars | Const Integer
           | Add AExpr AExpr | Sub AExpr AExpr
           | Mul AExpr AExpr | Div AExpr AExpr
           | FApply FName [AExpr]
  deriving Show
-- Boolean expressions
data BExpr = TT | FF          -- the true and false constants
           | And BExpr BExpr | Or BExpr BExpr | Not BExpr -- boolean operations
           | Eql AExpr AExpr  -- equality of arithmetic expressions
           | Lt AExpr AExpr   -- true if the first is less than the second
  deriving Show
-- Instructions
data Instr = Assign Vars AExpr            -- assign X to the value of an expression
           | IfThenElse BExpr Instr Instr -- conditional
           | While BExpr Instr            -- looping construct
           | Do [Instr]                   -- a block of several instructions
           | Nop                          -- the "do nothing" instruction
  deriving Show

-- A program is a list of instructions
type Program = [Instr]

data FunDefn = Function FName [Vars] [Instr]

type Defs = [FunDefn]

lookupDef :: FName -> Defs -> FunDefn
lookupDef f [] = error $ "No such function found: " ++ f
lookupDef f (fg@(Function g vars insts):gs) | f == g = fg
                                            | otherwise = lookupDef f gs

-- data Value = VInt Integer | VBool Bool | VDouble Double --- ...
--            | Fun FunDefn

type Env = [(Vars,Integer)]

run :: Program -> Env
run p = exec [] (Do p) []

sum100 :: Program     -- a program to add together all the numbers up to 100
sum100 = [
  Assign "X" (Const 0),         -- initialize the sum     at X=0
  Assign "C" (Const 1),         -- initialize the counter at C=1
  While (Lt (Var "C") (Const 101))    -- while C < 101, do:
        (Do [Assign "X" (Add (Var "X") (Var "C")),  -- X := X + C;
             Assign "C" (Add (Var "C") (Const 1))]  -- C := C + 1
        )]

sum100output = lookUp "X" (run sum100)

data UOps = NotOp deriving Show
data BOps = AddOp | SubOp | MulOp | DivOp | AndOp | OrOp | EqlOp | LtOp | AssignOp
  deriving Show
data Token = VSym Vars | FSym FName | CSym Integer | BSym Bool
           | UOp UOps | BOp BOps
           | LPar | RPar | LBra | RBra | Semi | Comma
           | Keyword String
           | Err
           | PA AExpr | PB BExpr | PI Instr
           | PF FName [AExpr]
  deriving Show

-- lookUp x e returns the value assigned to x in environment e
lookUp :: Vars -> Env -> Integer
lookUp x env = case (lookup x env) of (Just v) -> v
-- update x v e sets the value of x to v and keeps other variables in e the same

update :: Vars -> Integer -> Env -> Env
update x newval [] = [(x,newval)]
update x newval ((y,v) : env) | x == y = ((x,newval) : env)
                              | otherwise = (y,v) : update x newval env

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "nothing to return in fromJust"

evala :: Defs -> Env -> AExpr -> Integer
evala defs env (Var x) = lookUp x env
evala defs env (Const v) = v
evala defs env (Add p1 p2) = evala defs env p1 + evala defs env p2
evala defs env (Sub p1 p2) = evala defs env p1 - evala defs env p2
evala defs env (Mul p1 p2) = evala defs env p1 * evala defs env p2
evala defs env (Div p1 p2) = evala defs env p1 `div` evala defs env p2
evala defs env (FApply f args) =
  let evalArgs = map (evala defs env) args -- :: [Integer]
      (Function _ vars body) = lookupDef f defs
      bindings = zip vars evalArgs
      execBody = exec defs (Do body) bindings
   in fromJust (lookup ("retVal_"++f) execBody)

evalb :: Defs -> Env -> BExpr -> Bool
evalb _ _ TT = True
evalb _ _ FF = False
evalb defs e (And b1 b2) = evalb defs e b1 && evalb defs e b2
evalb defs e (Or  b1 b2) = evalb defs e b1 || evalb defs e b2
evalb defs e (Not b) = not $ evalb defs e b
evalb defs e (Eql e1 e2) = evala defs e e1 == evala defs e e2
evalb defs e (Lt e1 e2)  = evala defs e e1 < evala defs e e2

exec :: Defs -> Instr -> Env -> Env
exec defs (Assign x v) e = update x (evala defs e v) e
exec defs (IfThenElse c i1 i2) e = if evalb defs e c then exec defs i1 e else exec defs i2 e
exec defs (While c i) e = if evalb defs e c then exec defs (While c i) (exec defs i e) else e
exec defs (Do []) e = e
exec defs (Do (i:is)) e = exec defs (Do is) (exec defs i e)

isVSym :: String -> Bool
isVSym "" = False
isVSym (x:xs) =
  let q1 "" = True
      q1 (y:ys) = if isDigit y || isAlpha y || y == '-' || y == '_'
                    then q1 ys
                    else if y == '\'' then q2 ys else False
      q2 "" = True
      q2 ('\'':ys) = q2 ys
      q2 _ = False
  in  isLower x && q1 xs

isFSym :: String -> Bool
isFSym "" = False
isFSym (x:xs) =
  let q1 "" = True
      q1 (y:ys) = if isDigit y || isAlpha y || y == '-' || y == '_'
                    then q1 ys
                    else if y == '\'' then q2 ys else False
      q2 "" = True
      q2 ('\'':ys) = q2 ys
      q2 _ = False
  in  isUpper x && q1 xs

isCSym :: String -> Bool
isCSym = all isDigit


classify :: String -> Token

classify "+" = BOp AddOp
classify "-" = BOp SubOp
classify "*" = BOp MulOp
classify "/" = BOp DivOp

classify "!" = UOp NotOp
classify "/\\" = BOp AndOp
classify "\\/" = BOp OrOp
classify "==" = BOp EqlOp
classify "<" = BOp LtOp
classify ":=" = BOp AssignOp

classify "(" = LPar
classify ")" = RPar
classify ";" = Semi
classify "," = Comma

classify "while" = Keyword "while"
classify "if" = Keyword "if"
classify "then" = Keyword "then"
classify "else" = Keyword "else"
classify "do" = Keyword "do"
classify "nop" = Keyword "nop"
classify "def" = Keyword "def"

classify "T" = BSym True
classify "F" = BSym False
classify x | isCSym x = CSym (read x)
classify x | isVSym x = VSym x
classify x | isFSym x = FSym x
classify _ = Err

addSpaces :: String -> String
addSpaces "" = ""
addSpaces ('/' : '\\' : s) = " /\\ " ++ addSpaces s
addSpaces ('\\' : '/' : s) = " \\/ " ++ addSpaces s
addSpaces (':' : '=' : s)  = " := " ++ addSpaces s
addSpaces ('=' : '=' : s)  = " == " ++ addSpaces s
addSpaces ('!' : s) = " ! " ++ addSpaces s
addSpaces ('*' : s) = " * " ++ addSpaces s
addSpaces ('/' : s) = " / " ++ addSpaces s
addSpaces ('+' : s) = " + " ++ addSpaces s
addSpaces ('-' : s) = " - " ++ addSpaces s
addSpaces ('<' : s) = " < " ++ addSpaces s
addSpaces ('(' : s) = " ( " ++ addSpaces s
addSpaces (')' : s) = " ) " ++ addSpaces s
addSpaces (';' : s) = " ; " ++ addSpaces s
addSpaces (',' : s) = " , " ++ addSpaces s
addSpaces (x : s) = x : addSpaces s

lexer :: String -> [Token]
lexer s = map classify (words (addSpaces s))

parser :: [Token] -> Instr
parser l = sr [] (LPar : l ++ [RPar])

-- The main parsing function alternates between shifting elements
-- from the input queue to the parse stack, and reducing the top of the stack
sr :: [Token] -> [Token] -> Instr

sr (VSym x : ts) i     = sr (PA (Var  x) : ts) i
sr (CSym x : ts) i     = sr (PA (Const x) : ts) i
sr (BSym True : ts) i  = sr (PB TT : ts) i
sr (BSym False : ts) i = sr (PB FF : ts) i

sr (PB p : UOp NotOp : ts) l          = sr (PB (Not p) : ts) l
sr (PB p2 : BOp AndOp : PB p1 : ts) l = sr (PB (And p1 p2) : ts) l
sr (PB p2 : BOp OrOp  : PB p1 : ts) l = sr (PB (Or  p1 p2) : ts) l

sr (PA p2 : BOp AddOp : PA p1 : ts) l = sr (PA (Add p1 p2) : ts) l
sr (PA p2 : BOp SubOp : PA p1 : ts) l = sr (PA (Sub p1 p2) : ts) l
sr (PA p2 : BOp MulOp : PA p1 : ts) l = sr (PA (Mul p1 p2) : ts) l
sr (PA p2 : BOp DivOp : PA p1 : ts) l = sr (PA (Div p1 p2) : ts) l

sr (PA p2 : BOp EqlOp : PA p1 : ts) l     = sr (PB (Eql p1 p2) : ts) l
sr (PA p2 : BOp LtOp  : PA p1 : ts) l     = sr (PB (Lt p1 p2) : ts) l
sr (PA p : BOp AssignOp : PA (Var x) : ts) l = sr (PI (Assign x p) : ts) l

sr (RPar : PA p : LPar : ts) l        = sr (PA p : ts) l
sr (RPar : PB p : LPar : ts) l        = sr (PB p : ts) l
-- sr (RPar : RPar : ts) i               = sr ts (RPar : RPar : i)

sr (PI i : PB b : Keyword "while" : ts) l = sr (PI (While b i) : ts) l
sr (Keyword "nop" : ts) l                 = sr (PI Nop : ts) l
sr (PI i2 : Keyword "else" : PI i1 : Keyword "then" : PB b : Keyword "if" : ts) l
    = sr (PI (IfThenElse b i1 i2) : ts) l

sr (RPar : PI p : LPar : ts) i        = sr (PI p : ts) i

-- Parsing function application
sr (FSym f : ts) (LPar : RPar : is)  = sr (PA (FApply f []) : ts) is
sr (FSym f : ts) (LPar : is)         = sr (PF f [] : ts) is
sr (RPar  : PA a : PF f args : ts) i  = sr (PA (FApply f (reverse $ a:args)) : ts) i
sr (Comma : PA a : PF f args : ts) i  = sr (PF f (a:args) : ts) i

sr (PI (Do is) : Keyword "do" : ts) l   = sr (PI (Do is) : ts) l
sr (PI (Do is) : Semi : PI i : ts) l     = sr (PI (Do (i : is)) : ts) l
sr (RPar : PI p : ts)       i                   = sr (PI (Do [p]) : ts) (RPar : i)

-- sr (PI i : Keyword "do" : ts) l                  = sr (PI (Do [i]) : ts) l
-- sr (PI i : Semi : PI (Do is) : ts) l      = sr (PI (Do (is ++ [i])) : ts) l
--

sr xs (i:is) = sr (i:xs) is
sr [PI p] [] = p
sr (Err : _) _ = error "Lexical error!"
sr x [] = error (show x)

fibonacci8 :: String
fibonacci8 = -- you can break strings over many lines by connecting them with \
  "x:=0; \
  \ y:=1; \
  \ c:=0; \
  \ while (c<7) \\/ (c==7) do ( \
  \  c:= (c+1); \
  \  z:= (x+y); \
  \  x:=y; \
  \  y:=z ) "

fibLexed :: [Token]
fibLexed = lexer fibonacci8

fibParsed :: Instr
fibParsed = parser fibLexed

fibResult :: Integer
fibResult = lookUp "x" (run [fibParsed])

factorial5 :: String
factorial5 =
    " fact:=1; \
    \ c :=1 ;  \
    \ while (! (5 < c)) do ( \
    \   c := (c+1); \
    \   fact := (fact*c )\
    \ )"

factLexed = lexer factorial5
factParsed = parser ((LPar : factLexed) ++ [RPar])
factResult = lookUp "fact" (run [factParsed])

fromDo :: Instr -> [Instr]
fromDo (Do is) = is
fromDo _ = error "Not a Do instruction"

factFun :: FunDefn
factFun = Function "Fac" ["n"]
  [Assign "retVal_Fac" (Const 1),
   Assign "c" (Const 1),
   While (Not (Lt (Var "n") (Var "c")))
         (Do [Assign "c" (Add (Var "c") (Const 1)),Assign "retVal_Fac" (Mul (Var "retVal_Fac") (Var "c"))])]

main :: IO ()
main = do
  let filename = "testFun.imp"
  input <- readFile filename
  let lexed = lexer input
  let parsed = parser ((LPar : lexed) ++ [RPar])
  let result = run [parsed]
  putStrLn (show result)

test :: String
test = "Exp(5,x+1)"

mydefns = [factFun]

test2 = (FApply "Fac" [Const 10])
