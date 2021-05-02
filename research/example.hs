{-
Goal:
Be able to parse, represent, and interpret programs like the following:

int fact(int x)
 {
 int c := 1;
 int r := 1;
 while (c <= x) {
  r := r * c;
  c := c + 1;
 }
 return r;
}  -}

import Data.Char

type Vars = String

data AExpr = Var Vars | Add AExpr AExpr | Mul AExpr AExpr | Neg AExpr
                       | Const Double
                       | Mod AExpr AExpr | Expt AExpr AExpr | Div AExpr AExpr
  deriving Show

data BExpr = And BExpr BExpr | Or BExpr BExpr | Imp BExpr BExpr
           | Not BExpr | Iff BExpr BExpr | TT | FF
           | Lte AExpr AExpr | Lt AExpr AExpr | Eql AExpr AExpr
  deriving Show

data Instr = Assign Vars AExpr
           | While BExpr Instr
           | Do [Instr]
           | Return AExpr
           | IfThenElse BExpr Instr Instr
  deriving Show

type Program = [Instr]

fact :: Program
fact = [ Assign "c" (Const 1)
       , Assign "r" (Const 1)
       , While (Lte (Var "c") (Var "x"))
               (Do [ Assign "r" (Mul (Var "r") (Var "c"))
                   , Assign "c" (Add (Var "c") (Const 1.0)) ] )
       , Return (Var "r")
       ]

data Types = IntType | DoubleType | BoolType
  deriving Show

data BOps = ModOp | AddOp | MulOp | DivOp | ExptOp
           | AndOp | OrOp | ImpOp | IffOp | LteOp | LtOp | EqlOp
          | AssignOp
  deriving (Show,Enum,Eq,Ord)

data Keywords = KWhile | KReturn | KIf | KThen | KElse
  deriving Show

data Token = Type Types | VSym String | CSym Double
           | LPar | RPar | LBra | RBra | Semi
           | BOp BOps | Keyword String
           | NegOp
           | Err
           | PA AExpr | PB BExpr | PI Instr | PDo [Instr]
  deriving Show
-- Want: analyze :: String -> [Token]
-- easy hack: use whitespace as a separator,
--  +  write a function to classify a single string as a token

-- isLowerCase :: Char -> Bool
-- isLowerCase x = 'a' <= x && x <= 'z'

isVSym :: String -> Bool
isVSym "" = False
isVSym (x:xs) = isLower x && q1 xs
  where q1 "" = True
        q1 (y:ys) = (isAlpha y || isDigit y || y `elem` "-_'") && q1 ys

isCSym :: String -> Bool
isCSym "" = False
isCSym (x:xs) = isDigit x && q1 xs
  where q1 "" = True
        q1 (y:ys) = (isDigit y && q1 ys) || (y == '.' && not (null ys) && q2 ys)
        q2 ys = all isDigit ys
  -- where q1 "" = True
  --       q1 (y:ys) = (isDigit y && q1 ys) || (y == '.' && not (null ys) && q2 ys)
  --       q2 "" = True
  --       q2 (y:ys) = isDigit y && q2 ys
  -- where q1 "" = True
  --       q1 (y:ys) | isDigit y = q1 ys
  --       q1 ('.':y:ys) = isDigit y && q2 ys
  --       q1 _ = False
  --       q2 "" = True
  --       q2 (y:ys) = isDigit y && q2 ys

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
preproc ('^':xs) = " ^ " ++ preproc xs
preproc ('/':xs) = " / " ++ preproc xs
preproc ('-':'>':xs) = " -> " ++ preproc xs
preproc ('<':'-':'>':xs) = " <-> " ++ preproc xs
preproc ('<':'=':xs) = " <= " ++ preproc xs
preproc ('<':xs) = " < " ++ preproc xs
preproc ('=':'=':xs) = " == " ++ preproc xs
preproc (':':'=':xs) = " := " ++ preproc xs
-- preproc ('-':xs) = " - " ++ preproc xs
preproc (x:xs) = x : preproc xs

classify :: String -> Token
classify "int" = Type IntType
classify "double" = Type DoubleType
classify "bool" = Type BoolType

classify "(" = LPar
classify ")" = RPar
classify "{" = LBra
classify "}" = RBra
classify ";" = Semi

classify "/\\" = BOp AndOp
classify "\\/" = BOp OrOp

classify "+" = BOp AddOp
classify "*" = BOp MulOp
classify "%" = BOp ModOp
classify "^" = BOp ExptOp
classify "/" = BOp DivOp
classify "!" = NegOp
classify "->" = BOp ImpOp
classify "<->" = BOp IffOp
classify "<=" = BOp LteOp
classify "<" = BOp LtOp
classify "==" = BOp EqlOp
classify ":=" = BOp AssignOp

-- classify "-" = NegOp

classify s@"while" = Keyword s
classify s@"return" = Keyword s
classify s@"if" = Keyword s
classify s@"then" = Keyword s
classify s@"else" = Keyword s

classify s | isVSym s = VSym s
classify s | isCSym s = CSym (read s)

classify _ = Err

analyze :: String -> [Token]
analyze s = map classify (words (preproc s))

test :: String
test =  "int c := 1 ; int r := 1 ; while ( c <= x ) { r := r * c ; c := c + 1 ; }"

test2 :: String
test2 =  "int c:=1 ; int r:= 1; while (c<=x) {r := r*c; c := c+1 ; }"

{-
data AExpr = Var Vars | Add AExpr AExpr | Mul AExpr AExpr | Neg AExpr
                       | Const Double
                       | Mod AExpr AExpr | Expt AExpr AExpr | Div AExpr AExpr

data BOps = AddOp | MulOp | ModOp | ExptOp | DivOp
          | AndOp | OrOp | ImpOp | IffOp | LteOp | LtOp | EqlOp
          | AssignOp

data Token = Type Types | VSym String | CSym Double
           | LPar | RPar | LBra | RBra | Semi
           | BOp BOps | Keyword Keywords
           | Err
-}
-- parser :: [Token] -> S
-- parser ts = case (sr [] ts) of
--   [PS s] -> s
--   l      -> error ("No parse:" ++ show l)
--

parseAExpr :: [Token] -> AExpr
parseAExpr input = case sr [] input of
  [PA a] -> a
  ps -> error ("No parse:" ++ show ps)

parseInstrs :: [Token] -> [Instr]
parseInstrs ts = map getInstr $ sr [] ts
  where getInstr (PI i) = i
        getInstr _ = error "The list returned by the parser contains non-instructions"

-- prec :: BOps -> Integer
-- prec AddOp = 1
-- prec MulOp = 2
-- prec ExptOp  = 3

sr :: [Token] -> [Token] -> [Token]
sr (VSym v : stack)                    input = sr (PA (Var v) : stack) input  -- AExpr -> Var v, Var v -> VSym v
sr (CSym c : stack)                    input = sr (PA (Const c) : stack) input -- AExpr -> Const c, Const c -> CSym c
sr s@(PA e2 : BOp op1 : PA e1 : stack) (BOp op2 : input) | op1 < op2 = sr (BOp op2 : s) input
-- sr s@(PA e2 : BOp MulOp : PA e1 : stack) (BOp ExptOp:input) = sr (BOp ExptOp:s) input
-- sr s@(PA e2 : BOp AddOp : PA e1 : stack) (BOp ExptOp:input) = sr (BOp ExptOp:s) input
-- sr s@(PA e2 : BOp AddOp : PA e1 : stack) (BOp MulOp:input)  = sr (BOp MulOp:s) input
sr (PA e2 : BOp AddOp : PA e1 : stack) input = sr (PA (Add e1 e2) : stack) input -- AExpr -> AExpr (AddOp) AExpr
sr (PA e2 : BOp MulOp : PA e1 : stack) input = sr (PA (Mul e1 e2) : stack) input
sr (PA e2 : BOp ModOp : PA e1 : stack) input = sr (PA (Mod e1 e2) : stack) input
sr (PA e2 : BOp ExptOp : PA e1 :stack) input = sr (PA (Expt e1 e2) : stack) input
sr (PA e2 : BOp DivOp : PA e1 : stack) input = sr (PA (Div e1 e2) : stack) input
sr (PA e : NegOp : stack)              input = sr (PA (Neg e) : stack) input -- AExpr -> - (AExpr)

sr (RPar : PA e : LPar : stack)        input = sr (PA e : stack) input -- AExpr -> ( AExpr )
sr (RPar : PB e : LPar : stack)        input = sr (PB e : stack) input

sr (PA a2 : BOp EqlOp : PA a1 : stack)  input = sr (PB (Eql a1 a2) : stack) input -- BEXpr -> AExpr == AExpr
sr (PA a2 : BOp LtOp : PA a1 : stack)  input = sr (PB (Lt a1 a2) : stack) input -- BExpr -> AExpr (BOp LtOp) AExpr
sr (PB b2 : BOp OrOp : PB b1 : stack)  input = sr (PB (Or b1 b2) : stack) input     -- BEXpr -> BExpr \/ BExpr
sr (PB b : NegOp : stack) input              = sr (PB (Not b) : stack) input   -- BExpr -> Not BExpr

sr (PA a : BOp AssignOp : PA (Var c) : stack) input = sr (PI (Assign c a) : stack) input -- Instr -> Var AssignOp AExpr
sr (PI i : PB b : Keyword "while" : stack) input = sr (PI (While b i) : stack) input -- Instr -> While BExpr Instr

sr (RBra : PI i : stack) input = sr (PDo [i] : stack) input
sr (RBra : stack) input = sr (PDo [] : stack) input
sr (PDo s : Semi : PI i : stack) input = sr (PDo (i:s) : stack) input
sr (PDo s : LBra : stack) input = sr (PI (Do s) : stack) input
sr stack                           (i:input) = sr (i:stack) input
sr stack [] = stack

main :: IO ()
main = do
  let filename = "testcase1.imp"
  contents <- readFile filename
  let analyzed = analyze contents
  putStrLn "Here is the result of lexical analysis:"
  putStrLn (show analyzed)
  putStrLn "------------------------------------------"
  let parsed = sr [] (LBra : analyzed ++ [RBra])
  putStrLn "Here is the result of parsing:"
  putStrLn (show parsed)

testAExpr :: [Token] -- 2x^2-3y^z/4
testAExpr = analyze "( 2 * ( x ^ 2 ) ) + ( - ( ( 3.1 * ( y ^ z ) ) / 4.0 ) )"

testAExpr2 :: [Token]
testAExpr2 = analyze "2*x^2+(-3*y^z/4)"

easy :: [Token]
easy = analyze "2*x^2"

easy2 :: [Token]
easy2 = analyze "y+(1+2)*x^3+4"
