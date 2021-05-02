module Types where

-- Variables
type Vars = String
type FName = String

data Values = IntPrim Integer | BoolPrim Bool | VNull
    deriving Show
type Env = [(Vars, Values)]

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

-- Lexical Analysis
data UOps = NotOp deriving Show
data BOps = AddOp | SubOp | MulOp | DivOp | AndOp | OrOp | EqlOp | LtOp | GtOp
          | AssignOp | ModOp
    deriving (Show,Enum,Eq,Ord)


data Types = IntType | DoubleType | BoolType | StringType
   deriving Show


data Array = Arr [Values]
data LList = Node Values LList | Null deriving Show
data Struct = Array | LList deriving Show



-- function vs var names
-- instr block
-- Decl
-- add method type to AExpr
-- data Method = Function FName Types Decl [Instr]
--
-- type Decl = [(Vars, Types)]




data Token = VSym Vars | CSym Integer | BSym Bool | FSym FName
           | UOp UOps | BOp BOps
           | LPar | RPar | LBra | RBra | Semi
           | Keyword String
           | Err | Comma | Type Types
           | PA AExpr | PB BExpr | PI Instr  | PDo [Instr]
           | PF FName [AExpr]
    deriving Show
