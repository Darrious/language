module Parse where

import Types


sr :: [Token] -> [Token] -> [Token]
sr (CSym c : stack)               i = sr (PA (Const (IntPrim c)) : stack) i
sr (BSym c : stack)               i = sr (PA (Const (BoolPrim c)) : stack) i
sr (VSym v : Type _  : stack )    i = sr (PA (Var v) : stack) i
sr (VSym b : stack)               i = sr (PA (Var b) : stack) i

sr s@(PA e2 : BOp  op1 : PA e1 : stack) (BOp op2 : i) | op1 < op2
                                                        = sr (BOp op2 : s) i
sr (PA e2 : BOp AddOp : PA e1 : stack) i = sr (PA (Add e1 e2) : stack) i
sr (PA e2 : BOp MulOp : PA e1 : stack) i = sr (PA (Mul e1 e2) : stack) i
sr (PA e2 : BOp DivOp : PA e1 : stack) i = sr (PA (Div e1 e2) : stack) i
sr (PA e2 : BOp SubOp : PA e1 : stack) i = sr (PA (Sub e1 e2) : stack) i
sr (PA e2 : BOp ModOp : PA e1 : stack) i = sr (PA (Mod e1 e2) : stack) i
sr (RPar : PA e : LPar : stack)        i = sr (PA e : stack) i
sr (RPar : PB e : LPar : stack)        i = sr (PB e : stack) i
sr (PA a2 : BOp EqlOp : PA a1 : stack)  i = sr (PB (Eql a1 a2) : stack) i
sr (PA a2 : BOp LtOp : PA a1 : stack)  i  = sr (PB (Lt a1 a2) : stack) i
sr (PA a2 : BOp GtOp : PA a1 : stack)  i  = sr (PB (Gt a1 a2) : stack) i
sr (PB b2 : BOp OrOp : PB b1 : stack)  i  = sr (PB (Or b1 b2) : stack) i
sr (PB b : UOp NotOp : stack) i           = sr (PB (Not b) : stack) i

sr (Semi : PA a : BOp AssignOp : PA (Var c) : stack) i
                                            = sr (PI (Assign c a) : stack) i

sr (PI ins : PB b : Keyword "while" : stack) i
                                            = sr (PI (While b ins) : stack) i

sr (PI ins : PA (Var b) : Keyword "while" : stack) i
                                      = sr (PI (While (BVar b) ins) : stack) i

sr (PI ins : Keyword "else" : PI i2 : Keyword "then" : PB b : Keyword "if" : stack) i
                                             = sr (PI (IfThenElse b i2 ins) : stack) i

sr (PI ins : Keyword "else" : PI i2 : Keyword "then" : PA (Var b) : Keyword "if" : stack) i
                                          = sr (PI (IfThenElse (BVar b) i2 ins) : stack) i
-- sr (FSym f : ts ) (LPar : RPar : is)         = sr (PA (FApply f []) : ts) is
-- sr (FSym f : ts ) (LPar : is)                = sr (PF f [] : ts) is
-- sr (RPar : PA a : PF f args : ts) i          = sr (PA (FApply f (reverse $ a:args)):ts) i
-- sr (Comma : PA a : PF f args : ts) i         = sr (PF f (a:args) : ts) i

sr (RBra : PI ins : stack) i = sr (PDo [ins] : stack) i
sr (RBra : stack) i = sr (PDo [] : stack) i
sr (PDo s  : PI ins : stack) i = sr (PDo (ins:s) : stack) i
sr (PDo s : LBra : stack) i = sr (PI (Do s) : stack) i
sr stack                           (ins:i) = sr (ins:stack) i
sr stack [] = stack
