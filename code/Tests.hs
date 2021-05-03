module Tests where

import Exec
import Types
import Lex
import Parse

-- Environment
-- integer -> Values
env1 = [("X", IntPrim 1), ("Y", IntPrim  99)]
env2 = [("X", IntPrim 23), ("Y", IntPrim 3)]
env3 = [("X", IntPrim 12), ("Y", IntPrim 1)]
env4 = [("X", IntPrim 5), ("Y", IntPrim 6), ("Z", IntPrim 11)]
env5 = [("Y", IntPrim 6), ("Z", IntPrim 11)]
env6 = [("X", IntPrim 5),("flag", BoolPrim True)]
env7 = [("X", IntPrim 10)]
env8 = [("Z", BoolPrim True)]



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
test5 = "fact:=1; c :=1 ; while (! (5 < c)) { c := (c+1); fact := (fact * c)} "

test6 :: String
test6 = "x:=3; if (x>5) then { x:=0; } else {x:=1;}"

test7 = (sr [] (lexer test6))
