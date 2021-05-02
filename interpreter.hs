module Main where

import Types
import Exec
import Tests
import Lex
import Parse

funTest = "Exp(5, c+1)"

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
  let filename = "testIf2.imp"
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
