module Main where

import Exec
import Types
import Lex
import Parse

listInstr :: [Token] -> [Instr]
listInstr [] = []
listInstr (PI ins : ts) = [ins] ++ listInstr ts
listInstr (a:as) = listInstr as


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

  let parsed = sr [] $ lexed
  putStrLn "Here is the result of parsing:"
  putStrLn (show parsed)
  putStrLn "------------------------------------------"

  let answer = exec ( Do(reverse $ listInstr parsed)) []
  putStrLn "Here is the result of the program:"
  putStrLn (show answer)
