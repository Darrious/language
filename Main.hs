module Main where

import Exec
import Types
import Lex
import Parse
import Debug.Trace

listInstr :: [Token] -> [Instr]
listInstr [] = []
listInstr (PI ins : ts) = [ins] ++ listInstr ts
listInstr (a:as) = listInstr as


-- IO
main :: IO ()
main = do
  -- putStrLn "Enter a .imp file with code."
  -- filename <- getLine
  let filename = "testFun.imp"
  contents <- readFile filename

  let lexed = lexer contents
  putStrLn "Here is the result of lexical analysis:"
  putStrLn (show lexed)
  putStrLn "------------------------------------------"

  let parsed = sr [] $ lexed
  putStrLn "Here is the result of parsing:"
  putStrLn (show parsed)
  putStrLn "------------------------------------------"

  let update = updateDefs parsed
  let removed = removeDefs parsed
  let parse = exec update ( Do(reverse $ listInstr removed)) []
  putStrLn "Here are the functions:"
  putStrLn (show update)
  putStrLn ""
  putStrLn (show  (removed))
  putStrLn ""
  putStrLn "Here is the result of the program:"
  putStrLn (show parse)
