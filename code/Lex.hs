module Lex where

import Types
import Data.Char

-- Classify method takes strings and assigns the correct token type
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
classify s@"ret" = Keyword s

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


-- Checks if string is a constant
isCSym :: String -> Bool
isCSym "" = False
isCSym (x:xs) = isDigit x && q1 xs
  where q1 "" = True
        q1 (y:ys) = (isDigit y && q1 ys) || (y == '.' && not (null ys) && q2 ys)
        q2 ys = all isDigit ys

-- Checks if string is a variable
isVSym :: String -> Bool
isVSym "" = False
isVSym (x:xs) = isLower x && q1 xs
  where q1 "" = True
        q1 (y:ys) = (isAlpha y || isDigit y || y `elem` "-_'") && q1 ys

-- Checks if string is function
isFun :: String -> Bool
isFun "" = False
isFun (x:xs) = isUpper x && q1 xs
  where q1 "" = True
        q1 (y:ys) = (isAlpha y || isDigit y || y `elem` "-_'") && q1 ys


-- Adds spaces to certain characters in the input. Importent for classification
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

-- Driver program for lexing, it runs preproc and classify on the input
lexer :: String -> [Token]
lexer "" = []
lexer s = map classify (words (preproc s))
