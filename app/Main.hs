module Main where

data Symbol = Add | Sub | Mul | Div | ParO | ParC | Num Integer
  deriving (Show)

parseDigit :: Char -> Symbol
parseDigit '0' = Num 0
parseDigit '1' = Num 1
parseDigit '2' = Num 2
parseDigit '3' = Num 3
parseDigit '4' = Num 4
parseDigit '5' = Num 5
parseDigit '6' = Num 6
parseDigit '7' = Num 7
parseDigit '8' = Num 8
parseDigit '9' = Num 9



lexer :: String -> [Symbol]
lexer [] = []
lexer ('+':rest) = [Add] ++ lexer rest
lexer ('-':rest) = [Sub] ++ lexer rest
lexer ('*':rest) = [Mul] ++ lexer rest
lexer ('/':rest) = [Div] ++ lexer rest
lexer ('(':rest) = [ParO] ++ lexer rest
lexer (')':rest) = [ParC] ++ lexer rest
lexer (char:rest)= [parseDigit char] ++ lexer rest


main :: IO ()
main = print (lexer "+*/56( )")
-- main = print [ParO, Num 42, Add ,Num 69, ParC, Mul, Num 2]
