module Main where

data Symbol = Add | Sub | Mul | Div | ParO | ParC | Num Integer
  deriving Show


parseDigit :: Char -> Maybe Symbol
parseDigit '0' = Just (Num 0)
parseDigit '1' = Just (Num 1)
parseDigit '2' = Just (Num 2)
parseDigit '3' = Just (Num 3)
parseDigit '4' = Just (Num 4)
parseDigit '5' = Just (Num 5)
parseDigit '6' = Just (Num 6)
parseDigit '7' = Just (Num 7)
parseDigit '8' = Just (Num 8)
parseDigit '9' = Just (Num 9)
parseDigit _ = Nothing


lexer :: String -> [Symbol]
lexer [] = []
lexer ('+':rest) = [Add] ++ lexer rest
lexer ('-':rest) = [Sub] ++ lexer rest
lexer ('*':rest) = [Mul] ++ lexer rest
lexer ('/':rest) = [Div] ++ lexer rest
lexer ('(':rest) = [ParO] ++ lexer rest
lexer (')':rest) = [ParC] ++ lexer rest
lexer (char:rest) = case parseDigit char of
    Nothing -> lexer rest
    Just x -> [x] ++ lexer rest


main :: IO ()
main = print (lexer "+*/56( )")
-- main = print [ParO, Num 42, Add ,Num 69, ParC, Mul, Num 2]
