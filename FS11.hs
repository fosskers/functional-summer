module FS11 where

import Data.Char

----------
-- CIPHERS
----------

newtype Key = Key String

-- | Assumes capital letters.
vigenere :: Key -> String -> String
vigenere (Key key) msg = zipWith encrypt full msg
  where full = concat $ repeat key
        encrypt k c = shift (ord k - 65) c

-- | Single-Char caesar cipher.
shift :: Int -> Char -> Char
shift _ ' ' = ' '
shift n msg | c > 90 = chr $ c - 26
            | otherwise = chr c
  where c = ord msg + n

--------------
-- AS-PATTERNS
--------------

isSubseqOf :: Eq a => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf (x:_) [] = False
isSubseqOf foo@(c:cs) (t:ts) | c == t = isSubseqOf cs ts
                             | otherwise = isSubseqOf foo ts

-- Looks unsafe, but it actually isn't since:
--    words "" == []
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\w@(c:cs) -> (w, toUpper c : cs)) . words

-----------------
-- HUTTON'S RAZOR
-----------------

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b
