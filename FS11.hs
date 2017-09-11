module FS11 where

import           Data.Char
import           Data.List
import qualified Data.Map.Lazy as M

-- data User = User { name :: String, age :: Int }
-- data Fuser = Fuser { name :: String , age :: Int }

data Colour = Red | Green | Blue deriving (Enum, Show)

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

data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr deriving (Show)

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b
printExpr (Sub a b) = printExpr a ++ " - " ++ printExpr b

bad :: Expr -> Expr
bad e = Add (bad e) (bad e)

--------------
-- BINARY TREE
--------------

data BTree a = Leaf | Node (BTree a) a (BTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BTree a -> BTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) | b == a = Node left a right
                              | b < a  = Node (insert' b left) a right
                              | b > a  = Node left a (insert' a right)

mapTree :: (t -> a) -> BTree t -> BTree a
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

inorder :: BTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

foldTree :: (a -> b -> b) -> b -> BTree a -> b
foldTree f acc bt = foldr f acc $ inorder bt

--------
-- PHONE
--------
type Digit = Char
type Presses = Int

-- | Not that important.
phone :: M.Map Char [(Digit, Presses)]
phone = undefined

-- | Convert a message to key presses.
work :: String -> [(Digit, Presses)]
work = foldr f []
  where f c acc = case M.lookup c phone of
          Nothing -> acc
          Just dp -> dp ++ acc

-- | How many times was anything clicked?
fingerTaps :: [(Digit, Presses)] -> Int
fingerTaps = foldl' (\acc (_, p) -> acc + p) 0

-- | Most popular letter in a single message.
mostPopularLetter :: String -> Char
mostPopularLetter = fst . maximumBy h . map g . groupBy f . sort . work
  where f (d,_) (d',_) = d == d'
        g = foldl1 (\(d,p) (_,p') -> (d, p + p'))
        h (_,p) (_,p') = compare p p'

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat
