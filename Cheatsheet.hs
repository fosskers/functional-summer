module Cheatsheet where

import Data.Maybe

--------
-- TYPES
--------

-- | A type with no constructors. An instance of it can't be created, but it can
-- be used in type signatures.
data WebMercator

-- | A Sum Type, similar to an "enum" in other languages.
data Colour = Red | Green | Blue deriving (Eq, Ord, Show)

-- | A Product Type, similar to a "struct" in other languages.
data User = User String Int Float deriving (Eq, Ord, Show)

-- | A Product Type using "record syntax". `name`, etc., are functions
-- that can be used like any other to compose, map, etc.
-- The actual type of `name` is:
--   Uzer -> String
data Uzer = Uzer { name :: String, age :: Int, coolness :: Float } deriving (Eq, Ord, Show)

-- | A type alias which has no special meaning at compile time or runtime.
-- WM /is/ WebMercator in every sense.
type WM = WebMercator

-- | A newtype; stronger than a type alias but weaker than a data declaration.
-- Can only contain one field. Acts like a unique type for the purposes of
-- type checking, but all wrapping and unwrapping calls are stripped at compile time,
-- so no performance penalty exists for using these.
newtype Raster a = Raster { values :: [a] } deriving (Eq, Ord, Show)

-- | `p` here is a "phantom type". It has no corresponding field within the data structure,
-- but still lets us use it for type checking.
newtype Razter p a = Razter { valuez :: [a] } deriving (Eq, Ord, Show)

r0 :: Razter WebMercator Int
r0 = undefined

r1 :: Razter Bool Int
r1 = undefined

-- Won't compile!
-- foo :: Bool
-- foo = r0 == r1

-------------------
-- PATTERN MATCHING
-------------------

-- | Match directly on argument values.
isOne :: Int -> String
isOne 1 = "You did it!"
isOne _ = "Shucks"  -- Use of a pattern match "wildcard".

-- | Match on data structure "shapes".
isEmpty :: [a] -> String
isEmpty [] = "Cool"
isEmpty (_:rest) = isEmpty rest  -- Silly recursion.

maybeOne :: Maybe Int -> String
maybeOne (Just 1) = "Wicked"
maybeOne _ = "Not this time"

----------------------
-- MAP / FILTER / FOLD
----------------------

-- | We can `map` functions purely over lists.
doubleThem :: Num a => [a] -> [a]
doubleThem ns = map (\n -> n * 2) ns

-- | Terse version.
doubleThem' :: Num a => [a] -> [a]
doubleThem' = map (* 2)

-- | Anything that is a `Functor` be also be mapped over with `fmap`.
-- Try running this on Just 4, Right 8, [1,2,3], etc.
doubleIt :: (Functor f, Num a) => f a -> f a
doubleIt = fmap (* 2)

-- | Reduce a list to a subset of its original values based on a test.
--
-- @
-- >>> evensOnly [1,2,3,4]
-- [2,4]
-- @
evensOnly :: [Int] -> [Int]
evensOnly = filter even

-- | Use `foldl` to reduce a collection to a single value.
fuseThem :: [String] -> String
fuseThem strings = foldl (\acc s -> s ++ acc) "" strings
-- or: foldl (++) ""

-- | Use `foldr` to reconstruct the container, possibly giving it
-- less values, possible changing the type of container itself (e.g. Map to List).
oddsOnly :: [Maybe Int] -> [Int]
oddsOnly = foldr f []
  where f (Just n) acc | odd n = n : acc
                       | otherwise = acc
        f _ acc = acc

-----------------------------
-- MODELLING FAILURE / ERRORS
-----------------------------

-- TLDR; You will never need to throw an exception.

-- | If a computation can logically fail and you don't care about
-- providing any error message, use `Maybe`.
safeRoot :: (Ord a, Floating a) => a -> Maybe a
safeRoot n | n >= 0 = Just $ sqrt n
           | otherwise = Nothing

-- | If you do want to yield a message, pass it back with `Left`
-- in the case of an error. `Right` otherwise.
saferRoot :: (Ord b, Floating b) => b -> Either String b
saferRoot n | n >= 0 = Right $ sqrt n
            | otherwise = Left "Less than 0!"

-- | "Escaping" `Maybe` via pattern matching.
escapeMaybe0 :: Maybe Int -> String
escapeMaybe0 (Just 1) = "It's one!"
escapeMaybe0 _ = "Darn"

-- | One-liner via the `maybe` function.
escapeMaybe1 :: Maybe Int -> String
escapeMaybe1 = maybe "Darn" (\_ -> "It's one!")

-- | Yolo cowboy escaping. Beware of bottom.
escapeMaybe2 :: Maybe Int -> String
escapeMaybe2 = show . fromJust

-- | Escape `Either` via pattern matching.
escapeEither0 :: Either String Int -> String
escapeEither0 (Left err) = err
escapeEither0 (Right n) = show n

-- | `either` is an analogue to `maybe`.
escapeEither1 :: Either String Int -> String
escapeEither1 = either id show
