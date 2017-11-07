{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module FS17 where

import Data.Monoid
import GHC.Generics
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

----------
-- LOOKUPS
----------

added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1,2,3] [4,5,6])

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- Wrong?
--   sum (a, b) == b
summed :: Maybe Integer
summed = (\a b -> sum (a,b)) <$> y <*> z

-----------
-- IDENTITY
-----------

newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> i = fmap f i

-----------
-- CONSTANT
-----------

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a
  -- fmap _ c = c  -- doesn't work!

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty

  -- There is no spoon, Neo. `f` is not a function!
  (Constant f) <*> (Constant a) = Constant (f `mappend` a)

----------------
-- CHECKING LAWS
----------------

applic :: IO ()
applic = quickBatch $ applicative [("hi", "you", 1 :: Int)]

-------
-- LIST
-------

data List a = Nil | Cons a (List a) deriving (Eq, Show, Generic, CoArbitrary)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = foldr Cons Nil <$> listOf arbitrary

-- Boilerplate.
instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Monoid (List a) where
  mempty = Nil

  Cons a Nil `mappend` l = Cons a l
  Cons a as  `mappend` l = Cons a (as `mappend` l)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) $ fmap f l

instance Applicative List where
  pure a = Cons a Nil

  Cons f fs <*> l@(Cons a as) = (f <$> l) `mappend` (fs <*> as)

fooList :: List Int
fooList = Cons 1 (Cons 2 (Cons 3 Nil))

list :: IO ()
list = quickBatch $ applicative [(fooList, fooList, fooList)]

----------
-- ZIPLIST
----------

newtype ZipList a = ZipList { zippy :: List a } deriving (Eq, Show, Generic, CoArbitrary)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _   = Nil
take' n (Cons a l) = Cons a (take' (n-1) l)

instance Eq a => EqProp (ZipList a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 $ zippy xs
          ys' = take' 3000 $ zippy ys

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList $ fmap f xs

instance Applicative ZipList where
  pure = ZipList . pure

  ZipList Nil <*> _           = ZipList Nil
  ZipList _   <*> ZipList Nil = ZipList Nil
  ZipList (Cons f fs) <*> ZipList (Cons a as) = ZipList . Cons (f a) . zippy $ (ZipList fs <*> ZipList as)

ziplist :: IO ()
ziplist = quickBatch $ applicative [(ZipList fooList, ZipList fooList, ZipList fooList)]

-------------
-- VALIDATION
-------------

data Validation e a = Failure e | Success a deriving (Eq, Show, Generic, CoArbitrary)

etov :: Either e a -> Validation e a
etov (Right a) = Success a
etov (Left e)  = Failure e

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = etov <$> arbitrary

instance (Eq a, Eq e) => EqProp (Validation e a) where
  xs =-= ys = eq xs ys

instance Functor (Validation e) where
  fmap f (Success a) = Success $ f a
  fmap _ (Failure e) = Failure e  -- notice we have to "rewrap" it.

instance Monoid e => Applicative (Validation e) where
  pure = Success

  -- `es` does not contain functions, that only happens on `Success`!
  Failure es <*> Failure es' = Failure $ es <> es'
  Failure es <*> _           = Failure es
  _          <*> Failure es  = Failure es
  Success f  <*> Success a   = Success $ f a

valids :: (Validation String Int, Validation String Int, Validation String Int)
valids = (Success (1 :: Int), Failure "crap", Success (2 :: Int))

valid :: IO ()
valid = quickBatch $ applicative [valids]

---------------
-- COMBINATIONS
---------------

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos as bs cs = (\a b c -> (a,b,c)) <$> as <*> bs <*> cs
