{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}

module NamedTypeclass where

import Prelude hiding (Monoid, mempty, (<>))

data Sum
data Product

class Monoid b a where
  (<>)   :: a -> a -> a
  mempty :: a

instance Monoid Sum Int where
  x <> y = x + y
  mempty = 0

instance Monoid Product Int where
  x <> y = x * y
  mempty = 1

instance Monoid b [a] where
  x <> y = x ++ y
  mempty = []

fold :: forall b a . Monoid b a => [a] -> a
fold []       = mempty @b
fold (x : xs) = (<>) @b x (fold @b xs)

sum :: [Int] -> Int
sum = fold @Sum

product :: [Int] -> Int
product = fold @Product

concat :: [[a]] -> [a]
concat = fold
