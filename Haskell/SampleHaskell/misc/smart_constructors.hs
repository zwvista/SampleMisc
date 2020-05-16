{-# LANGUAGE FlexibleInstances #-}

module Resistor (
         Resistor,       -- abstract, hiding constructors
         metalResistor1,  -- only way to build a metal resistor
         metalResistor2,  -- only way to build a metal resistor
         Resistor3,       -- abstract, hiding constructors
         resistor3,  -- only way to build a metal resistor
       ) where

import Control.Exception(assert)

-- runtime check

data Resistor = Metal   Bands
              | Ceramic Bands 
                deriving Show

type Bands = Int

metalResistor1 :: Bands -> Resistor
metalResistor1 n | n < 4 || n > 8 = error "Invalid number of resistor bands" 
                | otherwise      = Metal n

metalResistor2 :: Bands -> Resistor
metalResistor2 n = assert (n >= 4 && n <= 8) $ Metal n

{-
*Resistor> metalResistor1 4
Metal 4
*Resistor> metalResistor2 7
Metal 7
*Resistor> metalResistor1 9
*** Exception: Invalid number of resistor bands
CallStack (from HasCallStack):
  error, called at smart_constructors.hs:18:37 in main:Resistor
*Resistor> metalResistor2 0
*** Exception: Assertion failed
CallStack (from HasCallStack):
  assert, called at smart_constructors.hs:22:20 in main:Resistor
-}

-- compile-time check

data Z   = Z
data S a = S a

class Card c where
 
instance Card Z where
instance (Card c) => Card (S c) where

class Card size => InBounds size where
 
instance InBounds (S (S (S (S Z)))) where                 -- four
instance InBounds (S (S (S (S (S Z))))) where             -- five
instance InBounds (S (S (S (S (S (S Z)))))) where         -- six
instance InBounds (S (S (S (S (S (S (S Z))))))) where     -- seven
instance InBounds (S (S (S (S (S (S (S (S Z)))))))) where -- eight

d0  = undefined :: Z
d3  = undefined :: S (S (S Z))
d4  = undefined :: S (S (S (S Z)))
d6  = undefined :: S (S (S (S (S (S Z)))))
d8  = undefined :: S (S (S (S (S (S (S (S Z)))))))
d10 = undefined :: S (S (S (S (S (S (S (S (S (S Z)))))))))

data Resistor3 size = Resistor3 deriving Show

resistor3 :: InBounds size => size -> Resistor3 size
resistor3 _ = Resistor3

{-
*Resistor> resistor3 d0

<interactive>:15:1: error:
    • No instance for (InBounds Z) arising from a use of ‘resistor3’
    • In the expression: resistor3 d0
      In an equation for ‘it’: it = resistor3 d0
*Resistor> resistor3 d3

<interactive>:16:1: error:
    • No instance for (InBounds (S (S (S Z))))
        arising from a use of ‘resistor3’
    • In the expression: resistor3 d3
      In an equation for ‘it’: it = resistor3 d3
*Resistor> resistor3 d4
Resistor3
*Resistor> :t resistor3 d4
resistor3 d4 :: Resistor3 (S (S (S (S Z))))
*Resistor> resistor3 d6
Resistor3
*Resistor> resistor3 d8
Resistor3
*Resistor> :t resistor3 d8
resistor3 d8 :: Resistor3 (S (S (S (S (S (S (S (S Z))))))))
*Resistor> resistor3 d10

<interactive>:22:1: error:
    • No instance for (InBounds
                         (S (S (S (S (S (S (S (S (S (S Z)))))))))))
        arising from a use of ‘resistor3’
    • In the expression: resistor3 d10
-}
