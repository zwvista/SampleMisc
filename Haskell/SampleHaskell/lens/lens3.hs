{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
 
import Control.Applicative (Applicative)
import Control.Lens hiding (element)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid)
 
data Atom = Atom { _element :: String, _point :: Point } deriving (Show)
data Point = Point { _x :: Double, _y :: Double } deriving (Show)
data Molecule = Molecule { _atoms :: [Atom] } deriving (Show)
data Pair a = Pair a a deriving (Functor, Foldable, Traversable)
makeLenses ''Atom
makeLenses ''Point
makeLenses ''Molecule

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)

shift :: ASetter' a Double -> a -> a
shift lens = over lens (+ 1)

atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
molecule = Molecule { _atoms = [atom1, atom2] }

{-
type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)
type ASetter' a b   = (b -> Identity b) -> (a -> Identity a)
-- ... equivalent to: (b ->          b) -> (a ->          a)
type Getting b a b  = (b -> Const b b) -> (a -> Const b a)
-- ... equivalent to: (b ->       b  ) -> (a ->       b  )
--                                        (a ->       b  )

--    +-- Bigger type
--    |
--    v
Lens' bigger smaller
--           ^
--           |
--           +--  Smaller type within the bigger type

element :: Lens' Atom String
point   :: Lens' Atom Point
x       :: Lens' Point Double
y       :: Lens' Point Double

-}

-- lens :: (a -> b) -> (a -> b -> a) -> Lens' a b
point2 :: Lens' Atom Point
point2 = lens _point (\atom newPoint -> atom { _point = newPoint })

-- point3 :: Lens' Atom Point
point3 :: Functor f => (Point -> f Point) -> Atom -> f Atom
point3 k atom = fmap (\newPoint -> atom { _point = newPoint }) (k (_point atom))

{-
(.) :: Lens' a b -> Lens' b c -> Lens' a c
type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)
(.) :: Functor f
    => ((b -> f b) -> (a -> f a))
    -> ((c -> f c) -> (b -> f b))
    -> ((c -> f c) -> (a -> f a))
point     :: Lens' Atom Point
x         :: Lens' Point Double
point . x :: Lens' Atom Double

view :: Lens' a b -> a -> b
over :: Lens' a b -> (b -> b) -> a -> a
view (point . x) :: Atom -> Double
over (point . x) :: (Double -> Double) -> (Atom -> Atom)

set  :: Lens' a b ->       b  -> a -> a
set lens b = over lens (\_ -> b)

view (lens1 . lens2) = (view lens2) . (view lens1)
view id = id
over (lens1 . lens2) = (over lens1) . (over lens2)
over id = id

atom ^. (point . x)
(^.) :: a -> Lens' a b -> b
x ^. l = view l x
-}

