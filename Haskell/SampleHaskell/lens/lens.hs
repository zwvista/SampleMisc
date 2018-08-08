-- Some of the examples in this chapter require a few GHC extensions:
-- TemplateHaskell is needed for makeLenses; RankNTypes is needed for
-- a few type signatures later on. 
{-# LANGUAGE TemplateHaskell, RankNTypes #-}

import Control.Lens
import Control.Monad.State

data Point = Point
    { _positionX :: Double
    , _positionY :: Double
    } deriving (Show)
makeLenses ''Point

data Segment = Segment
    { _segmentStart :: Point
    , _segmentEnd :: Point
    } deriving (Show)
makeLenses ''Segment

makePoint :: (Double, Double) -> Point
makePoint (x, y) = Point x y

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)

testPoint = makePoint (2,3)
testSeg = makeSegment (0, 1) (2, 4)

pointCoordinates :: Traversal Point Point Double Double
-- :: Applicative f => (Double -> f Double) -> Point -> f Point
pointCoordinates g (Point x y) = Point <$> g x <*> g y

deleteIfNegative x = if x < 0 then Nothing else Just x

extremityCoordinates :: Traversal Segment Segment Double Double
-- :: Applicative f => (Double -> f Double) -> Segment -> f Segment
extremityCoordinates g (Segment start end) =
    Segment <$> pointCoordinates g start <*> pointCoordinates g end

scaleSegment :: Double -> Segment -> Segment
scaleSegment x = over extremityCoordinates (x *)

stateExample :: State Segment ()
stateExample = do
    segmentStart .= makePoint (0,0)
    zoom segmentEnd $ do
        positionX += 1
        positionY *= 2
        pointCoordinates %= negate

unmakePoint :: Point -> (Double, Double)
unmakePoint (Point x y) = (x,y)

pointPair :: Iso' Point (Double, Double)
pointPair = iso unmakePoint makePoint

