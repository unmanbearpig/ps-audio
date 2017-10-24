module Music.IntervalClass where

import Prelude
import Data.Newtype (class Newtype)
import Data.Monoid (class Monoid)
import Data.Group (class Group)
import Music.Intervals (class ToInterval, Interval(..))

newtype IntervalClass = IntervalClass Int

intervalClass :: Int -> IntervalClass
intervalClass i = IntervalClass $ i `mod` 12

derive instance eqIntervalClass :: Eq IntervalClass
derive instance ordIntervalClass :: Ord IntervalClass

instance newtypeIntervalClass :: Newtype IntervalClass Int where
  wrap = intervalClass
  unwrap (IntervalClass ic) = ic

instance semigroupIntervalClass :: Semigroup IntervalClass where
  append (IntervalClass a) (IntervalClass b) = intervalClass $ a + b

instance boundedIntervalClass :: Bounded IntervalClass where
  bottom = IntervalClass 0
  top = IntervalClass 11

instance monoidIntervalClass :: Monoid IntervalClass where
  mempty = intervalClass 0

instance groupIntervalClass :: Group IntervalClass where
  ginverse (IntervalClass i) = intervalClass $ 12 - i

instance showIntervalClass :: Show IntervalClass where
  show (IntervalClass ic) = "(IntervalClass " <> show ic <> ")"

instance toIntervalIntervalClass :: ToInterval IntervalClass where
  toInterval (IntervalClass n) = Interval n

semitone :: IntervalClass
semitone = intervalClass 1

wholeTone :: IntervalClass
wholeTone = intervalClass 2
