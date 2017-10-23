module Music.SetTheory where

import Prelude
import Data.Newtype (class Newtype, wrap, unwrap)
-- import Data.Group
import Data.Monoid (class Monoid)
import Data.Set
import Music.Intervals

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

instance showIntervalClass :: Show IntervalClass where
  show (IntervalClass ic) = "(IntervalClass " <> show ic <> ")"

newtype PitchClass = PitchClass IntervalClass

pitchClass :: Int -> PitchClass
pitchClass = PitchClass <<< intervalClass

instance newtypePitchClass :: Newtype PitchClass Int where
  wrap ic = PitchClass (wrap ic)
  unwrap (PitchClass i) = unwrap i

derive instance eqPitchClass :: Eq PitchClass
derive instance ordPitchClass :: Ord PitchClass
derive newtype instance boundedPitchClass :: Bounded PitchClass
derive newtype instance monoidPitchClass :: Monoid PitchClass

instance showPitchClass :: Show PitchClass where
  show (PitchClass ic) = "(PitchClass " <> show ic <> ")"

class ToPitchClass a where
  toPitchClass :: a -> PitchClass

class ToIntervalClass a where
  toIntervalClass :: a -> IntervalClass

transposePitchClass :: PitchClass -> IntervalClass -> PitchClass
transposePitchClass pc ic = wrap $ (unwrap pc) + (unwrap ic)

instance toIntervalIntervalClass :: ToInterval IntervalClass where
  toInterval (IntervalClass n) = Interval n
