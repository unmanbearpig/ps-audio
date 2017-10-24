module Music.PitchClass where

import Prelude
import Data.Group (ginverse)
import Music.IntervalClass (IntervalClass, intervalClass)
import Data.Newtype (class Newtype, wrap, unwrap)

newtype PitchClass = PitchClass IntervalClass

pitchClass :: Int -> PitchClass
pitchClass = PitchClass <<< intervalClass

instance newtypePitchClass :: Newtype PitchClass Int where
  wrap ic = PitchClass (wrap ic)
  unwrap (PitchClass i) = unwrap i

derive instance eqPitchClass :: Eq PitchClass
derive instance ordPitchClass :: Ord PitchClass
derive newtype instance boundedPitchClass :: Bounded PitchClass

instance showPitchClass :: Show PitchClass where
  show (PitchClass ic) = "(PitchClass " <> show ic <> ")"

class ToPitchClass a where
  toPitchClass :: a -> PitchClass

class ToIntervalClass a where
  toIntervalClass :: a -> IntervalClass

transposePitchClass :: PitchClass -> IntervalClass -> PitchClass
transposePitchClass pc ic = wrap $ (unwrap pc) + (unwrap ic)

diffPitchClasses :: PitchClass -> PitchClass -> IntervalClass
diffPitchClasses (PitchClass pc1) (PitchClass pc2) = (ginverse pc1) <> pc2
