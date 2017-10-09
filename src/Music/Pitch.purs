module Music.Pitch where

import Prelude
import Data.Newtype (class Newtype, over2, wrap)
import Data.Monoid (class Monoid)

newtype Hz = Hz Number

derive instance newtypeHz :: Newtype Hz _
derive newtype instance eqHz :: Eq Hz
derive newtype instance ordHz :: Ord Hz

pitchFreq :: Hz -> Number
pitchFreq (Hz freq) = freq

instance semigroupHz :: Semigroup Hz where
  append = over2 Hz (+)

instance monoidHz :: Monoid Hz where
  mempty = wrap 0.0

instance hzShow :: Show Hz where
  show (Hz s) = (show s) <> "hz"

class Pitch a where
  toHz :: a -> Hz
