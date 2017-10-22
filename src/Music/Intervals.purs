module Music.Intervals where

import Prelude
import Data.Newtype (class Newtype)

newtype Interval = Interval Int

class ToInterval a where
  toInterval :: a -> Interval

derive instance newtypeInterval :: Newtype Interval _
derive instance eqInterval :: Eq Interval
derive newtype instance ordInterval :: Ord Interval
instance semigroupInterval :: Semigroup Interval where
  append (Interval i) (Interval j) = Interval (i + j)

instance showInterval :: Show Interval where
  show (Interval n) = show n <> " semitones"

iinverse :: Interval -> Interval
iinverse (Interval i) = (Interval (-i))

newtype Octave = Octave Int

derive instance newtypeOctave :: Newtype Octave _
derive newtype instance eqOctave :: Eq Octave
instance showOctave :: Show Octave where
  show (Octave n) = show n

instance toIntervalOctave :: ToInterval Octave where
  toInterval (Octave n) = Interval (n * 12)
