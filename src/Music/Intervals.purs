module Music.Intervals where

import Prelude
import Data.Newtype (class Newtype)
import Data.Group
import Data.Monoid
import Data.Semigroup.Commutative (class Commutative)

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

instance monoidInterval :: Monoid Interval where
  mempty = (Interval 0)

instance groupInterval :: Group Interval where
  ginverse :: Interval -> Interval
  ginverse (Interval i) = (Interval (-i))

instance commutativeInterval :: Commutative Interval

newtype Octave = Octave Int

derive instance newtypeOctave :: Newtype Octave _
derive newtype instance eqOctave :: Eq Octave
instance showOctave :: Show Octave where
  show (Octave n) = show n

instance toIntervalOctave :: ToInterval Octave where
  toInterval (Octave n) = Interval (n * 12)
