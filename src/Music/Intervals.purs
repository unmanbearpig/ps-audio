module Music.Intervals where

import Prelude
import Data.Newtype (class Newtype, over2, wrap)
import Data.Monoid (class Monoid)

newtype Semitones = Semitones Int

derive instance newtypeSemitones :: Newtype Semitones _
derive newtype instance eqSemitones :: Eq Semitones
derive newtype instance ordSemitones :: Ord Semitones

instance monoidSemitones :: Monoid Semitones where
  mempty = wrap 0

instance semigroupSemitones :: Semigroup Semitones where
  append = over2 Semitones (+)

instance semitonesShow :: Show Semitones where
  show (Semitones s) = "(" <> (show s) <> " semitones)"

class (Eq i, Show i) <= Interval i where
  toSemitones :: i -> Semitones

instance semitoneInterval :: Interval Semitones where
  toSemitones = id

newtype Octaves = Octaves Int

derive instance newtypeOctaves :: Newtype Octaves _
derive newtype instance eqOctaves :: Eq Octaves
derive newtype instance ordOctaves :: Ord Octaves

instance monoidOctaves :: Monoid Octaves where
  mempty = wrap 0

instance semigroupOctaves :: Semigroup Octaves where
  append = over2 Octaves (+)


instance showOctaves :: Show Octaves where
  show (Octaves octs) = "(" <> (show octs) <> " octaves)"

instance octaveInterval :: Interval Octaves where
  toSemitones (Octaves octs) = Semitones $ octs * 12

unison = Semitones 0
perfectUnison = Semitones 0
minorSecond = Semitones 1
majorSecond = Semitones 2
minorThird = Semitones 3
majorThird = Semitones 4
perfectFourth = Semitones 5
perfectFifth = Semitones 7
minorSixth = Semitones 8
majorSixth = Semitones 9
minorSeventh = Semitones 10
majorSeventh = Semitones 11

data Accidental = Flat | Natural | Sharp

derive instance eqAccidental :: Eq Accidental

instance ordAccidental :: Ord Accidental where
  compare a b = if a == a then EQ
                else case a of
                  Flat -> LT
                  Sharp -> GT
                  Natural -> if b == Flat then GT else LT

instance accidentalInterval :: Interval Accidental where
  toSemitones = case _ of
    Flat -> Semitones (-1)
    Natural -> Semitones 0
    Sharp -> Semitones 1

instance accidentalShow :: Show Accidental where
  show = case _ of
    Natural -> "♮"
    Flat -> "♭"
    Sharp -> "♯"
