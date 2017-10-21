module Music.LetterNotation where

import Music.SetTheory
import Prelude
import Data.Enum (class Enum)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafeCrashWith)

data NoteLetter = C | D | E | F | G | A | B

derive instance eqNoteLetter :: Eq NoteLetter
derive instance ordNoteLetter :: Ord NoteLetter
instance boundedNoteLetter :: Bounded NoteLetter where
  bottom = C
  top = B

instance enumNoteLetter :: Enum NoteLetter where
  succ l = Just newLetter
    where newLetter =
            case l of
              C -> D
              D -> E
              E -> F
              F -> G
              G -> A
              A -> B
              B -> C

  pred l = Just newLetter
    where newLetter =
            case l of
              C -> B
              D -> C
              E -> D
              F -> E
              G -> F
              A -> G
              B -> A

instance showNoteLetter :: Show NoteLetter where
  show = case _ of
    C -> "C"
    D -> "D"
    E -> "E"
    F -> "F"
    G -> "G"
    A -> "A"
    B -> "B"

data Accidental = Flat | Natural | Sharp

derive instance eqAccidental :: Eq Accidental
derive instance ordAccidental :: Ord Accidental
instance boundedAccidental :: Bounded Accidental where
  bottom = Flat
  top = Sharp

instance showAccidental :: Show Accidental where
  show = case _ of
    Flat -> "♭"
    Natural -> "♮"
    Sharp -> "♯"

data PitchClassDescription = PitchClassDescription NoteLetter Accidental

instance showPitchClassDescirption :: Show PitchClassDescription where
  show (PitchClassDescription noteLetter accidental) = (show accidental) <> (show noteLetter)

derive instance eqPitchClassDescription :: Eq PitchClassDescription

pitchClassLetterNotation :: PitchClass -> PitchClassDescription
pitchClassLetterNotation (PitchClass p) =
  case (unwrap p) of
    0 -> (PitchClassDescription C Natural)
    1 -> (PitchClassDescription C Sharp)
    2 -> (PitchClassDescription D Natural)
    3 -> (PitchClassDescription D Sharp)
    4 -> (PitchClassDescription E Natural)
    5 -> (PitchClassDescription F Natural)
    6 -> (PitchClassDescription F Sharp)
    7 -> (PitchClassDescription G Natural)
    8 -> (PitchClassDescription G Sharp)
    9 -> (PitchClassDescription A Natural)
    10 -> (PitchClassDescription A Sharp)
    11 -> (PitchClassDescription B Natural)
    _ -> unsafeCrashWith $ "PitchClass " <> show p <> " out of bounds."

instance toPitchClassNoteLetter :: ToPitchClass NoteLetter where
  toPitchClass pc = pitchClass semitones
    where semitones = case pc of
            C -> 0
            D -> 2
            E -> 4
            F -> 5
            G -> 7
            A -> 9
            B -> 11

instance toIntervalClassAccidental :: ToIntervalClass Accidental where
  toIntervalClass ic = intervalClass semitones
    where semitones = case ic of
            Flat -> -1
            Natural -> 0
            Sharp -> 1


pitchClass' :: NoteLetter -> Accidental -> PitchClass
pitchClass' letter accidental = transposePitchClass (toPitchClass letter) (toIntervalClass accidental)

pitchClass'' :: PitchClassDescription -> PitchClass
pitchClass'' (PitchClassDescription noteLetter accidental) = pitchClass' noteLetter accidental

pitchClassNames :: Array String
pitchClassNames = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "B" ]

parsePitchClass :: String -> Maybe PitchClassDescription
parsePitchClass = case _ of
  "C" -> Just (PitchClassDescription C Natural)
  "C#" -> Just (PitchClassDescription C Sharp)
  "D" -> Just (PitchClassDescription D Natural)
  "D#" -> Just (PitchClassDescription D Natural)
  "E" -> Just (PitchClassDescription E Natural)
  "F" -> Just (PitchClassDescription F Natural)
  "F#" -> Just (PitchClassDescription F Natural)
  "G" -> Just (PitchClassDescription G Natural)
  "G#" -> Just (PitchClassDescription G Natural)
  "A" -> Just (PitchClassDescription A Natural)
  "A#" -> Just (PitchClassDescription A Sharp)
  "B" -> Just (PitchClassDescription B Natural)
  _ -> Nothing
