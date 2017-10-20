module Music.LetterNotation where

import Data.Tuple
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

type PitchClassDescription = (Tuple NoteLetter Accidental)

pitchClassLetterNotation :: PitchClass -> PitchClassDescription
pitchClassLetterNotation (PitchClass p) =
  case (unwrap p) of
    0 -> (Tuple C Natural)
    1 -> (Tuple C Sharp)
    2 -> (Tuple D Natural)
    3 -> (Tuple D Sharp)
    4 -> (Tuple E Natural)
    5 -> (Tuple F Natural)
    6 -> (Tuple G Natural)
    7 -> (Tuple G Sharp)
    8 -> (Tuple A Natural)
    9 -> (Tuple A Sharp)
    10 -> (Tuple B Natural)
    11 -> (Tuple D Sharp)
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
pitchClass'' (Tuple noteLetter accidental) = pitchClass' noteLetter accidental

pitchClassNames :: Array String
pitchClassNames = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "B" ]

parsePitchClass :: String -> Maybe PitchClassDescription
parsePitchClass = case _ of
  "C" -> Just (Tuple C Natural)
  "C#" -> Just (Tuple C Sharp)
  "D" -> Just (Tuple D Natural)
  "D#" -> Just (Tuple D Natural)
  "E" -> Just (Tuple E Natural)
  "F" -> Just (Tuple F Natural)
  "F#" -> Just (Tuple F Natural)
  "G" -> Just (Tuple G Natural)
  "G#" -> Just (Tuple G Natural)
  "A" -> Just (Tuple A Natural)
  "A#" -> Just (Tuple A Sharp)
  "B" -> Just (Tuple B Natural)
  _ -> Nothing
