module Music.Chords where

import Prelude
import Music.LetterNotation
import Music.Notes
import Music.Pitch
import Music.SetTheory
import Music.Intervals
import Data.Maybe (Maybe(..))
-- import Data.Array ((:))
import Data.Set as Set
import Data.Int as Int
import Data.List as List
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Either
import Data.Array as Array

data Chord = Chord Octave PitchClass (Set.Set IntervalClass)


data IntervalName = Unison | MinorSecond | MajorSecond | MinorThird | MajorThird | PerfectFourth
                  | Tritone  | PerfectFifth  | MinorSixth  | MajorSixth  | MinorSeventh | MajorSeventh

intervalName :: IntervalName -> IntervalClass
intervalName name = intervalClass (case name of
  Unison -> 0
  MinorSecond -> 1
  MajorSecond -> 2
  MinorThird -> 3
  MajorThird -> 4
  PerfectFourth -> 5
  Tritone -> 6
  PerfectFifth -> 7
  MinorSixth -> 8
  MajorSixth -> 9
  MinorSeventh -> 10
  MajorSeventh -> 11
  )

-- also dominant
data ChordQuality = Major | Minor | Augmented | Diminished

instance showChordQuality :: Show ChordQuality where
  show = case _ of
    Major -> "Major"
    Minor -> "Minor"
    Augmented -> "Augmented"
    Diminished -> "Dimineshed"


chordQualityNames :: Array String
chordQualityNames = [ "Major", "Minor", "Augmented", "Diminished" ]

parseChordQuality :: String -> Maybe ChordQuality
parseChordQuality = case _ of
  "Major" -> Just Major
  "Minor" -> Just Minor
  "Augmented" -> Just Augmented
  "Diminished" -> Just Diminished
  _ -> Nothing


makeTriad' :: ChordQuality -> PitchClassDescription -> Octave -> Chord
makeTriad' cq pcd oct = Chord oct root intervals
  where root = pitchClass'' pcd
        thirdInterval = case cq of
          Major -> intervalName MajorThird
          Minor -> intervalName MinorThird
          Augmented -> intervalName MajorThird
          Diminished -> intervalName MinorThird
        fifthInterval = case cq of
          Major -> intervalName PerfectFifth
          Minor -> intervalName PerfectFifth
          Augmented -> intervalName MinorSixth
          Diminished -> intervalName Tritone
        intervals = (Set.singleton thirdInterval) <> (Set.singleton fifthInterval)

makeTriad :: ChordQuality -> NoteLetter -> Accidental -> Octave -> Chord
makeTriad cq nl accidental oct = makeTriad' cq (PitchClassDescription nl accidental) oct

data ChordInversion = RootPosition | Inversion Int

derive instance eqChordInversion :: Eq ChordInversion
instance ordChordInversion :: Ord ChordInversion where
  compare RootPosition (Inversion n) = LT
  compare (Inversion n) RootPosition = GT
  compare (Inversion a) (Inversion b) = compare a b
  compare RootPosition RootPosition = EQ
instance showChordInversion :: Show ChordInversion where
  show RootPosition = "Root position"
  show (Inversion n) = "Chord inversion " <> show n

parseChordInversion :: String -> Maybe ChordInversion
parseChordInversion "Root position" = Just RootPosition
parseChordInversion str = Inversion <$> maybeInt
  where rx :: Maybe Regex.Regex
        rx = hush $ Regex.regex "Chord inversion (\\d+)" RegexFlags.ignoreCase
        maybeInt :: Maybe Int
        maybeInt = rx >>= (\r -> Regex.match r str) >>= Array.catMaybes >>> (\m -> Array.index m 1) >>= Int.fromString

chordNotes :: ChordInversion -> Chord -> (Set.Set Note)
chordNotes inversion (Chord octave pc intervalClasses) =
  (Set.singleton $ root) <> (Set.map (\i -> transposeNote root i) intervals)
  where root = Note octave pc
        intervals :: Set.Set Interval
        intervals =
          case inversion of
            RootPosition -> Set.map toInterval intervalClasses
            Inversion 1 -> Set.map (toInterval >>> (_ <> iinverse $ toInterval (Octave 1))) intervalClasses
            Inversion _ -> Set.map toInterval intervalClasses

numberOfChordNotes :: Chord -> Int
numberOfChordNotes (Chord _ _ intervalClasses) = 1 + Set.size intervalClasses

possibleChordInversions :: Chord -> Set.Set ChordInversion
possibleChordInversions chord = (Set.singleton RootPosition) <> (Set.fromFoldable $ map Inversion $ List.range 1 $ (numberOfChordNotes chord) - 1)

chordRoot :: Chord -> Note
chordRoot (Chord octave pc intervalClasses) = Note octave pc
