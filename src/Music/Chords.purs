module Music.Chords where

import Music.LetterNotation
import Music.Notes
import Music.Pitch
import Music.SetTheory
import Prelude

import Data.Array
import Data.Set as Set
import Data.Tuple (Tuple(..))

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
makeTriad cq nl accidental oct = makeTriad' cq (Tuple nl accidental) oct

chordNotes :: Chord -> (Array Note)
chordNotes (Chord octave pc intervals) =
  (Note octave pc) : (map (\i -> Note octave (transposePitchClass pc i)) (Set.toUnfoldable intervals))
