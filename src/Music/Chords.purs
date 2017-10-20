module Music.Chords where

import Prelude

import Music.Notes
import Music.SetTheory
import Music.LetterNotation
import Music.Pitch
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
data ThirdQuality = Major | Minor --  | Sus2 | Sus4
data FifthQuality = Perfect | Augmented | Diminished


makeTriad' :: ThirdQuality -> FifthQuality -> PitchClassDescription -> Octave -> Chord
makeTriad' thirdQ fifthQ pcd oct = Chord oct root intervals
  where root = pitchClass'' pcd
        thirdInterval = case thirdQ of
          Major -> intervalName MajorThird
          Minor -> intervalName MinorThird
        fifthInterval = case fifthQ of
          Perfect -> intervalName PerfectFifth
          Augmented -> intervalName MinorSixth
          Diminished -> intervalName Tritone
        intervals = (Set.singleton thirdInterval) <> (Set.singleton fifthInterval)

makeTriad :: ThirdQuality -> FifthQuality -> NoteLetter -> Accidental -> Octave -> Chord
makeTriad tq fq nl accidental oct = makeTriad' tq fq (Tuple nl accidental) oct

chordNotes :: Chord -> (Array Note)
chordNotes (Chord octave pc intervals) =
  map (\i -> Note octave (transposePitchClass pc i)) (Set.toUnfoldable intervals)
