module Music where

import Prelude
import Data.Int (toNumber)
import Math (pow)

data Octave = Octave Int

data Accidental = Natural | Sharp | Flat

-- theoretically we could have double sharps or double flats as well
-- also I don't think that natural is exactly the same as no accidental
instance showAccidental :: Show Accidental where
  show acc = case acc of
    Natural -> "♮"
    Flat -> "♭"
    Sharp -> "♯"

data Pitch = Hz Number
derive instance eqPitch :: Eq Pitch

instance showPitch :: Show Pitch where
  show :: Pitch -> String
  show (Hz hz) = (show hz) <> "hz"

data LetterNote = C | D | E | F | G | A | B
data SolfegeNote = Do | Re | Mi | Fa | Sol | La | Si

instance showLetterNote :: Show LetterNote where
  show n = case n of
    A -> "A"
    B -> "B"
    C -> "C"
    D -> "D"
    E -> "E"
    F -> "F"
    G -> "G"

instance showSolfegeNote :: Show SolfegeNote where
  show n = case n of
    La -> "La"
    Si -> "Si"
    Do -> "Do"
    Re -> "Re"
    Mi -> "Mi"
    Fa -> "Fa"
    Sol -> "Sol"

-- number of half steps from C / Do
data NoteIndex = NoteIndex Int
derive instance eqNoteIndex :: Eq NoteIndex

instance showNoteIndex :: Show NoteIndex where
  show :: NoteIndex -> String
  show (NoteIndex n) = "♪" <> show n

class NoteName noteName where
  noteIndex :: noteName -> NoteIndex

instance letterNoteName :: NoteName LetterNote where
  noteIndex letter = case letter of
    C -> NoteIndex 0
    D -> NoteIndex 2
    E -> NoteIndex 4
    F -> NoteIndex 5
    G -> NoteIndex 7
    A -> NoteIndex 9
    B -> NoteIndex 11

instance solfegeNoteName :: NoteName SolfegeNote where
  noteIndex letter = case letter of
    Do  -> NoteIndex 0
    Re  -> NoteIndex 2
    Mi  -> NoteIndex 4
    Fa  -> NoteIndex 5
    Sol -> NoteIndex 7
    La  -> NoteIndex 9
    Si  -> NoteIndex 11

instance indexNoteName :: NoteName NoteIndex where
  noteIndex = id

data Interval = Interval Int

unison = Interval 0
perfectUnison = Interval 0
minorSecond = Interval 1
majorSecond = Interval 2
minorThird = Interval 3
majorThird = Interval 4
perfectFourth = Interval 5
perfectFifth = Interval 7
minorSixth = Interval 8
majorSixth = Interval 9
minorSeventh = Interval 10
majorSeventh = Interval 11
perfectOctave = Interval 12

toInterval :: Accidental -> Interval
toInterval accidental =
      case accidental of
      Natural -> Interval 0
      Sharp -> Interval 1
      Flat -> Interval (-1)

data Note n = Note n Accidental

instance showNote :: Show n => Show (Note n) where
  show (Note n acc) = (show n) <> (show acc)


instance noteNoteName :: NoteName n => NoteName (Note n) where
  noteIndex (Note n accidental) = NoteIndex $ nIndex + accInterval
    where
      (NoteIndex nIndex) = noteIndex n
      (Interval accInterval) = toInterval accidental

data OctaveNote n = OctaveNote Octave n


a440 :: Pitch
a440 = Hz 440.0

data MidiNote = MidiNote Int
derive instance eqMidiNote :: Eq MidiNote

instance showMidiNote :: Show MidiNote where
  show (MidiNote m) = "MIDI#" <> show m

midiNote :: forall n. (NoteName n) => OctaveNote n -> MidiNote
midiNote (OctaveNote octave note) = MidiNote $ (octIndex + 1) * 12 + nIndex
  where
    (NoteIndex nIndex) = noteIndex note
    (Octave octIndex) = octave

notePitch' :: forall n. (NoteName n) => Pitch -> OctaveNote n -> Pitch
notePitch' (Hz p) note = Hz $ p * (2.0 `pow` (((toNumber m) - 69.0) / 12.0))
  where
    (MidiNote m) = midiNote note


notePitch :: forall n. (NoteName n) => OctaveNote n -> Pitch
notePitch = notePitch' a440
