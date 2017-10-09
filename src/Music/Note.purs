module Music.Note where

import Prelude
import Data.Newtype (class Newtype, unwrap)

import Music.Pitch
import Music.Intervals

import Math (pow)
import Data.Int (toNumber)

data NoteLetter = C | D | E | F | G | A | B
instance noteLetterShow :: Show NoteLetter where
  show = case _ of
    A -> "A"
    B -> "B"
    C -> "C"
    D -> "D"
    E -> "E"
    F -> "F"
    G -> "G"

derive instance eqNoteLetter :: Eq NoteLetter

instance noteLetterInterval :: Interval NoteLetter where
  toSemitones = case _ of
    C -> Semitones 0
    D -> Semitones 2
    E -> Semitones 4
    F -> Semitones 5
    G -> Semitones 7
    A -> Semitones 9
    B -> Semitones 11

data Note = Note Octaves NoteLetter Accidental

instance noteShow :: Show Note where
  show (Note octs letter accidental) = (show letter) <> (show octs) <> (show octs)

a440 :: Hz
a440 = Hz 440.0

noteToMidi :: Note -> MidiNote
noteToMidi (Note octs noteLetter accidental) = MidiNote $ unwrap $
                                               (toSemitones octs) <>
                                               (toSemitones noteLetter) <>
                                               (toSemitones accidental)

newtype MidiNote = MidiNote Int

derive instance newtypeMidiNote :: Newtype MidiNote _
instance midiNotePitch :: Pitch MidiNote where
  toHz :: MidiNote -> Hz
  toHz = midiNotePitch' a440

midiNotePitch' :: Hz -> MidiNote -> Hz
midiNotePitch' (Hz p) (MidiNote midiNote) = Hz $ p * (2.0 `pow` (((toNumber midiNote) - 69.0) / 12.0))


instance notePitch :: Pitch Note where
  toHz :: Note -> Hz
  toHz = toHz <<< noteToMidi
















-- import Music.Interval
-- import Math (pow)
-- import Data.Int (toNumber)

-- data Pitch = Hz Number
-- derive instance eqPitch :: Eq Pitch

-- instance showPitch :: Show Pitch where
--   show :: Pitch -> String
--   show (Hz hz) = (show hz) <> "hz"

-- data LetterNote = C | D | E | F | G | A | B

-- instance showLetterNote :: Show LetterNote where
--   show n = case n of
--     A -> "A"
--     B -> "B"
--     C -> "C"
--     D -> "D"
--     E -> "E"
--     F -> "F"
--     G -> "G"


-- data SolfegeNote = Do | Re | Mi | Fa | Sol | La | Si

-- instance showSolfegeNote :: Show SolfegeNote where
--   show n = case n of
--     La -> "La"
--     Si -> "Si"
--     Do -> "Do"
--     Re -> "Re"
--     Mi -> "Mi"
--     Fa -> "Fa"
--     Sol -> "Sol"



-- data Accidental = Natural | Sharp | Flat
-- -- theoretically we could have double sharps or double flats as well
-- -- also I don't think that natural is exactly the same as no accidental
-- instance showAccidental :: Show Accidental where
--   show acc = case acc of
--     Natural -> "♮"
--     Flat -> "♭"
--     Sharp -> "♯"


-- data MidiNote = MidiNote Int


-- -- instance showOctave :: Show Octave where
-- --   show (Octave oct) = "O" <> show oct

-- -- data Accidental = Natural | Sharp | Flat

-- -- -- theoretically we could have double sharps or double flats as well
-- -- -- also I don't think that natural is exactly the same as no accidental
-- -- instance showAccidental :: Show Accidental where
-- --   show acc = case acc of
-- --     Natural -> "♮"
-- --     Flat -> "♭"
-- --     Sharp -> "♯"

-- -- data Pitch = Hz Number
-- -- derive instance eqPitch :: Eq Pitch

-- -- instance showPitch :: Show Pitch where
-- --   show :: Pitch -> String
-- --   show (Hz hz) = (show hz) <> "hz"

-- -- data LetterNote = C | D | E | F | G | A | B
-- -- data SolfegeNote = Do | Re | Mi | Fa | Sol | La | Si

-- -- instance showLetterNote :: Show LetterNote where
-- --   show n = case n of
-- --     A -> "A"
-- --     B -> "B"
-- --     C -> "C"
-- --     D -> "D"
-- --     E -> "E"
-- --     F -> "F"
-- --     G -> "G"

-- -- instance showSolfegeNote :: Show SolfegeNote where
-- --   show n = case n of
-- --     La -> "La"
-- --     Si -> "Si"
-- --     Do -> "Do"
-- --     Re -> "Re"
-- --     Mi -> "Mi"
-- --     Fa -> "Fa"
-- --     Sol -> "Sol"

-- -- -- number of half steps from C / Do
-- -- data NoteIndex = NoteIndex Int
-- -- derive instance eqNoteIndex :: Eq NoteIndex

-- -- instance showNoteIndex :: Show NoteIndex where
-- --   show :: NoteIndex -> String
-- --   show (NoteIndex n) = "♪" <> show n

-- -- noteIndexToInterval :: NoteIndex -> Interval
-- -- noteIndexToInterval (NoteIndex n) = Interval n

-- -- intervalToNoteIndex :: Interval -> NoteIndex
-- -- intervalToNoteIndex (Interval i) = NoteIndex i

-- -- -- should noteIndex be an interval instead?
-- -- class NoteName noteName where
-- --   noteIndex :: noteName -> NoteIndex
-- --   toInterval

-- -- instance letterNoteName :: NoteName LetterNote where
-- --   noteIndex letter = case letter of
-- --     C -> NoteIndex 0
-- --     D -> NoteIndex 2
-- --     E -> NoteIndex 4
-- --     F -> NoteIndex 5
-- --     G -> NoteIndex 7
-- --     A -> NoteIndex 9
-- --     B -> NoteIndex 11

-- -- instance solfegeNoteName :: NoteName SolfegeNote where
-- --   noteIndex letter = case letter of
-- --     Do  -> NoteIndex 0
-- --     Re  -> NoteIndex 2
-- --     Mi  -> NoteIndex 4
-- --     Fa  -> NoteIndex 5
-- --     Sol -> NoteIndex 7
-- --     La  -> NoteIndex 9
-- --     Si  -> NoteIndex 11

-- -- instance indexNoteName :: NoteName NoteIndex where
-- --   noteIndex = id

-- -- toInterval :: Accidental -> Interval
-- -- toInterval accidental =
-- --       case accidental of
-- --       Natural -> Interval 0
-- --       Sharp -> Interval 1
-- --       Flat -> Interval (-1)

-- -- data Note n = Note n Accidental

-- -- instance showNote :: Show n => Show (Note n) where
-- --   show (Note n acc) = (show n) <> (show acc)


-- -- instance noteNoteName :: NoteName n => NoteName (Note n) where
-- --   noteIndex (Note n accidental) = NoteIndex $ nIndex + accInterval
-- --     where
-- --       (NoteIndex nIndex) = noteIndex n
-- --       (Interval accInterval) = toInterval accidental

-- -- data OctaveNote n = OctaveNote Octave n

-- -- data MidiNote = MidiNote Int
-- -- derive instance eqMidiNote :: Eq MidiNote

-- -- instance showMidiNote :: Show MidiNote where
-- --   show (MidiNote m) = "MIDI#" <> show m

-- -- class (NoteName n) <= AbsoluteNote n where
-- --   noteOctave :: n -> Octave
-- --   octaveIndexNote :: n -> OctaveNote NoteIndex

-- -- instance midiNoteNoteName :: NoteName MidiNote where
-- --   noteIndex :: MidiNote -> NoteIndex
-- --   noteIndex n = NoteIndex 3

-- -- instance midiNoteAbsoluteNote :: AbsoluteNote MidiNote where
-- --   noteOctave :: MidiNote -> Octave
-- --   noteOctave n = Octave 1

-- --   octaveIndexNote :: MidiNote -> OctaveNote NoteIndex
-- --   octaveIndexNote n = OctaveNote (noteOctave n) (noteIndex n)


-- -- midiNote :: forall n. AbsoluteNote n => n -> MidiNote
-- -- midiNote note = MidiNote $ (octIndex + 1) * 12 + nIndex
-- --   where
-- --     (NoteIndex nIndex) = noteIndex note
-- --     (Octave octIndex) = noteOctave note


-- -- instance octaveNoteNoteName :: NoteName n => NoteName (OctaveNote n) where
-- --   noteIndex (OctaveNote _ note) = noteIndex note

-- -- instance octaveNoteAbsoluteNote :: NoteName n => AbsoluteNote (OctaveNote n) where
-- --   noteOctave (OctaveNote oct _) = oct

-- --   octaveIndexNote :: OctaveNote n -> OctaveNote NoteIndex
-- --   octaveIndexNote n = OctaveNote (noteOctave n) (noteIndex n)

-- -- -- instance eqAbsoluteNote :: (Eq n, AbsoluteNote n) => Eq n where
-- -- --   eq a b = (octaveIndexNote a) == octaveIndexNote b

-- -- instance showOctaveNote :: (Show n) => Show (OctaveNote n) where
-- --   show (OctaveNote oct note) = (show note) <> show note

-- -- notePitch' :: forall n. (NoteName n) => Pitch -> OctaveNote n -> Pitch
-- -- notePitch' (Hz p) note = Hz $ p * (2.0 `pow` (((toNumber m) - 69.0) / 12.0))
-- --   where
-- --     (MidiNote m) = midiNote note


-- -- notePitch :: forall n. (NoteName n) => OctaveNote n -> Pitch
-- -- notePitch = notePitch' a440

-- -- modifyNote :: forall n. NoteName n => n -> Interval -> NoteIndex
-- -- modifyNote n i = intervalToNoteIndex $ (noteIndexToInterval $ noteIndex n) <> i
