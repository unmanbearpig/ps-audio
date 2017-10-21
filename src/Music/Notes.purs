module Music.Notes where

import Data.Newtype (class Newtype, unwrap)
import Music.LetterNotation
import Music.MidiNote
import Music.Pitch
import Music.SetTheory
import Prelude

newtype Interval = Interval Int

newtype Octave = Octave Int

derive instance newtypeOctave :: Newtype Octave _
derive newtype instance eqOctave :: Eq Octave
instance showOctave :: Show Octave where
  show (Octave n) = show n

instance toMidiNoteOctave :: ToMidiNote Octave where
  toMidiNote (Octave oct) = MidiNote $ (oct + 1) * 12


data NoteDescription = NoteDescription Octave PitchClassDescription

data Note = Note Octave PitchClass

instance toMidiNoteNote :: ToMidiNote Note where
  toMidiNote (Note oct pc) = transposeMidiNote (toMidiNote oct) (unwrap pc)

instance showNote :: Show Note where
  show (Note oct pc) = "(Octave " <> show oct <> " + " <> show pc <> " semitones)"

instance pitchNote :: Pitch Note where
  toHz = toHz <<< toMidiNote

makeNoteDesc :: Octave -> NoteLetter -> Accidental -> NoteDescription
makeNoteDesc oct noteLetter accidental = NoteDescription oct (PitchClassDescription noteLetter accidental)

instance toMidiNoteNoteDescription :: ToMidiNote NoteDescription where
  toMidiNote (NoteDescription oct pcd) = transposeMidiNote (toMidiNote oct) (unwrap $ pitchClass'' pcd)

instance pitchNoteDescription :: Pitch NoteDescription where
  toHz = toHz <<< toMidiNote

instance showNoteDescription :: Show NoteDescription where
  show (NoteDescription oct (PitchClassDescription pc accidental)) = (show pc) <> (show accidental) <> show oct

notePitchClass :: Note -> PitchClass
notePitchClass (Note _ pc) = pc
