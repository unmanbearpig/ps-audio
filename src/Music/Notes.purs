module Music.Notes where

import Prelude
import Data.Newtype (unwrap)
import Music.LetterNotation
import Music.MidiNote
import Music.Pitch
import Music.SetTheory
import Music.Intervals

diffNotes :: Note -> Note -> Interval
diffNotes n1 n2 = diffMidiNotes (toMidiNote n1) (toMidiNote n2)

data NoteDescription = NoteDescription Octave PitchClassDescription

data Note = Note Octave PitchClass

derive instance eqNote :: Eq Note

instance toMidiNoteNote :: ToMidiNote Note where
  toMidiNote (Note oct pc) = transposeMidiNote (toMidiNote oct) (unwrap pc)

instance showNote :: Show Note where
  show (Note oct pc) = "(Octave " <> show oct <> " + " <> show pc <> " semitones)"

instance pitchNote :: Pitch Note where
  toHz = toHz <<< toMidiNote

instance ordNote :: Ord Note where
  compare note1 note2 = (toMidiNote note1) `compare` (toMidiNote note2)

makeNoteDesc :: Octave -> NoteLetter -> Accidental -> NoteDescription
makeNoteDesc oct noteLetter accidental = NoteDescription oct (PitchClassDescription noteLetter accidental)

instance toMidiNoteNoteDescription :: ToMidiNote NoteDescription where
  toMidiNote (NoteDescription oct pcd) = transposeMidiNote (toMidiNote oct) (unwrap $ pitchClass'' pcd)

instance pitchNoteDescription :: Pitch NoteDescription where
  toHz = toHz <<< toMidiNote

instance showNoteDescription :: Show NoteDescription where
  show (NoteDescription oct (PitchClassDescription pc accidental)) = (show pc) <> (show accidental) <> show oct

midiToNote :: MidiNote -> Note
midiToNote (MidiNote n) = Note (Octave octNumber) (pitchClass $ n `mod` 12)
  where octNumber = (n / 12) -1

notePitchClass :: Note -> PitchClass
notePitchClass (Note _ pc) = pc

transposeNote :: Note -> Interval -> Note
transposeNote note (Interval i) = midiToNote $ transposeMidiNote (toMidiNote note) i

noteOctave :: Note -> Octave
noteOctave (Note oct _) = oct
