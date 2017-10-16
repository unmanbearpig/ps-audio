module Music.Notes where

import Data.Newtype
import Data.Tuple
import Music.LetterNotation
import Music.MidiNote
import Music.Pitch
import Music.SetTheory
import Prelude

newtype Octave = Octave Int

derive instance newtypeOctave :: Newtype Octave _
derive newtype instance eqOctave :: Eq Octave
instance showOctave :: Show Octave where
  show (Octave n) = show n

instance toMidiNoteOctave :: ToMidiNote Octave where
  toMidiNote (Octave oct) = MidiNote $ (oct + 1) * 12


data Note = Note Octave PitchClassDescription

makeNote :: Octave -> NoteLetter -> Accidental -> Note
makeNote oct noteLetter accidental = Note oct (Tuple noteLetter accidental)

instance toMidiNoteNote :: ToMidiNote Note where
  toMidiNote (Note oct pcd) = transposeMidiNote (toMidiNote oct) (unwrap $ pitchClass'' pcd)

instance pitchNote :: Pitch Note where
  toHz = toHz <<< toMidiNote

instance showNote :: Show Note where
  show (Note oct (Tuple pc accidental)) = (show pc) <> (show accidental) <> show oct
