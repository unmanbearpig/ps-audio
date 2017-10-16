module Music.MidiNote where

import Prelude

import Data.Int (toNumber)
import Data.Newtype (class Newtype)
import Math (pow)
import Music.Pitch (class Pitch, Hz(..), a440)

newtype MidiNote = MidiNote Int

derive newtype instance eqMidiNote :: Eq MidiNote
derive instance newtypeMidiNote :: Newtype MidiNote _
instance midiNotePitch :: Pitch MidiNote where
  toHz :: MidiNote -> Hz
  toHz = midiNotePitch' a440

midiNotePitch' :: Hz -> MidiNote -> Hz
midiNotePitch' (Hz p) (MidiNote midiNote) = Hz $ p * (2.0 `pow` (((toNumber midiNote) - 69.0) / 12.0))

instance midiNoteShow :: Show MidiNote where
  show (MidiNote m) = "MIDI=" <> (show m)

class ToMidiNote a where
  toMidiNote :: a -> MidiNote

instance midiNoteToMidiNote :: ToMidiNote MidiNote where
  toMidiNote = id

transposeMidiNote :: MidiNote -> Int -> MidiNote
transposeMidiNote (MidiNote mn) semitones = MidiNote $ mn + semitones

toPitch :: ∀ a. ToMidiNote a => a -> Hz
toPitch = midiNotePitch' a440 <<< toMidiNote
