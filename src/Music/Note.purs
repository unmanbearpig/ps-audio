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
  show (Note octs letter accidental) = (show letter) <> (show accidental) <> (show octs)

a440 :: Hz
a440 = Hz 440.0

noteToMidi :: Note -> MidiNote
noteToMidi (Note octs noteLetter accidental) = MidiNote $ unwrap $
                                               (toSemitones $ Octaves 1) <>
                                               (toSemitones octs) <>
                                               (toSemitones noteLetter) <>
                                               (toSemitones accidental)

newtype MidiNote = MidiNote Int

derive newtype instance eqMidiNote :: Eq MidiNote
derive instance newtypeMidiNote :: Newtype MidiNote _
instance midiNotePitch :: Pitch MidiNote where
  toHz :: MidiNote -> Hz
  toHz = midiNotePitch' a440

midiNotePitch' :: Hz -> MidiNote -> Hz
midiNotePitch' (Hz p) (MidiNote midiNote) = Hz $ p * (2.0 `pow` (((toNumber midiNote) - 69.0) / 12.0))

instance notePitch :: Pitch Note where
  toHz :: Note -> Hz
  toHz = toHz <<< noteToMidi

instance midiNoteShow :: Show MidiNote where
  show (MidiNote m) = "MIDI=" <> (show m)
