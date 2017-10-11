module Music.Note where

import Prelude
import Data.Newtype (unwrap)

import Music.Pitch
import Music.Intervals
import Music.MidiNote

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

data Note = Note Octave NoteLetter Accidental

instance noteShow :: Show Note where
  show (Note octs letter accidental) = (show letter) <> (show accidental) <> (show octs)

noteToMidi :: Note -> MidiNote
noteToMidi (Note octs noteLetter accidental) = MidiNote $ unwrap $
                                               (toSemitones $ Octave 1) <>
                                               (toSemitones octs) <>
                                               (toSemitones noteLetter) <>
                                               (toSemitones accidental)

instance notePitch :: Pitch Note where
  toHz :: Note -> Hz
  toHz = toHz <<< noteToMidi
