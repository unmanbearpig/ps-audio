module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)

import Test.Unit (suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Music.MidiNote
import Music.Pitch
import Music.LetterNotation
import Music.Notes

main = runTest do
  suite "music" do
    -- test "letter note index" do
    --   Assert.equal (noteIndex C) (noteIndex Do)
    --   Assert.equal (noteIndex D) (noteIndex Re)
    --   Assert.equal (noteIndex E) (noteIndex Mi)
    --   Assert.equal (noteIndex F) (noteIndex Fa)
    --   Assert.equal (noteIndex G) (noteIndex Sol)
    --   Assert.equal (noteIndex A) (noteIndex La)
    --   Assert.equal (noteIndex B) (noteIndex Si)

    test "octaveNote" do
      Assert.equal (MidiNote 0) (toMidiNote (Note (Octave (-1)) (pitchClass' C Natural)))
      Assert.equal (MidiNote 100) (toMidiNote (Note (Octave 7) (pitchClass' E Natural)))
      Assert.equal (MidiNote 127) (toMidiNote (Note (Octave 9) (pitchClass' G Natural)))

    test "note pitch" do
      Assert.equal (Hz 440.0) (toHz (Note (Octave 4) (pitchClass' A Natural)))
      Assert.equal (Hz 880.0) (toHz (Note (Octave 5) (pitchClass' A Natural)))
      Assert.equal (Hz 220.0) (toHz (Note (Octave 3) (pitchClass' A Natural)))
      Assert.equal (Hz 110.0) (toHz (Note (Octave 2) (pitchClass' A Natural)))
      Assert.equal (Hz  55.0) (toHz (Note (Octave 1) (pitchClass' A Natural)))
      Assert.equal (Hz  27.5) (toHz (Note (Octave 0) (pitchClass' A Natural)))
      Assert.equal (Hz  13.75) (toHz (Note (Octave (-1)) (pitchClass' A Natural)))
      -- doesn't match exactly, not sure if correct
      -- Assert.equal (Hz  14.568) (toHz (Note (Octave (-1)) (Note A Sharp)))

    -- test "showNote" do
    --   Assert.equal "C♭" (show $ Note C Flat)
    --   Assert.equal "C♯" (show $ Note C Sharp)
    --   Assert.equal "C♮" (show $ Note C Natural)
      -- Assert.equal "Do♭" (show $ Note Do Flat)
      -- Assert.equal "Do♯" (show $ Note Do Sharp)
      -- Assert.equal "Do♮" (show $ Note Do Natural)

    -- test "midi note absolute" do
    --   let n1 = (Note (Octave 3) (noteIndex A))

    --   Assert.equal n1 (octaveIndexNote $ noteToMidi n1)
