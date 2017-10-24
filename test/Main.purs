module Test.Main where

import Prelude
import Data.Newtype (unwrap)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.Unit.QuickCheck (quickCheck)
import Data.Monoid (mempty)
import Data.Set as Set
import Data.Maybe (Maybe(..))

import Music.MidiNote
import Music.Pitch
import Music.LetterNotation
import Music.Notes
import Music.Chords
import Music.Intervals
import Music.PitchClass
import Music.IntervalClass
import Data.Group (ginverse)
import Data.Cyclic

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

    test "note <-> midi note conversion" do
      Assert.equal (midiToNote $ MidiNote 60) (Note (Octave 4) (pitchClass' C Natural))
      Assert.equal (midiToNote $ MidiNote 61) (Note (Octave 4) (pitchClass' C Sharp))
      quickCheck \n -> let midi = (MidiNote n) in (toMidiNote <<< midiToNote) midi == midi

    test "transpose note" do
      Assert.equal (Note (Octave 4) (pitchClass' C Sharp)) (transposeNote (Note (Octave 4) (pitchClass' C Natural)) (Interval 1))
      quickCheck \n -> let note = (midiToNote (MidiNote n)) in notePitchClass (transposeNote note (Interval 12)) == notePitchClass note
      quickCheck \n -> let note = (midiToNote (MidiNote n)) in unwrap (noteOctave (transposeNote note (Interval 12))) == (unwrap (noteOctave note)) + 1

    test "diffNotes" do
      Assert.equal (Interval 1) (diffNotes (Note (Octave 1) (pitchClass' C Sharp)) (Note (Octave 1) (pitchClass' C Natural)))
      Assert.equal (Interval 2) (diffNotes (Note (Octave 1) (pitchClass' D Natural)) (Note (Octave 1) (pitchClass' C Natural)))
      Assert.equal (Interval 12) (diffNotes (Note (Octave 2) (pitchClass' C Natural)) (Note (Octave 1) (pitchClass' C Natural)))

    test "chord inversions" do
      Assert.equal
        ( (Set.singleton (Note (Octave 4) (pitchClass' G Sharp)))
        <> (Set.singleton (Note (Octave 5) (pitchClass' C Natural)))
        <> (Set.singleton (Note (Octave 5) (pitchClass' D Sharp)))
        <> mempty) (chordNotes RootPosition (makeTriad Major G Sharp (Octave 4)))

      Assert.equal
        ( (Set.singleton $ Note (Octave 4) (pitchClass' C Natural))
        <> (Set.singleton $ Note (Octave 4) (pitchClass' D Sharp))
        <> (Set.singleton $ Note (Octave 4) (pitchClass' G Sharp))
        <> mempty) (chordNotes (Inversion 1) (makeTriad Major G Sharp (Octave 4)))

      Assert.equal
        ( (Set.singleton (Note (Octave 4) (pitchClass' D Sharp)))
        <> (Set.singleton (Note (Octave 5) (pitchClass' C Natural)))
        <> (Set.singleton (Note (Octave 4) (pitchClass' G Sharp)))
        <> mempty) (chordNotes (Inversion 2) (makeTriad Major G Sharp (Octave 4)))

    test "parse chord inversion" do
      Assert.equal (Just RootPosition) (parseChordInversion "Root position")
      Assert.equal (Just $ Inversion 1) (parseChordInversion "Chord inversion 1")

    test "interval class ginverse" do
      quickCheck \n -> (intervalClass n) <> (ginverse $ intervalClass n) == mempty

    test "interval ginverse" do
      quickCheck \n -> (Interval n) <> (ginverse $ Interval n) == mempty

    test "rotateRight" do
      Assert.equal [3, 1, 2] (rotateRight 1 [1, 2, 3])
      Assert.equal [2, 3, 1] (rotateRight 2 [1, 2, 3])
      Assert.equal [1, 2, 3] (rotateRight 3 [1, 2, 3])
      Assert.equal [3, 1, 2] (rotateRight 4 [1, 2, 3])

    test "rotateLeft" do
      Assert.equal [2, 3, 1] (rotateLeft 1 [1, 2, 3])
      Assert.equal [3, 1, 2] (rotateLeft 2 [1, 2, 3])
      Assert.equal [1, 2, 3] (rotateLeft 3 [1, 2, 3])
      Assert.equal [2, 3, 1] (rotateLeft 4 [1, 2, 3])

    test "rotations" do
      quickCheck \n -> let a = [1, 2, 3] in (rotateRight n (rotateLeft n a)) == a

    test "cyclic" do
      Assert.equal (Cyclic [1, 2, 3]) (Cyclic [2, 3, 1])
      Assert.equal (Cyclic [1, 2, 3]) (Cyclic [3, 1, 2])

      quickCheck \n -> let a = [1, 2, 3] in Cyclic (rotateLeft n a) == Cyclic a

    -- test "diff interval classes" do
    --   Assert.equal (IntervalClass 3) (diffPitchClasses (PitchClass))

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
