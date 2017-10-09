module Music.Chord where

-- import Prelude

-- import Music.Interval
-- import Music.Note

-- import Data.Set as Set

-- data Chord root = Chord root (Set.Set Interval)

-- chordNotes :: forall n. Chord n -> (Set.Set OctaveNote NoteIndex)
-- chordNotes (Chord root intervals) = (Set.singleton root) <> addedNotes
--   where addedNotes = Set.map (\i -> modifyNote root i) intervals
