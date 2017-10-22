module Synths.Sequence where

import Prelude
import Data.List.Lazy (List, iterate, zip)
import Data.Int (toNumber)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.AudioParam as AuParam
import Audio.WebAudio.AudioContext as AuCtx
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Newtype (unwrap)
import Music.Chords (Chord, chordNotes, ChordInversion(..))
import Synths.Poly (polySynth)
import Synths.Utils

import Data.Set as Set

playSequence :: forall eff dest. AudioNode dest => AudioContext -> dest -> Number -> (List Chord) -> (Eff ( wau :: WebAudio, console :: CONSOLE | eff ) Unit)
playSequence ctx dest duration chords = do
     g <- createGain ctx 1.0
     AuCtx.connect g dest
     traverse_ (\(Tuple chord idx) -> scheduleChordSynth chord idx g) $ zip chords (iterate (_ + 1) 0)
  where
    scheduleChordSynth :: Chord -> Int -> GainNode -> (Eff ( wau :: WebAudio, console :: CONSOLE | eff ) Unit)
    scheduleChordSynth chord idx g = do
      synth <- polySynth ctx (Set.toUnfoldable $ chordNotes RootPosition chord) 0.0
      synthGain <- gain ((unwrap synth).gainNode)
      synth `plugInto` g

      t <- AuCtx.currentTime ctx

      let startTime = t + (toNumber idx) * duration
          endTime = (startTime + duration)
          fadeDuration = (duration * 0.01)

      play synth startTime
      _ <- AuParam.linearRampToValueAtTime 0.0 (startTime) synthGain
      _ <- AuParam.linearRampToValueAtTime 1.0 (startTime + fadeDuration) synthGain
      _ <- AuParam.linearRampToValueAtTime 1.0 (endTime) synthGain
      _ <- AuParam.linearRampToValueAtTime 0.0 (endTime + duration) synthGain
      stop synth (endTime + duration)
