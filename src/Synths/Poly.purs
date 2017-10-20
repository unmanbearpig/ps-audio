module Synths.Poly where

import Prelude
import Synths.Utils
import Audio.WebAudio.AudioContext as AuCtx
import Control.Monad.Eff (Eff)
import Data.Traversable (traverse, traverse_)

newtype PolySynth = PolySynth { notes :: (Array Note)
                              , oscs :: (Array OscillatorNode)
                              , gainNode :: GainNode }

polySynth :: ∀ eff. AudioContext -> (Array Note) -> GainValue -> (Eff ( wau :: WebAudio | eff ) PolySynth)
polySynth ctx notes gainValue = do
  oscs <- traverse (noteOsc ctx) notes
  g <- createGain ctx gainValue
  traverse_ (\osc -> AuCtx.connect osc g) oscs
  pure $ PolySynth { notes: notes
                   , oscs: oscs
                   , gainNode: g }


polySynth' :: ∀ eff. AudioContext -> (Array Note) -> GainNode -> (Eff ( wau :: WebAudio | eff ) PolySynth)
polySynth' ctx notes g = do
  oscs <- traverse (noteOsc ctx) notes
  traverse_ (\osc -> AuCtx.connect osc g) oscs
  pure $ PolySynth { notes: notes
                   , oscs: oscs
                   , gainNode: g }



instance auSouncePolySynth :: AuSource PolySynth where
  plugInto (PolySynth ps) dest =
    AuCtx.connect ps.gainNode dest

instance playerPolySynth :: Player PolySynth where
  play (PolySynth ps) time = traverse_ (\osc -> startOsc osc time) ps.oscs
  stop (PolySynth ps) time = traverse_ (\osc -> stopOsc osc time) ps.oscs
