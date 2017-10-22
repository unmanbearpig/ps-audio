module Synths.Poly where

import Audio.WebAudio.AudioParam
import Prelude
import Synths.Utils

import Audio.WebAudio.AudioContext as AuCtx
import Audio.WebAudio.GainNode
import Audio.WebAudio.AudioParam as AuParam
import Control.Monad.Eff (Eff)
import Data.Traversable (traverse, traverse_)
import Data.Newtype (class Newtype)
import Data.List


newtype PolySynth = PolySynth { notes :: (List Note)
                              , oscs :: (List OscillatorNode)
                              , gainNode :: GainNode }

derive instance newtypePolySynth :: Newtype PolySynth _

polySynth :: ∀ eff. AudioContext -> (List Note) -> GainValue -> (Eff ( wau :: WebAudio | eff ) PolySynth)
polySynth ctx notes gainValue = do
  oscs <- traverse (noteOsc ctx) notes
  g <- createGain ctx gainValue
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
