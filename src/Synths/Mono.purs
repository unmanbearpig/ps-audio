module Synths.Mono where

import Prelude
import Synths.Utils
import Audio.WebAudio.AudioContext as AuCtx
import Control.Monad.Eff (Eff)

newtype MonoSynth = MonoSynth { note :: Note
                              , osc :: OscillatorNode
                              , gainNode :: GainNode }

instance monoSynthPlayer :: Player MonoSynth where
  play (MonoSynth ms)= startOsc ms.osc
  stop (MonoSynth ms) = stopOsc ms.osc

instance auSounceMonoSynth :: AuSource MonoSynth where
  plugInto (MonoSynth ms) dest = AuCtx.connect ms.gainNode dest

monoSynth :: ∀ eff. AudioContext -> Note -> GainValue -> (Eff ( wau :: WebAudio | eff) MonoSynth)
monoSynth ctx note gainValue = do
  osc <- noteOsc ctx note
  g <- createGain ctx gainValue
  AuCtx.connect osc g
  pure $ MonoSynth { note: note
                   , osc: osc
                   , gainNode: g }
