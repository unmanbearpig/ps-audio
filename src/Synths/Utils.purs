module Synths.Utils ( module Music.Notes
                    , module Music.Pitch
                    , module Audio.WebAudio.Types
                    , class Player
                    , play
                    , stop
                    , class AuSource
                    , plugInto
                    , startOsc
                    , stopOsc
                    , noteOsc
                    , GainValue
                    , createGain
 ) where

import Prelude
import Control.Monad.Eff (Eff)
import Audio.WebAudio.AudioContext as AuCtx
import Audio.WebAudio.Oscillator as AuOsc
import Audio.WebAudio.AudioParam as AuParam
import Audio.WebAudio.Types (WebAudio, AudioContext, OscillatorNode, GainNode, class AudioNode)
import Audio.WebAudio.GainNode (gain)
import Music.Notes
import Music.Pitch

type GainValue = Number

class Player a where
  play :: ∀ eff. a -> Number -> (Eff ( wau :: WebAudio | eff ) Unit)
  stop :: ∀ eff. a -> Number -> (Eff ( wau :: WebAudio | eff ) Unit)

class Gain a where
  setGain :: ∀ eff. a -> Number -> (Eff ( wau :: WebAudio | eff ) Unit)
  getGain :: ∀ eff. a -> (Eff ( wau :: WebAudio | eff ) Number)

class AuSource a where
  plugInto :: ∀ eff b. AudioNode b => a -> b -> (Eff ( wau :: WebAudio | eff) Unit)

createGain :: ∀ eff. AudioContext -> GainValue -> (Eff ( wau :: WebAudio | eff) GainNode)
createGain ctx gValue = do
  g <- AuCtx.createGain ctx
  gain g >>= AuParam.setValue gValue
  pure g

noteOsc :: ∀ eff. AudioContext -> Note → (Eff ( wau ∷ WebAudio | eff ) OscillatorNode)
noteOsc ctx note = do
  osc <- AuCtx.createOscillator ctx
  freqParam <- AuOsc.frequency osc
  AuParam.setValue (pitchFreq $ toHz note) freqParam
  AuOsc.setOscillatorType AuOsc.Sine osc
  pure osc

startOsc :: ∀ eff. OscillatorNode -> Number -> (Eff ( wau :: WebAudio | eff) Unit)
startOsc osc startTime = AuOsc.startOscillator startTime osc

stopOsc :: ∀ eff. OscillatorNode -> Number -> (Eff ( wau :: WebAudio | eff) Unit)
stopOsc osc stopTime = AuOsc.stopOscillator stopTime osc
