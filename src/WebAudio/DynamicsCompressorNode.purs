module WebAudio.DynamicsCompressorNode where

import Audio.WebAudio.Types
import Control.Monad.Eff

import Audio.WebAudio.Utils (unsafeGetProp)

foreign import data DynamicsCompressorNode :: Type

instance audioNodeDynamicsCompressorNode :: AudioNode DynamicsCompressorNode

foreign import createDynamicsCompressor
  :: ∀ eff. AudioContext
  -> (Eff (wau :: WebAudio | eff) DynamicsCompressorNode)

threshold :: ∀ eff. DynamicsCompressorNode -> (Eff (wau :: WebAudio | eff) AudioParam)
threshold = unsafeGetProp "threshold"

reduction :: ∀ eff. DynamicsCompressorNode -> (Eff (wau :: WebAudio | eff) AudioParam)
reduction = unsafeGetProp "reduction"

knee :: ∀ eff. DynamicsCompressorNode -> (Eff (wau :: WebAudio | eff) AudioParam)
knee = unsafeGetProp "knee"

ratio :: ∀ eff. DynamicsCompressorNode -> (Eff (wau :: WebAudio | eff) AudioParam)
ratio = unsafeGetProp "ratio"

attack :: ∀ eff. DynamicsCompressorNode -> (Eff (wau :: WebAudio | eff) AudioParam)
attack = unsafeGetProp "attack"

release :: ∀ eff. DynamicsCompressorNode -> (Eff (wau :: WebAudio | eff) AudioParam)
release = unsafeGetProp "release"
