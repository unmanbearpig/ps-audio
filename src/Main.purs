module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Control.Monad.Eff.Console (CONSOLE)
import Audio.WebAudio.Types (WebAudio)

import Component (component)

main :: forall eff. Eff (HA.HalogenEffects ( wau :: WebAudio, console :: CONSOLE | eff )) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
