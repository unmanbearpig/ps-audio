module Component where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Audio.WebAudio.Types (WebAudio, AudioContext, OscillatorNode)
import Audio.WebAudio.AudioContext as AuCtx
import Audio.WebAudio.AudioParam as AuParam
import Audio.WebAudio.Oscillator as AuOsc
import Audio.WebAudio.GainNode (gain)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console

data Query a = ToggleState a

type State = { on :: Boolean
             , ctx :: Maybe AudioContext
             , osc :: Maybe OscillatorNode }

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff ( wau :: WebAudio, console :: CONSOLE | eff ))
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { on: false
                 , ctx: Nothing
                 , osc: Nothing }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Hello world!" ]
      , HH.p_
          [ HH.text "Why not toggle this button:" ]
      , HH.button
          [ HE.onClick (HE.input_ ToggleState) ]
          [ HH.text
              if not state.on
              then "Play"
              else "Stop"
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff ( wau :: WebAudio, console :: CONSOLE | eff ))
  eval = case _ of
    ToggleState next -> do
      H.liftEff $ Console.log "starting doing something"

      newCtx <- H.liftEff AuCtx.makeAudioContext
      newOsc <- H.liftEff $ AuCtx.createOscillator newCtx

      g <- H.liftEff $ AuCtx.createGain newCtx
      H.liftEff $ AuParam.setValue 1.0 =<< gain g

      H.liftEff $ AuCtx.connect newOsc g
      H.liftEff $ AuCtx.connect g =<< AuCtx.destination newCtx

      H.liftEff $ Console.log "starting playing"
      H.liftEff $ AuOsc.startOscillator 0.0 newOsc
      H.liftEff $ Console.log "started playing"

      H.modify (\state -> { on: not state.on
                          , ctx: Just newCtx
                          , osc: Just newOsc })
      pure next
