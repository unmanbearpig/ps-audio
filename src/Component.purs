module Component where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Data.Maybe -- (Maybe(..))

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
import Data.Traversable

-- import Music.Intervals
import Music.Pitch
import Music.Notes
import Music.SetTheory
import Music.LetterNotation
import Synths

data Query a = ToggleState a

type State = { on :: Boolean
             , ctx :: Maybe AudioContext
             , synth :: Maybe PolySynth }

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
                 , synth: Nothing }

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

  initCtx :: ∀ eff. Maybe AudioContext -> (Eff ( wau :: WebAudio | eff) AudioContext)
  initCtx (Just ctx) = pure ctx
  initCtx (Nothing) = AuCtx.makeAudioContext

  init :: ∀ eff a. (Eff (wau :: WebAudio | eff) a) -> Maybe a -> (Eff ( wau :: WebAudio | eff) a)
  init a Nothing = a
  init _ (Just a) = pure a

  eval :: Query ~> H.ComponentDSL State Query Void (Aff ( wau :: WebAudio, console :: CONSOLE | eff ))
  eval = case _ of
    ToggleState next -> do
      H.liftEff $ Console.log "making audio context"
      ctx <- H.liftEff <<< init AuCtx.makeAudioContext =<< H.gets (_.ctx)

      let
          notes :: Array Note
          notes = [ (makeNote (Octave 3) A Natural)
                  , (makeNote (Octave 4) C Natural)
                  , (makeNote (Octave 5) G Flat) ]

      H.liftEff $ traverse_ (Console.log <<< show) notes

      synth <- H.liftEff <<< init (polySynth ctx notes 1.0) =<< H.gets (_.synth)
      dest <- H.liftEff $ AuCtx.destination ctx
      H.liftEff $ synth `plugInto` dest

      H.liftEff <<< (if _ then stop synth else play synth) =<< H.gets (_.on)

      H.modify (\state -> { on: not state.on
                          , ctx: Just ctx
                          , synth: Just synth })
      pure next
