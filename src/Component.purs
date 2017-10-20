module Component where

import Control.Monad
import Data.Int
import Data.List.Lazy
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Music.Chords
import Music.LetterNotation
import Music.Notes
import Music.Pitch
import Music.SetTheory
import Prelude
import Synths
import WebAudio.DynamicsCompressorNode
import Data.Newtype
import Audio.WebAudio.AudioContext as AuCtx
import Audio.WebAudio.AudioParam as AuParam
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.Oscillator as AuOsc
import Audio.WebAudio.Types (WebAudio, AudioContext, OscillatorNode, DestinationNode)
import Audio.WebAudio.Utils
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Monoid
import Synths.Sequence

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
          chords :: List Chord
          chords = (\blah -> blah (Octave 4)) <$>
                      ((makeTriad Major C Natural)
                       : (makeTriad Augmented C Natural)
                       : (makeTriad Minor A Natural)
                       : (makeTriad Diminished G Natural)
                       : mempty)
          chords2 :: List Chord
          chords2 = (\blah -> blah (Octave 4)) <$>
                     ((makeTriad Minor C Sharp)
                       : (makeTriad Minor A Natural)
                       : (makeTriad Minor B Natural)
                       : (makeTriad Major G Natural)
                       : mempty)


      dest <- H.liftEff $ AuCtx.destination ctx

      compressor <- H.liftEff $ createDynamicsCompressor ctx

      H.liftEff $ AuCtx.connect compressor dest

      H.liftEff $ AuParam.setValue 1.0 =<< threshold compressor
      H.liftEff $ AuParam.setValue 30.0 =<< ratio compressor

      H.liftEff $ playSequence ctx compressor 0.20 (take 100 $ cycle chords)
      H.liftEff $ playSequence ctx compressor 0.25 (take 100 $ cycle chords2)


      H.modify (\state -> { on: not state.on
                          , ctx: Just ctx
                          , synth: Nothing })
      pure next
