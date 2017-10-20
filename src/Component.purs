module Component where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Data.Maybe -- (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Audio.WebAudio.Types (WebAudio, AudioContext, OscillatorNode, DestinationNode)
import Audio.WebAudio.AudioContext as AuCtx
import Audio.WebAudio.AudioParam as AuParam
import Audio.WebAudio.Oscillator as AuOsc
import Audio.WebAudio.GainNode (gain)
import Control.Monad
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Data.Traversable
import Data.List.Lazy
import Data.Int
import Data.Tuple


-- import Music.Intervals
import Music.Pitch
import Music.Notes
import Music.SetTheory
import Music.LetterNotation
import Music.Chords
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

  playSequence :: forall eff. AudioContext -> DestinationNode -> Number -> (List Chord) -> (Eff ( wau :: WebAudio | eff ) Unit)
  playSequence ctx dest duration chords = do
      g <- createGain ctx 1.0
      traverse_ (\(Tuple chord idx) -> scheduleChordSynth chord idx g) $ zip chords (iterate (_ + 1) 0)
    where
      scheduleChordSynth :: Chord -> Int -> GainNode -> (Eff ( wau :: WebAudio | eff ) Unit)
      scheduleChordSynth chord idx g = do
        synth <- polySynth' ctx (chordNotes chord) g
        synth `plugInto` dest
        play synth startTime
        stop synth (startTime + duration)
          where startTime = (toNumber idx) * duration

  eval :: Query ~> H.ComponentDSL State Query Void (Aff ( wau :: WebAudio, console :: CONSOLE | eff ))
  eval = case _ of
    ToggleState next -> do
      H.liftEff $ Console.log "making audio context"
      ctx <- H.liftEff <<< init AuCtx.makeAudioContext =<< H.gets (_.ctx)

      let
          chords :: List Chord
          chords = (\blah -> blah (Octave 4)) <$>
                      ((makeTriad Major C Natural)
                       : (makeTriad Minor D Natural)
                       : (makeTriad Minor E Natural)
                       : (singleton $ makeTriad Major F Natural))
          chords2 :: List Chord
          chords2 = (\blah -> blah (Octave 5)) <$>
                     ((makeTriad Major C Natural)
                       : (makeTriad Minor D Natural)
                       : (makeTriad Minor E Natural)
                       : (singleton $ makeTriad Major G Natural))


      dest <- H.liftEff $ AuCtx.destination ctx
      H.liftEff $ playSequence ctx dest 0.33 (take 30 $ cycle chords)
      H.liftEff $ playSequence ctx dest 0.5 (take 20 $ cycle chords2)


      H.modify (\state -> { on: not state.on
                          , ctx: Just ctx
                          , synth: Nothing })
      pure next
