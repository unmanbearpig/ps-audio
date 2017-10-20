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

  playSequence :: forall eff dest. AudioNode dest => AudioContext -> dest -> Number -> (List Chord) -> (Eff ( wau :: WebAudio, console :: CONSOLE | eff ) Unit)
  playSequence ctx dest duration chords = do
      g <- createGain ctx 1.0
      AuCtx.connect g dest
      traverse_ (\(Tuple chord idx) -> scheduleChordSynth chord idx g) $ zip chords (iterate (_ + 1) 0)
    where
      scheduleChordSynth :: Chord -> Int -> GainNode -> (Eff ( wau :: WebAudio, console :: CONSOLE | eff ) Unit)
      scheduleChordSynth chord idx g = do
        synth <- polySynth ctx (chordNotes chord) 0.0
        synthGain <- gain ((unwrap synth).gainNode)
        synth `plugInto` g

        play synth startTime
        _ <- AuParam.linearRampToValueAtTime 0.0 (startTime) synthGain
        _ <- AuParam.linearRampToValueAtTime 1.0 (startTime + fadeDuration) synthGain
        _ <- AuParam.linearRampToValueAtTime 1.0 (endTime) synthGain
        _ <- AuParam.linearRampToValueAtTime 0.0 (endTime + duration) synthGain
        stop synth (endTime + duration)
          where startTime = (toNumber idx) * duration
                endTime = (startTime + duration)
                fadeDuration = (duration * 0.025)
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
          chords2 = (\blah -> blah (Octave 4)) <$>
                     ((makeTriad Major C Natural)
                       : (makeTriad Minor A Natural)
                       : (makeTriad Minor B Natural)
                       : (singleton $ makeTriad Major G Natural))


      dest <- H.liftEff $ AuCtx.destination ctx

      compressor <- H.liftEff $ createDynamicsCompressor ctx

      H.liftEff $ AuCtx.connect compressor dest

      H.liftEff $ playSequence ctx compressor 0.333333 (take 30 $ cycle chords)
      H.liftEff $ playSequence ctx compressor 0.25 (take 20 $ cycle chords2)


      H.modify (\state -> { on: not state.on
                          , ctx: Just ctx
                          , synth: Nothing })
      pure next
