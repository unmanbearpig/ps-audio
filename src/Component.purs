module Component where

import Data.List.Lazy
import Data.Monoid
import Data.Tuple
import Music.Chords
import Music.LetterNotation
import Music.Notes
import Music.SetTheory
import Prelude
import Synths
import Synths.Sequence
import WebAudio.DynamicsCompressorNode

import Audio.WebAudio.AudioContext as AuCtx
import Audio.WebAudio.AudioParam as AuParam
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a = Play a | ChangeChordQuality String a | ChangePitchClass String a

type State = { ctx :: Maybe AudioContext
             , dest :: Maybe DynamicsCompressorNode
             , chordQuality :: ChordQuality
             , chordPitchClass :: PitchClassDescription
             , octave :: Octave}

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
  initialState = { ctx: Nothing
                 , dest: Nothing
                 , chordQuality: Major
                 , chordPitchClass: (Tuple C Natural)
                 , octave: (Octave 4) }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Hello world!" ]
      , HH.p_
          [ HH.text "Why not toggle this button:" ]
      , HH.select [ ]
        (map (\noteName -> HH.option [ ] [ HH.text noteName ]) pitchClassNames)
      , HH.select [ HE.onValueChange (HE.input ChangeChordQuality) ]
        (map (\cqName -> HH.option [ ] [ HH.text cqName ]) chordQualityNames)
      , HH.button
          [ HE.onClick (HE.input_ Play) ]
          [ HH.text  "Play"
          ]
      ]

  initCtx :: ∀ eff. Maybe AudioContext -> (Eff ( wau :: WebAudio | eff) AudioContext)
  initCtx (Just ctx) = pure ctx
  initCtx (Nothing) = AuCtx.makeAudioContext

  init :: ∀ eff a. (Eff (wau :: WebAudio | eff) a) -> Maybe a -> (Eff ( wau :: WebAudio | eff) a)
  init a Nothing = a
  init _ (Just a) = pure a


  initSink :: forall eff. AudioContext -> (Eff ( wau :: WebAudio, console :: CONSOLE | eff) DynamicsCompressorNode)
  initSink ctx = do
    dest <- AuCtx.destination ctx
    compressor <- createDynamicsCompressor ctx
    AuParam.setValue 1.0 =<< threshold compressor
    AuParam.setValue 30.0 =<< ratio compressor
    AuCtx.connect compressor dest
    pure compressor

  eval :: Query ~> H.ComponentDSL State Query Void (Aff ( wau :: WebAudio, console :: CONSOLE | eff ))
  eval = case _ of
    ChangePitchClass str next -> do
      pc <- H.gets (_.chordPitchClass)
      H.modify (\state -> state { chordPitchClass = (fromMaybe state.chordPitchClass (parsePitchClass str)) })
      pure next

    ChangeChordQuality str next -> do
      cq <- H.gets (_.chordQuality)
      H.modify (\state -> state { chordQuality = (fromMaybe state.chordQuality (parseChordQuality str)) })
      pure next

    Play next -> do
      ctx <- H.liftEff <<< init AuCtx.makeAudioContext =<< H.gets (_.ctx)
      dest <- H.liftEff <<< init (initSink ctx) =<< H.gets (_.dest)

      pc <- H.gets (_.chordPitchClass)
      cq <- H.gets (_.chordQuality)

      H.liftEff $ playSequence ctx dest 2.0 $ singleton $ makeTriad' cq pc (Octave 4)

      H.modify (\state -> state { ctx = Just ctx
                                , dest = Just dest })
      pure next
