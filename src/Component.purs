module Component where

import Data.List.Lazy (singleton)
import Music.Chords
import Music.LetterNotation
import Music.MidiNote
import Prelude
import Synths
import Synths.Sequence (playSequence)
import WebAudio.DynamicsCompressorNode
import Audio.WebAudio.AudioContext as AuCtx
import Audio.WebAudio.AudioParam as AuParam
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Newtype (unwrap)
import Data.List
import Data.Array as Array

data Query a = Play a | ChangeChordQuality String a | ChangePitchClass String a

type State = { ctx :: Maybe AudioContext
             , dest :: Maybe DynamicsCompressorNode
             , chordQuality :: ChordQuality
             , chordPitchClass :: PitchClassDescription
             , chordInversion :: ChordInversion
             , octave :: Octave}

makeChord :: State -> Chord
makeChord record = makeTriad' record.chordQuality record.chordPitchClass (Octave 4)

renderNote :: Note -> H.ComponentHTML Query
renderNote note = HH.div [ ] [ HH.text ("note: " <> (show (pitchClassDescription)) <> " (root note + " <> (show $ interval) <> " semitones) " <> show midiNote ) ]
  where
    pitchClass = notePitchClass note
    pitchClassDescription = pitchClassLetterNotation pitchClass
    interval :: Int
    interval = unwrap pitchClass
    midiNote = toMidiNote note

renderChordPitches :: Chord -> Array (H.ComponentHTML Query)
renderChordPitches chord = Array.fromFoldable $ map (\p -> HH.div [ ] [ HH.text (show p) ]) pitches
  where pitches = map toHz $ chordNotes RootPosition chord

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
                 , chordPitchClass: (PitchClassDescription C Natural)
                 , chordInversion: RootPosition
                 , octave: (Octave 4) }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Hello world!" ]
      , HH.p_
          [ HH.text "Why not toggle this button:" ]
      , HH.select [ HE.onValueChange (HE.input ChangePitchClass) ]
        (map (\noteName -> HH.option [ ] [ HH.text noteName ]) pitchClassNames)
      , HH.select [ HE.onValueChange (HE.input ChangeChordQuality) ]
        (map (\cqName -> HH.option [ ] [ HH.text cqName ]) chordQualityNames)
      , HH.button
          [ HE.onClick (HE.input_ Play) ]
          [ HH.text  "Play"
          ]
      , HH.p [ ] [ HH.text $ "Octave " <> show state.octave ]
      , HH.p [ ] (Array.fromFoldable $ map renderNote (chordNotes RootPosition chord))
      , HH.p [ ] ( [ HH.text ("pitches:") ] <> renderChordPitches chord )
      ]
    where chord = makeChord state

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

      H.liftEff $ Console.log $ maybe ("could not parse string \"" <> str <> "\"") (\_ -> "successfully parsed string \"" <> str <> "\"") $ parsePitchClass str

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


      state <- H.gets id
      H.liftEff $ playSequence ctx dest 2.0 $ singleton $ makeChord state

      H.modify (\state -> state { ctx = Just ctx
                                , dest = Just dest })
      pure next
