module Midi.Const where
import Data.Tuple (swap)
import Data.Word (Word8)
import Sure

data EventType = NoteOff
               | NoteOn
               | NoteAftertouch
               | Controller
               | ProgramChange
               | ChannelAftertouch
               | PitchBend
                 -- Meta Events
               | SequenceNumber
               | TextEvent
               | CopyrightNotice
               | TrackName
               | InstrumentName
               | Lyrics
               | Marker
               | CuePoint
               | MidiChannelPrefix
               | EndOfTrack
               | SetTempo
               | SmpteOffset
               | TimeSignature
               | KeySignature
               | SequencerSpecific
                 -- Sysex Events
               | Sysex -- unparsed
               | SysexNormal
               | DividedContinue
               | DividedLast
               | Authorization
               deriving (Eq, Ord, Show)

ctrl_events = [ NoteOff
              , NoteOn
              , NoteAftertouch
              , Controller
              , ProgramChange
              , ChannelAftertouch
              , PitchBend ]

meta_events = [ SequenceNumber
              , TextEvent
              , CopyrightNotice
              , TrackName
              , InstrumentName
              , Lyrics
              , Marker
              , CuePoint
              , MidiChannelPrefix
              , EndOfTrack
              , SetTempo
              , SmpteOffset
              , TimeSignature
              , KeySignature
              , SequencerSpecific ]

ctrl_event_code_table = zip ([0x80, 0x90 .. ] :: [Word8]) ctrl_events
meta_event_code_table = zip ([0, 1, 2, 3, 4, 5, 6, 7, 32, 47, 81, 84, 88, 89, 127] :: [Word8]) meta_events
event_codes =  map swap $ ctrl_event_code_table ++ meta_event_code_table

isCtrlEvent = flip elem ctrl_events
isMetaEvent = flip elem meta_events
isAsciiMetaEvent = flip elem [1..7] . sure . flip lookup event_codes

ctrl_event = sure . flip lookup ctrl_event_code_table
meta_event = sure . flip lookup meta_event_code_table

event_parameter_lentgh = zip ctrl_events ([2, 2, 2, 2, 1, 1, 2] :: [Int])
parameter_lentgh = sure . flip lookup event_parameter_lentgh
