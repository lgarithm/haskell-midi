module Midi.Format where
import Data.Word
import Midi.Const

data MidiEvent = CtrlEvent { status_byte :: Word8
                           , delta_time :: Int
                           , event_type :: EventType
                           , channel :: Word8
                           , parameters :: [Word8] }
               | MetaEvent { status_byte :: Word8
                           , delta_time :: Int
                           , event_type :: EventType
                           , meta_len :: Int
                           , meta_data :: [Word8] }
               | SysexEvent { status_byte :: Word8
                            , delta_time :: Int
                            , event_type :: EventType
                            , sysex_len :: Int
                            , sysex_data :: [Word8] }
               deriving (Ord, Eq, Show)

data MidiChunk = HeadChunk1 { format :: Int
                            , ntrks :: Int
                            , division :: Int }
               | TrackChunk { track_len :: Int
                            , midi_events :: [MidiEvent] }
               deriving (Show)

show_chunk (HeadChunk1 x y z) = unlines . map show . zip ["format", "ntrks", "division"] $ [x, y, z]
show_chunk (TrackChunk len events) = unlines $ (show len) : (map show events)

data MidiFile = MidiFile [MidiChunk]
instance Show MidiFile where show (MidiFile chunks) = unlines . map show_chunk $ chunks

head_chunk (MidiFile chunks) = head chunks
