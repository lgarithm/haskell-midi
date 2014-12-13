module Midi.Format where
import Data.Word
import Midi.Const

data MidiEvent = MidiEvent { status_byte :: Word8
                           , delta_time :: Int
                           , event_type :: EventType
                           , channel :: Word8
                           , parameters :: [Word8] }
               | SysexEvent { status_byte :: Word8
                            , delta_time :: Int
                            , sysex_len :: Int
                            , sysex_data :: [Word8] }
               | MetaEvent { status_byte :: Word8
                           , delta_time :: Int
                           , meta_type :: Word8
                           , meta_len :: Int
                           , meta_data :: [Word8] }
               deriving (Show)

data MidiChunk = MidiHeadChunk1 { format :: Int
                                , ntrks :: Int
                                , division :: Int }
               | MidiTrackChunk { track_len :: Int
                                , midi_events :: [MidiEvent] }
               deriving (Show)

show_chunk (MidiHeadChunk1 x y z) = unlines $ map show $ zip ["format", "ntrks", "division"] [x, y, z]
show_chunk (MidiTrackChunk len events) = unlines $ (show len) : (map show events)

data MidiFile = MidiFile [MidiChunk]
instance Show MidiFile where show (MidiFile chunks) = unlines $ map show_chunk chunks
