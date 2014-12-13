module Midi.Const where
import Data.Word
import Sure

data EventType = NoteOff
               | NoteOn
               | NoteAftertouch
               | Controller
               | ProgramChange
               | ChannelAftertouch
               | PitchBend
               deriving (Eq, Ord, Show)

ctrl_events = [NoteOff, NoteOn, NoteAftertouch, Controller, ProgramChange, ChannelAftertouch, PitchBend]

event_parameter_lentgh = zip ctrl_events ([2, 2, 2, 2, 1, 1, 2] :: [Int])
event_code_table = zip [0x80, 0x90 .. ] ctrl_events :: [(Word8, EventType)]
code_event_table = map f event_code_table where f (x, y) = (y, x)

event_code = sure . flip lookup code_event_table
code_event = sure . flip lookup event_code_table
parameter_lentgh = sure . flip lookup event_parameter_lentgh
