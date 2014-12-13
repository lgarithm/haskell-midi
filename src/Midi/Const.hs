module Midi.Const where
import Data.Word

data EventType = NoteOff
               | NoteOn
               | NoteAftertouch
               | Controller
               | ProgramChange
               | ChannelAftertouch
               | PitchBend
               deriving (Eq, Ord, Show)


event_code_table = [ (0x80, NoteOff)
                   , (0x90, NoteOn)
                   , (0xa0, NoteAftertouch)
                   , (0xb0, Controller)
                   , (0xc0, ProgramChange)
                   , (0xd0, ChannelAftertouch)
                   , (0xe0, PitchBend)] :: [(Word8, EventType)]

code_event_table = map f event_code_table where f (x, y) = (y, x)

event_code e = let Just c = lookup e code_event_table in c
code_event c = let Just e = lookup c event_code_table in e
