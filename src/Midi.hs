module Midi ( module Sure
            , module Midi.Format
            , module Midi.Note
            , module Midi.Parse
            , module Midi.Const) where
import Sure
import Midi.Format
import Midi.Const
import Midi.Parse
import Midi.Note


index_track (MidiTrackChunk _ events) = f 0 events where f _ [] = []
                                                         f past (e:es) = let past' = past + delta_time e
                                                                         in  (past', e) : f past' es
