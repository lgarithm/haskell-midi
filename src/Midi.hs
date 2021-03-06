module Midi ( index_track
            , module Midi.Const
            , module Midi.Format
            , module Midi.Parse
            , module Midi.Info
            , module Midi.Note) where
import           Midi.Const
import           Midi.Format
import           Midi.Info
import           Midi.Note
import           Midi.Parse

index_track (TrackChunk _ events) = f 0 events
    where f _ [] = []
          f past (e:es) = let past' = past + delta_time e
                          in  (past', e) : f past' es
