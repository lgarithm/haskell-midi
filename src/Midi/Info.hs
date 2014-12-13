module Midi.Info (track_info) where
import Midi.Const
import Midi.Format

data TrackInfo = TrackInfo { len :: Int
                           }
               deriving (Show)


track_info (TrackChunk len events) = TrackInfo len
