module Midi.Info ( track_info
                 , info_ascii_meta_event) where
import Data.Char (chr)
import Midi.Const
import Midi.Format

data TrackInfo = TrackInfo { len :: Int
                           }
               deriving (Show)


track_info (TrackChunk len events) = TrackInfo len


info_ascii_meta_event (MetaEvent _ _ e _ d) = show e ++ map (chr . fromEnum) d
