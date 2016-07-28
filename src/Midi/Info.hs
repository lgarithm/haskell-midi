{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Midi.Info ( track_info
                 , info_ascii_meta_event) where
import           Data.Char   (chr)
import           Midi.Format (MidiChunk (TrackChunk), MidiEvent (MetaEvent))

data TrackInfo = TrackInfo { len :: Int
                           }
               deriving (Show)


track_info (TrackChunk len _) = TrackInfo len


info_ascii_meta_event (MetaEvent _ _ e _ d) = show e ++ map (chr . fromEnum) d
