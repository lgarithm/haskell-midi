module Midi.Note where
import Data.List
import Data.Word
import Midi.Format
import Midi.Const

data Note = Note { pitch :: Word8
                 , interval :: Int }
          deriving (Show)


take_find p s = let idx' = findIndex p s
                in  case idx' of
                  Just idx -> Just (s !! idx, take idx s ++ drop (idx + 1) s)

pair_notes p s [] = reverse p
pair_notes p s (n@(_, (CtrlEvent _ _ NoteOn ch [pitch, _])) : ns) = pair_notes p (s ++ [n]) ns
pair_notes p s (n@(_, (CtrlEvent _ _ NoteOff ch [pitch, _])) : ns) = let m' = take_find ((== pitch) . head . parameters . snd) s
                                                                     in  case m' of
                                                                       Just (m, s') -> pair_notes ((m, n) : p) s' ns

gen_note (m, n) = Note (head . parameters . snd $ m) (fst n - fst m)
