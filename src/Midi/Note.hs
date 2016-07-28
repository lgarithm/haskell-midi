{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Midi.Note where
import           Data.List
import           Data.Word
import           Midi.Const
import           Midi.Format

data Note = Note { offset   :: Int
                 , interval :: Int
                 , pitch    :: Word8 }
          deriving (Show)

data RawNote = RawNote { raw_offset   :: Rational
                       , raw_interval :: Rational
                       , raw_pitch    :: Word8 }
             deriving (Show)

channeln n = f where f (CtrlEvent _ _ _ c _) = c == n
                     f _ = False

take_find p s = let idx' = findIndex p s
                in  case idx' of
                  Just idx -> Just (s !! idx, take idx s ++ drop (idx + 1) s)

pair_notes p _ [] = reverse p
pair_notes p s (n@(_, (CtrlEvent _ _ NoteOn _ [_, _])) : ns) = pair_notes p (s ++ [n]) ns
pair_notes p s (n@(_, (CtrlEvent _ _ NoteOff _ [pitch, _])) : ns) = let m' = take_find ((== pitch) . head . parameters . snd) s
                                                                     in  case m' of
                                                                       Just (m, s') -> pair_notes ((m, n) : p) s' ns

note (m, n) = Note (fst m) (fst n - fst m) (head . parameters . snd $ m)

ratio a b = fromIntegral a / fromIntegral b :: Rational
rd d x = ratio (round $ fromRational x * fromIntegral d)  d
r16 = rd 16

align bpm (Note offset interval pitch) = RawNote (f offset) (f interval) pitch where f = r16 . flip ratio bpm
