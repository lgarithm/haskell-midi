module Main where
import System.Environment
import Midi

show_chunk (MidiHeadChunk x y z) = unlines $ map show $ zip ["format", "ntrks", "division"] [x, y, z]
show_chunk (MidiTrackChunk len events) = unlines $ (show len) : (map show events)

instance Show MidiFile where show (MidiFile chunks)= unlines $ map show_chunk chunks

main = do { file:_ <- getArgs
          ; midi <- pMidiFromFile file
          ; print . sure $ midi }
