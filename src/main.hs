module Main where
import System.Environment
import Midi

main = do { file:_ <- getArgs
          ; midi <- pMidiFromFile file
          ; print . sure $ midi }
