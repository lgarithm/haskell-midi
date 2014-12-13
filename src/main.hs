module Main where
import System.Environment
import Sure
import Midi

main = do { file:_ <- getArgs
          ; midi <- pMidiFromFile file
          ; print . sure $ midi }
