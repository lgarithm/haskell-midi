module Main where
import System.Environment
import Sure
import Midi

main = getArgs >>= \args -> case args of
  [file] -> pMidiFromFile file >>= return . sure >>= print
  ["info", file] -> do { midi <- pMidiFromFile file >>= return . sure
                       ; print $ head_chunk midi }
