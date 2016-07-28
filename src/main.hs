module Main where
import           Midi               (head_chunk, pMidiFromFile)
import           Sure               (sure)
import           System.Environment (getArgs, getProgName)

main = getArgs >>= \args -> case args of
  [file] -> pMidiFromFile file >>= return . sure >>= print
  ["info", file] -> do { midi <- pMidiFromFile file >>= return . sure
                       ; print $ head_chunk midi }
  _ -> usageF [[], ["info"]]

usageF actions = getProgName >>= mapM_ putStrLn . helps where
  helps cmd = "Usage:" :
    [ "\t" ++ (unwords $ cmd : cmds ++ ["<file>"]) | cmds <- actions ]
