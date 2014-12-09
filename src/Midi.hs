module Midi where
import Control.Monad
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Char (chr, ord)
import Data.List.Split (chunksOf)
import Text.Parsec.ByteString (Parser)
import Text.ParserCombinators.Parsec (parse, many, (<|>))
import Text.ParserCombinators.Parsec.Char (anyChar)
import Text.ParserCombinators.Parsec.Combinator (lookAhead)

data MidiEvent = MidiEvent { status_byte :: Char
                           , delta_time :: Int
                           , midi_type :: Char
                           , channel :: Char
                           , parameters :: String }
               | SysexEvent { status_byte :: Char
                            , delta_time :: Int
                            , sysex_len :: Int
                            , sysex_data :: String }
               | MetaEvent { status_byte :: Char
                           , delta_time :: Int
                           , meta_type :: Char
                           , meta_len :: Int
                           , meta_data :: String }
               deriving (Show)

data MidiChunk = MidiHeadChunk { format :: Int
                               , ntrks :: Int
                               , division :: Int }
               | MidiTrackChunk { track_len :: Int
                                , midi_events :: [MidiEvent] }
               deriving (Show)

class Sure m where sure :: m a -> a
instance Sure Maybe where sure (Just x) = x
instance Sure (Either e) where sure (Right x) = x

carry d x y = d * x + y
bytes2int = foldl (carry 256) 0 . map fromEnum

pVarlength = p 0 where p acc = do { byte <- anyChar >>= return . fromEnum
                                  ; if byte < 128 then return $ acc + byte
                                    else p $ (acc + byte - 128) * 128 } :: Parser Int

pSysexEvent status_byte delta_time = do { sysex_len <- pVarlength
                                        ; sysex_data <- replicateM sysex_len anyChar
                                        ;return $ SysexEvent status_byte delta_time sysex_len sysex_data }

pMetaEvent status_byte delta_time = do { meta_type <- anyChar
                                       ; meta_len <- pVarlength
                                       ; meta_data <- replicateM meta_len anyChar
                                       ; return $ MetaEvent status_byte delta_time meta_type meta_len meta_data }

pMidiEvent status_byte delta_time = do { midi_type <- return $ chr (fromEnum status_byte .&. fromEnum '\xF0')
                                       ; channel <- return $ chr (fromEnum status_byte .&. fromEnum '\x0F')
                                       ; len <- return $ sure . lookup midi_type $ zip "\x80\x90\xA0\xB0\xC0\xD0\xE0" [2, 2, 2, 2, 1, 1, 2]
                                       ; parameters <- replicateM len anyChar
                                       ; return $ MidiEvent status_byte delta_time midi_type channel parameters }

pEventWith '\xF0' delta_time = pSysexEvent '\xF0' delta_time
pEventWith '\xF7' delta_time = pSysexEvent '\xF7' delta_time
pEventWith '\xFF' delta_time = pMetaEvent '\xFF' delta_time
pEventWith status_byte delta_time = pMidiEvent status_byte delta_time

pEventLast last_status_byte = do { delta_time <- pVarlength
                                 ; byte <- lookAhead anyChar
                                 ; status_byte <- if (fromEnum byte) < 128 then return byte else anyChar
                                 ; pEventWith status_byte delta_time } :: Parser MidiEvent

pEvents = p '\0' [] where p last_status_byte events = do { e <- pEventLast last_status_byte
                                                         ; p (status_byte e) (e:events) } <|> (return $ reverse events)

pMidiChunkWith "MThd" len track = let [x, y, z] = map bytes2int (chunksOf 2 track) in MidiHeadChunk x y z
pMidiChunkWith "MTrk" len track = let events = sure . parse pEvents "MTrk" $ BS.pack $ map (fromIntegral . ord) track
                                  in MidiTrackChunk len events

pMidiChunk = do { mgk <- replicateM 4 anyChar
                ; len <- replicateM 4 anyChar >>= return . bytes2int
                ; track <- replicateM len anyChar
                ; track_data <- return $ BS.pack $ map (fromIntegral . ord) track
                ; return $ pMidiChunkWith mgk len track } :: Parser MidiChunk

pMidiFile = many pMidiChunk

pMidiFromFile file = BS.readFile file >>= return . parse pMidiFile file
