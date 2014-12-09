module Midi where
import Control.Monad (replicateM)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Char (chr, ord)
import Data.List.Split (chunksOf)
import Data.Word (Word8)
import Text.Parsec.ByteString (Parser)
import Text.ParserCombinators.Parsec (parse, many, (<|>))
import Text.ParserCombinators.Parsec.Char (anyChar)
import Text.ParserCombinators.Parsec.Combinator (lookAhead)

data MidiEvent = MidiEvent { status_byte :: Word8
                           , delta_time :: Int
                           , midi_type :: Word8
                           , channel :: Word8
                           , parameters :: [Word8] }
               | SysexEvent { status_byte :: Word8
                            , delta_time :: Int
                            , sysex_len :: Int
                            , sysex_data :: [Word8] }
               | MetaEvent { status_byte :: Word8
                           , delta_time :: Int
                           , meta_type :: Word8
                           , meta_len :: Int
                           , meta_data :: [Word8] }
               deriving (Show)

data MidiChunk = MidiHeadChunk1 { format :: Int
                                , ntrks :: Int
                                , division :: Int }
               | MidiTrackChunk { track_len :: Int
                                , midi_events :: [MidiEvent] }
               deriving (Show)

data MidiFile = MidiFile [MidiChunk]

class Sure m where sure :: m a -> a
instance Sure Maybe where sure (Just x) = x
instance Sure (Either e) where sure (Right x) = x

carry d x y = d * x + y
bytes2int n = foldl (carry n) 0 . map fromEnum

anyByte = anyChar >>= return . fromIntegral . fromEnum :: Parser Word8

pVarlength = p [] >>= return . bytes2int 128 where p acc = do { byte <- anyByte
                                                              ; if byte < 128 then return . reverse $ byte : acc
                                                                else p $ (byte - 128) : acc } :: Parser [Word8]

pSysexEvent status_byte delta_time = do { sysex_len <- pVarlength
                                        ; sysex_data <- replicateM sysex_len anyByte
                                        ; return $ SysexEvent status_byte delta_time sysex_len sysex_data }

pMetaEvent status_byte delta_time = do { meta_type <- anyByte
                                       ; meta_len <- pVarlength
                                       ; meta_data <- replicateM meta_len anyByte
                                       ; return $ MetaEvent status_byte delta_time meta_type meta_len meta_data }

pMidiEvent status_byte delta_time = do { midi_type <- return $ status_byte .&. 0xf0
                                       ; channel <- return $ status_byte .&. 0x0f
                                       ; len <- return $ sure . lookup midi_type $ zip [0x80, 0x90 .. ] [2, 2, 2, 2, 1, 1, 2]
                                       ; parameters <- replicateM len anyByte
                                       ; return $ MidiEvent status_byte delta_time midi_type channel parameters }

pEventWith 0xf0 delta_time = pSysexEvent 0xf0 delta_time
pEventWith 0xf7 delta_time = pSysexEvent 0xf7 delta_time
pEventWith 0xff delta_time = pMetaEvent 0xff delta_time
pEventWith status_byte delta_time = pMidiEvent status_byte delta_time

pEventLast last_status_byte = do { delta_time <- pVarlength
                                 ; byte <- lookAhead anyByte
                                 ; status_byte <- if byte < 128 then return byte else anyByte
                                 ; pEventWith status_byte delta_time } :: Parser MidiEvent

pEvents = p 0 [] where p last_status_byte events = do { e <- pEventLast last_status_byte
                                                      ; p (status_byte e) (e:events) } <|> (return $ reverse events)

pMidiChunkWith "MThd" len track = MidiHeadChunk1 x y z where [x, y, z] = map (bytes2int 266) . chunksOf 2 $ track
pMidiChunkWith "MTrk" len track = MidiTrackChunk len events where events = sure . parse pEvents "MTrk" . BS.pack $ track

pMidiChunk = do { mgk <- replicateM 4 anyChar
                ; len <- replicateM 4 anyByte >>= return . bytes2int 256
                ; track <- replicateM len anyByte
                ; return $ pMidiChunkWith mgk len track } :: Parser MidiChunk

pMidiFile = many pMidiChunk >>= return . MidiFile

pMidiFromFile file = BS.readFile file >>= return . parse pMidiFile file
