module Midi.Parse ( pMidiFile
                  , pMidiFromFile) where
import Control.Monad (replicateM)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS (pack, readFile)
import Data.Char (chr, ord)
import Data.List.Split (chunksOf)
import Data.Word (Word8)
import Text.Parsec.ByteString (Parser)
import Text.ParserCombinators.Parsec (parse, many, (<|>))
import Text.ParserCombinators.Parsec.Char (anyChar)
import Text.ParserCombinators.Parsec.Combinator (lookAhead)
import Midi.Format
import Midi.Const
import Sure

carry d x y = d * x + y
bytes2int n = foldl (carry n) 0 . map fromEnum
anyByte = anyChar >>= return . fromIntegral . fromEnum :: Parser Word8

pVarlength = p [] >>= return . bytes2int 128 where p acc = do { byte <- anyByte
                                                              ; if byte < 128 then return . reverse $ byte : acc
                                                                else p $ (byte - 128) : acc } :: Parser [Word8]

pCtrlEvent status_byte delta_time = do { midi_type <- return . ctrl_event $ status_byte .&. 0xf0
                                       ; channel <- return $ status_byte .&. 0x0f
                                       ; len <- return $ parameter_lentgh midi_type
                                       ; parameters <- replicateM len anyByte
                                       ; return $ CtrlEvent status_byte delta_time midi_type channel parameters }
pMetaEvent status_byte delta_time = do { meta_type <- anyByte >>= return . meta_event
                                       ; meta_len <- pVarlength
                                       ; meta_data <- replicateM meta_len anyByte
                                       ; return $ MetaEvent status_byte delta_time meta_type meta_len meta_data }
pSysexEvent status_byte delta_time = do { sysex_len <- pVarlength
                                        ; sysex_data <- replicateM sysex_len anyByte
                                        ; return $ SysexEvent status_byte delta_time Sysex sysex_len sysex_data }

pEventWith 0xf0 delta_time = pSysexEvent 0xf0 delta_time
pEventWith 0xf7 delta_time = pSysexEvent 0xf7 delta_time
pEventWith 0xff delta_time = pMetaEvent 0xff delta_time
pEventWith status_byte delta_time = pCtrlEvent status_byte delta_time

pEventLast last_status_byte = do { delta_time <- pVarlength
                                 ; byte <- lookAhead anyByte
                                 ; status_byte <- if byte < 128 then return byte else anyByte
                                 ; pEventWith status_byte delta_time } :: Parser MidiEvent

pEvents = p 0 [] where p last_status_byte events = do { e <- pEventLast last_status_byte
                                                      ; p (status_byte e) (e:events) } <|> (return $ reverse events)

pChunkWith "MThd" len track = HeadChunk1 x y z where [x, y, z] = map (bytes2int 256) . chunksOf 2 $ track
pChunkWith "MTrk" len track = TrackChunk len events where events = sure . parse pEvents "MTrk" . BS.pack $ track

pChunk = do { mgk <- replicateM 4 anyChar
            ; len <- replicateM 4 anyByte >>= return . bytes2int 256
            ; track <- replicateM len anyByte
            ; return $ pChunkWith mgk len track } :: Parser MidiChunk

pMidiFile = many pChunk >>= return . MidiFile
pMidiFromFile file = BS.readFile file >>= return . parse pMidiFile file
