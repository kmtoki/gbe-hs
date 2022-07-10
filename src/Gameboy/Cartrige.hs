module Gameboy.Cartrige where

import Gameboy.Utils

import Data.Bits
import Data.Char
import qualified Data.ByteString as B
import qualified Data.Vector as V

data Cartrige 
  = Cartrige {
    _entryPoint :: ROM,
    _logo :: ROM,
    _title :: String,
    _manufacturerCode :: ROM,
    _cgbFlag :: CGBFlag,
    _newLicenseeCode :: String,
    _sgbFlag :: SGBFlag,
    _mbcType :: MBCType,
    _romSize :: Int,
    _ramxSize :: Int,
    _destinationCode :: DestinationCode,
    _oldLicenseeCode :: String,
    _maskROMVersionNumber :: Word8,
    _headerChecksum :: Word8,
    _globalChecksum :: Word16,
    _rom :: ROM
  }

instance Show Cartrige where
  show (Cartrige { .. }) = 
    "Cartrige { " 
    ++ "entryPoint = " ++ show _entryPoint 
    -- ++ ", logo = " ++ show _logo 
    ++ ", logo = [...]"
    ++ ", title = " ++ (show $ filter (/= '\NUL') _title)
    ++ ", manufacturerCode = " ++ show _manufacturerCode 
    ++ ", cgbFlag = " ++ show _cgbFlag 
    ++ ", newLicenseeCode = " ++ show _newLicenseeCode 
    ++ ", sgbFlag = " ++ show _sgbFlag 
    ++ ", mbcType = " ++ show _mbcType 
    ++ ", romSize = " ++ show _romSize 
    ++ ", ramxSize = " ++ show _ramxSize 
    ++ ", destinationCode = " ++ show _destinationCode 
    ++ ", oldLicenseeCode = " ++ show _oldLicenseeCode 
    ++ ", maskROMVersionNumber = " ++ show _maskROMVersionNumber 
    ++ ", headerChecksum = " ++ show _headerChecksum 
    ++ ", globalChecksum = " ++ show _globalChecksum 
    ++ ", rom = ROM[0x" ++ showHex (V.length _rom) ++ "]"
    ++ " }"
 

data CGBFlag = CGBOnly | CGBSupport
  deriving Show

data SGBFlag = SGBSupport | NotSGBSupport
  deriving Show

data MBCType
  = MBC0 -- ROM_ONLY
  | MBC1
  | MBC1_RAM
  | MBC1_RAM_BATTERY
  | MBC2
  | MBC2_BATTERY
  | ROM_RAM
  | ROM_RAM_BATTERY
  | MMM01
  | MMM01_RAM
  | MMM01_RAM_BATTERY
  | MBC3_TIMER_BATTERY
  | MBC3_TIMER_RAM_BATTERY
  | MBC3
  | MBC3_RAM
  | MBC3_RAM_BATTERY
  | MBC5
  | MBC5_RAM
  | MBC5_RAM_BATTERY
  | MBC5_RUMBLE
  | MBC5_RUMBLE_RAM
  | MBC5_RUMBLE_RAM_BATTERY
  | MBC6
  | MBC7_SENSOR_RUMBLE_RAM_BATTERY
  | POCKET_CAMERA
  | BANDAI_TAMA5
  | HuC3
  | HuC1_RAM_BATTERY
  deriving Show

data DestinationCode = Japanese | NonJapanese
  deriving Show

makeLenses ''Cartrige

readCartrige :: String -> IO Cartrige
readCartrige s = parseROM <$> readFileROM s

readFileROM :: String -> IO ROM
readFileROM s = fromByteString <$> B.readFile s

fromByteString :: B.ByteString -> ROM
fromByteString = V.fromList . B.unpack

parseROM :: ROM -> Cartrige
parseROM v = Cartrige { .. }
  where
    (!) = (V.!)
    read = V.fromList . map (v !)
    _entryPoint = read [0x100..0x103]
    _logo = read [0x104..0x133]
    _title = map (chr . fromIntegral . (v !)) [0x134..0x143]
    _manufacturerCode = read [0x13f..0x142]
    _cgbFlag = cgbFlagTable $ v ! 0x143
    _newLicenseeCode = newLicenseeTable $ map (chr . fromIntegral . (v !)) [0x144..0x145]
    _sgbFlag = sgbFlagTable $ v ! 0x146
    _mbcType = mbcTypeTable $ v ! 0x147
    _romSize = romSizeTable $ v ! 0x148
    _ramxSize = ramxSizeTable $ v ! 0x149
    _destinationCode = destinationCodeTable $ v ! 0x14a
    _oldLicenseeCode = oldLicenseeCodeTable $ v ! 0x14b
    _maskROMVersionNumber = v ! 0x14c
    _headerChecksum = v ! 0x14d
    _globalChecksum = shift (fromIntegral $ v ! 0x14e) 8 .|.  (fromIntegral $ v ! 0x14f)
    _rom = v
      
cgbFlagTable :: Word8 -> CGBFlag
cgbFlagTable w = case w of
  0xc0 -> CGBOnly
  0x80 -> CGBSupport
  _ -> CGBSupport
 
sgbFlagTable :: Word8 -> SGBFlag
sgbFlagTable w = case w of
  0x03 -> SGBSupport
  0x00 -> NotSGBSupport
  _ -> NotSGBSupport
 
newLicenseeTable :: String -> String
newLicenseeTable c = case c of
  "00" -> "None"
  "01" -> "Nintendo R&D1"
  "08" -> "Capcom"
  "13" -> "Electronic Arts"
  "18" -> "Hudson Soft"
  "19" -> "b-ai"
  "20" -> "kss"
  "22" -> "pow"
  "24" -> "PCM Complete"
  "25" -> "san-x"
  "28" -> "Kemco Japan"
  "29" -> "seta"
  "30" -> "Viacom"
  "31" -> "Nintendo"
  "32" -> "Bandai"
  "33" -> "Ocean/Acclaim"
  "34" -> "Konami"
  "35" -> "Hector"
  "37" -> "Taito"
  "38" -> "Hudson"
  "39" -> "Banpresto"
  "41" -> "Ubi Soft"
  "42" -> "Atlus"
  "44" -> "Malibu"
  "46" -> "angel"
  "47" -> "Bullet-Proof"
  "49" -> "irem"
  "50" -> "Absolute"
  "51" -> "Acclaim"
  "52" -> "Activision"
  "53" -> "American sammy"
  "54" -> "Konami"
  "55" -> "Hi tech entertainment"
  "56" -> "LJN"
  "57" -> "Matchbox"
  "58" -> "Mattel"
  "59" -> "Milton Bradley"
  "60" -> "Titus"
  "61" -> "Virgin"
  "64" -> "LucasArts"
  "67" -> "Ocean"
  "69" -> "Electronic Arts"
  "70" -> "Infogrames"
  "71" -> "Interplay"
  "72" -> "Broderbund"
  "73" -> "sculptured"
  "75" -> "sci"
  "78" -> "THQ"
  "79" -> "Accolade"
  "80" -> "misawa"
  "83" -> "lozc"
  "86" -> "Tokuma Shoten Intermedia"
  "87" -> "Tsukuda Original"
  "91" -> "Chunsoft"
  "92" -> "Video system"
  "93" -> "Ocean/Acclaim"
  "95" -> "Varie"
  "96" -> "Yonezawa/s’pal"
  "97" -> "Kaneko"
  "99" -> "Pack in soft"
  "A4" -> "Konami (Yu-Gi-Oh!)"
  _ -> "undefined"


mbcTypeTable :: Word8 -> MBCType
mbcTypeTable w = case w of
  0x00 -> MBC0
  0x01 -> MBC1
  0x02 -> MBC1_RAM
  0x03 -> MBC1_RAM_BATTERY
  0x05 -> MBC2
  0x06 -> MBC2_BATTERY
  0x08 -> ROM_RAM
  0x09 -> ROM_RAM_BATTERY
  0x0B -> MMM01
  0x0C -> MMM01_RAM
  0x0D -> MMM01_RAM_BATTERY
  0x0F -> MBC3_TIMER_BATTERY
  0x10 -> MBC3_TIMER_RAM_BATTERY
  0x11 -> MBC3
  0x12 -> MBC3_RAM
  0x13 -> MBC3_RAM_BATTERY
  0x19 -> MBC5
  0x1A -> MBC5_RAM
  0x1B -> MBC5_RAM_BATTERY
  0x1C -> MBC5_RUMBLE
  0x1D -> MBC5_RUMBLE_RAM
  0x1E -> MBC5_RUMBLE_RAM_BATTERY
  0x20 -> MBC6
  0x22 -> MBC7_SENSOR_RUMBLE_RAM_BATTERY
  0xFC -> POCKET_CAMERA
  0xFD -> BANDAI_TAMA5
  0xFE -> HuC3
  0xFF -> HuC1_RAM_BATTERY
  _    -> MBC0

romSizeTable :: Word8 -> Int
romSizeTable w = case w of
  0x00 -> 32  * 2 ^ 10
  0x01 -> 64  * 2 ^ 10
  0x02 -> 128 * 2 ^ 10
  0x03 -> 256 * 2 ^ 10
  0x04 -> 512 * 2 ^ 10
  0x05 -> 1 * 2 ^ 20
  0x06 -> 2 * 2 ^ 20
  0x07 -> 4 * 2 ^ 20
  0x08 -> 8 * 2 ^ 20
  0x52 -> ceiling $ 1.1 * 2 ** 20
  0x53 -> ceiling $ 1.2 * 2 ** 20
  0x54 -> ceiling $ 1.5 * 2 ** 20

ramxSizeTable :: Word8 -> Int
ramxSizeTable w = case w of
  0x00 -> 0
  0x01 -> 0
  0x02 -> 8 * 2 ^ 10
  0x03 -> 32 * 2 ^ 10
  0x04 -> 128 * 2 ^ 10
  0x05 -> 64 * 2 ^ 10

destinationCodeTable :: Word8 -> DestinationCode
destinationCodeTable w = case w of
  0x00 -> Japanese
  0x01 -> NonJapanese
  _    -> NonJapanese

oldLicenseeCodeTable :: Word8 -> String
oldLicenseeCodeTable w = case w of
  0x00 -> "none"
  0x01 -> "nintendo"
  0x08 -> "capcom"
  0x09 -> "hot-b"
  0x0A -> "jaleco"
  0x0B -> "coconuts"
  0x0C -> "elite systems"
  0x13 -> "electronic arts"
  0x18 -> "hudsonsoft"
  0x19 -> "itc entertainment"
  0x1A -> "yanoman"
  0x1D -> "clary"
  0x1F -> "virgin"
  0x24 -> "pcm complete"
  0x25 -> "san-x"
  0x28 -> "kotobuki systems"
  0x29 -> "seta"
  0x30 -> "infogrames"
  0x31 -> "nintendo"
  0x32 -> "bandai"
  0x33 -> "see above"
  0x34 -> "konami"
  0x35 -> "hector"
  0x38 -> "capcom"
  0x39 -> "banpresto"
  0x3C -> "*entertainment i"
  0x3E -> "gremlin"
  0x41 -> "ubi soft"
  0x42 -> "atlus"
  0x44 -> "malibu"
  0x46 -> "angel"
  0x47 -> "spectrum holoby"
  0x49 -> "irem"
  0x4A -> "virgin"
  0x4D -> "malibu"
  0x4F -> "u.s. gold"
  0x50 -> "absolute"
  0x51 -> "acclaim"
  0x52 -> "activision"
  0x53 -> "american sammy"
  0x54 -> "gametek"
  0x55 -> "park place"
  0x56 -> "ljn"
  0x57 -> "matchbox"
  0x59 -> "milton bradley"
  0x5A -> "mindscape"
  0x5B -> "romstar"
  0x5C -> "naxat soft"
  0x5D -> "tradewest"
  0x60 -> "titus"
  0x61 -> "virgin"
  0x67 -> "ocean"
  0x69 -> "electronic arts"
  0x6E -> "elite systems"
  0x6F -> "electro brain"
  0x70 -> "infogrames"
  0x71 -> "interplay"
  0x72 -> "broderbund"
  0x73 -> "sculptered soft"
  0x75 -> "the sales curve"
  0x78 -> "t*hq"
  0x79 -> "accolade"
  0x7A -> "triffix entertainment"
  0x7C -> "microprose"
  0x7F -> "kemco"
  0x80 -> "misawa entertainment"
  0x83 -> "lozc"
  0x86 -> "*tokuma shoten i"
  0x8B -> "bullet-proof software"
  0x8C -> "vic tokai"
  0x8E -> "ape"
  0x8F -> "i'max"
  0x91 -> "chun soft"
  0x92 -> "video system"
  0x93 -> "tsuburava"
  0x95 -> "varie"
  0x96 -> "yonezawa/s'pal"
  0x97 -> "kaneko"
  0x99 -> "arc"
  0x9A -> "nihon bussan"
  0x9B -> "tecmo"
  0x9C -> "imagineer"
  0x9D -> "banpresto"
  0x9F -> "nova"
  0xA1 -> "hori electric"
  0xA2 -> "bandai"
  0xA4 -> "konami"
  0xA6 -> "kawada"
  0xA7 -> "takara"
  0xA9 -> "technos japan"
  0xAA -> "broderbund"
  0xAC -> "toei animation"
  0xAD -> "toho"
  0xAF -> "namco"
  0xB0 -> "acclaim"
  0xB1 -> "ascii or nexoft"
  0xB2 -> "bandai"
  0xB4 -> "enix"
  0xB6 -> "hal"
  0xB7 -> "snk"
  0xB9 -> "pony canyon"
  0xBA -> "*culture brain o"
  0xBB -> "sunsoft"
  0xBD -> "sony imagesoft"
  0xBF -> "sammy"
  0xC0 -> "taito"
  0xC2 -> "kemco"
  0xC3 -> "squaresoft"
  0xC4 -> "*tokuma shoten i"
  0xC5 -> "data east"
  0xC6 -> "tonkin house"
  0xC8 -> "koei"
  0xC9 -> "ufl"
  0xCA -> "ultra"
  0xCB -> "vap"
  0xCC -> "use"
  0xCD -> "meldac"
  0xCE -> "*pony canyon or"
  0xCF -> "angel"
  0xD0 -> "taito"
  0xD1 -> "sofel"
  0xD2 -> "quest"
  0xD3 -> "sigma enterprises"
  0xD4 -> "ask kodansha"
  0xD6 -> "naxat soft"
  0xD7 -> "copya systems"
  0xD9 -> "banpresto"
  0xDA -> "tomy"
  0xDB -> "ljn"
  0xDD -> "ncs"
  0xDE -> "human"
  0xDF -> "altron"
  0xE0 -> "jaleco"
  0xE1 -> "towachiki"
  0xE2 -> "uutaka"
  0xE3 -> "varie"
  0xE5 -> "epoch"
  0xE7 -> "athena"
  0xE8 -> "asmik"
  0xE9 -> "natsume"
  0xEA -> "king records"
  0xEB -> "atlus"
  0xEC -> "epic/sony records"
  0xEE -> "igs"
  0xF0 -> "a wave"
  0xF3 -> "extreme entertainment"
  0xFF -> "ljn"
