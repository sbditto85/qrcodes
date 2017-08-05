{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( encodedToSecret
    , getTotp
    , lvtTotpParams
    , secretToEncoded
    , verify
    , testItOut
    ) where

import           Codec.Binary.QRCode
import           Crypto.Hash.Algorithms
import qualified Crypto.Nonce               as N
import           Crypto.OTP
import           Data.Array                 (Array)
import qualified Data.Base32.Encode         as Base32
-- import qualified Data.ByteString.Base32     as Base32
import qualified Data.Bits                  as BI
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import           Data.Colour.Names          (black, white)
import           Data.Function              ((&))
import           Data.List                  (foldl')
import           Data.Maybe                 (fromJust)
import           Data.Monoid
import qualified Data.QRCode                as QR
import           Data.Time.Clock.POSIX
import qualified Data.Vector.Storable       as VS
import           Data.Word
import           Diagrams.Backend.SVG
import           Diagrams.Core.Types
import           Diagrams.Path
import           Diagrams.QRCode
import           Diagrams.Size
import           Diagrams.TwoD.Types
import qualified Diagrams.Attributes        as D
import qualified Diagrams.Core              as D
import qualified Diagrams.Located           as D
import qualified Diagrams.Path              as D
import qualified Diagrams.Trail             as D
import qualified Diagrams.TwoD              as D

testItOut mToTest = do
  rando <- randomSecret
  let encoded = secretToEncoded rando
      encodedStr = "otpauth://totp/LVTAUTH:demo@liveviewtech.com?secret=" ++ (BSC.unpack encoded) ++ "&issuer=LVT"
      randoStr = BSC.unpack rando
      matrix = fromJust $ encodeQRCode $ maybe encodedStr (BSC.unpack . secretToEncoded . BSC.pack) mToTest
      dData = diagramsData matrix
      svg = dData `seq` toSVG dData
  putStrLn $ maybe randoStr id mToTest
  putStrLn $ maybe
    encodedStr
    (BSC.unpack . secretToEncoded . BSC.pack)
    mToTest
  renderToFile svg

testItOut' mToTest = do
  rando <- randomSecret
  let encoded = secretToEncoded rando
      encodedStr = "otpauth://totp/LVTAUTH:demo@liveviewtech.com?secret=" ++ (BSC.unpack encoded) ++ "&issuer=LVT"
      randoStr = BSC.unpack rando
      qrString = maybe encodedStr (BSC.unpack . secretToEncoded . BSC.pack) mToTest
      version = Nothing
      caseSensative = True
  qrCode <- QR.encodeString qrString version QR.QR_ECLEVEL_L QR.QR_MODE_EIGHT caseSensative
  let matrix = QR.toMatrix qrCode
      dData = pathMatrix matrix
      svg = dData `seq` stroke dData
  putStrLn $ maybe randoStr id mToTest
  putStrLn $ maybe
    encodedStr
    (BSC.unpack . secretToEncoded . BSC.pack)
    mToTest
  renderToFile svg

otherSecret =  BSC.pack "bobloblaw!"
googleSecret = BSC.pack "Hello!\222\173\190\239"

randomSecret :: IO BSC.ByteString
randomSecret = do
  g <- N.new
  N.nonce128url g


encodeQRCode :: String -> Maybe Matrix
encodeQRCode toEncode =
  case version $ selectVersionNum (length toEncode) of
    Nothing ->
      error "ahhh"
    Just v ->
      let
        errorLevel = L
        numOrAlpha = EightBit
      in
        encode v errorLevel numOrAlpha toEncode
  where
    selectVersionNum len
      -- FOR L
      | len < 25 = 1
      | len < 47 = 2
      | len < 77 = 3
      | len < 114 = 4
      | len < 154 = 5
      | len < 195 = 6
      | len < 224 = 7
      | len < 279 = 8
      | len < 335 = 9
      | len < 395 = 10
      | otherwise = 10

      -- FOR M
      {-
      | len < 20 = 1
      | len < 38 = 2
      | len < 61 = 3
      | len < 90 = 4
      | len < 122 = 5
      | len < 154 = 6
      | len < 178 = 7
      | len < 221 = 8
      | len < 262 = 9
      | len < 311 = 10
      | otherwise = 10
      -}

      -- FOR Q
      {-
      | len < 16 = 1
      | len < 29 = 2
      | len < 47 = 3
      | len < 67 = 4
      | len < 87 = 5
      | len < 108 = 6
      | len < 125 = 7
      | len < 157 = 8
      | len < 189 = 9
      | len < 221 = 10
      | otherwise = 10
      -}


diagramsData :: Matrix -> Path V2 Double
diagramsData matrix =
  let
    arrData :: Array (Int, Int) Int
    arrData = toArray matrix
  in
    pathArray arrData

-- | Stroke using default QR code colors (black on white) and
-- with the \"quiet\" region.
stroke' :: (D.Backend b D.V2 Double, D.Renderable (D.Path D.V2 Double) b)
          => D.Path D.V2 Double
          -> D.QDiagram b D.V2 Double Any
stroke' = D.bg black . D.fc white . D.lw D.none . D.stroke
  where
    zoneX = D.strutX 4
    zoneY = D.strutY 4
    quiet d =
                  zoneY
                  D.===
       (zoneX D.||| d D.||| zoneX)
                  D.===
                  zoneY

toSVG :: Path V2 Double -> QDiagram SVG V2 Double Any
toSVG dData =
  stroke' dData

renderToFile :: QDiagram SVG V2 Double Any -> IO ()
renderToFile svgData =
  renderSVG "myqrcode.svg" absolute svgData

getTotp secret = do
  curPosTime <- getPOSIXTime
  let seconds = floor curPosTime
      params = lvtTotpParams
      t = totp params secret seconds
  pure t

lvtTotpParams =
  let
    countFrom = 0
    timeStep = 30
    digits = OTP6
    algo = SHA1
    skew = OneStep
  in
    case mkTOTPParams algo countFrom timeStep digits skew of
      Left _ ->
        error "ahhh"
      Right params ->
        params

verify secret totp = do
  curPosTime <- getPOSIXTime
  let seconds = floor curPosTime
      params = lvtTotpParams
      isGood = totpVerify params secret seconds totp
  pure isGood


secretToEncoded :: BS.ByteString -> BS.ByteString
secretToEncoded secret = BSC.filter (/='=') $ Base32.encode secret

encodedToSecret = error "this doesn't work yet"
