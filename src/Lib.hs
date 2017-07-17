{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( encodedToSecret
    , getTotp
    , lvtTotpParams
    , secretToEncoded
    , verify
    ) where

import           Data.Array                 (Array)
import           Data.Base32String.Default
import qualified Data.ByteString.Char8      as BC
import           Data.Colour.Names          (black, white)
import           Data.Maybe                 (fromJust)
import           Data.Monoid
import           Data.Time.Clock.POSIX
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
import           Codec.Binary.QRCode
import           Crypto.Hash.Algorithms
import qualified Crypto.Nonce               as N
import           Crypto.OTP

testItOut = do
  let matrix = fromJust $ encodeQRCode
      dData = diagramsData matrix
      svg = dData `seq` toSVG dData
  renderToFile svg

otherSecret =  BC.pack "bobloblaw!"
googleSecret = BC.pack "Hello!\222\173\190\239"

randomSecret :: IO BC.ByteString
randomSecret = do
  g <- N.new
  N.nonce128 g


encodeQRCode :: Maybe Matrix
encodeQRCode =
  case version 1 of
    Nothing ->
      error "ahhh"
    Just v ->
      let
        errorLevel = Q
        numOrAlpha = Alphanumeric
      in
        encode v errorLevel numOrAlpha "bobloblaw"


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
    skew = TwoSteps
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


secretToEncoded = fromBytes

encodedToSecret = toBytes
