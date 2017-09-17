{-
Resources:
  cassava tutorial:
  https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/
  gloss tutorial:
  http://andrew.gibiansky.com/blog/haskell/haskell-gloss/

-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass, GADTSyntax #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Main where

-- package for drawing
import Codec.Picture
import Codec.Picture.Types
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

-- package for loading database
-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

-- cassava
import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.:)
  , (.=)
  )
import qualified Data.Csv as Cassava

-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- vector
import Data.Vector hiding ( (++) )
import qualified Data.Vector as Vector

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data Ramen where
  Ramen :: { longitude :: Float
            , latitude :: Float
            , rating :: Float } -> Ramen
  deriving (Show, Eq)

instance FromNamedRecord Ramen where
  parseNamedRecord m =
    Ramen
    --  <$> fmap Text.decodeLatin1 (m .: "name")
      <$> m .: "longitude"
      <*> m .: "latitude"
      <*> m .: "rating"

decodeItems :: ByteString -> Either String (Vector.Vector Ramen)
decodeItems = fmap snd . Cassava.decodeByName

-- draw

file :: FilePath
file = "./world_map.jpg"

window :: Display
window = FullScreen

-- width of the picture
sizeX :: Float
sizeX = 600.0

-- height of the picture
sizeY :: Float
sizeY = 263.0

circleR :: Float
circleR = 2.5

circleRF :: Float
circleRF = 3.5

background :: Color
background = black

-- scale image
image :: Picture -> Picture
image p = scale 0.5 0.45 p

getColor :: Float -> Color
getColor rating
  |rating == 0.0 = withAlpha 0.5 rose
  |rating == 4.5 = makeColorI 255 255 0 140
  |rating < 4.5 = makeColorI 255 (round $ 60 * (rating - 1)) 0 180
  |rating > 4.5 = makeColorI (round $ 510 * (rating - 4.5)) 255 0 180
  |otherwise = black

createCircle :: Color -> Picture
createCircle c = color c $ circleSolid circleR

createCirFinal :: Color -> Picture
createCirFinal c = color (bright c) $ circleSolid circleRF

drawCircle :: Ramen -> Picture -> Picture
drawCircle ramen circ = case (latitude ramen) > 0 of
  True  -> translate (sizeX * ((longitude ramen) - 5)/ 190) (sizeY * ((latitude ramen) + 8) / 95) circ
  False -> translate (sizeX * ((longitude ramen) + 5) / 190) (sizeY * ((latitude ramen) + 5) / 110) circ

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ n) = 1 + toInt n

toIndex :: Float -> Int
toIndex f = round (f / 0.01)

toNat :: Int -> Nat
toNat 0 = Zero
toNat a = Succ (toNat $ a - 1)

minus :: Nat -> Nat -> Nat
minus Zero Zero = Zero
minus a Zero = a
minus Zero b = Zero
minus (Succ a) (Succ b) = minus a b

-- add new dot to existing pictures
-- (!?) :: Vector a -> Int -> Maybe a    Safe Indexing
getPictureList :: Vector.Vector Ramen -> Nat -> [Picture]
getPictureList vRamen a
  = case (vRamen !? (toInt a)) of
      Just v -> drawCircle v (createCircle $ getColor $ rating v) : getPictureList vRamen (Succ a)
      Nothing -> []

-- update the picture
updatePic :: Vector.Vector Ramen -> Float -> Picture
updatePic vRamen seconds
  = case (vRamen !? (toIndex seconds)) of
    Just v -> pictures $ getPictureList vRamen (minus (toNat (Vector.length vRamen)) (toNat (toIndex seconds)))
    Nothing -> pictures $ getPictureList vRamen Zero


main :: IO ()
main = do
  putStrLn "Enter the name of the database"
  dataFile <- getLine
  maybeImage <- loadJuicyJPG file
  case maybeImage of
    Nothing -> putStrLn "error loading image"
    Just img -> do
      d <- ByteString.readFile dataFile
      let v = decodeItems d
      case v of
        Left err -> putStrLn "error readin file"
        Right v -> animate window black (renderPic img v)
  where
    renderPic :: Picture -> Vector.Vector Ramen -> Float -> Picture
    renderPic pic vRamen time = pictures $ (image pic : [updatePic vRamen time])
