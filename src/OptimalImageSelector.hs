module OptimalImageSelector where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Map (Map)
import qualified Data.Map as M

data PictureData = PictureData
  { id :: Integer,
    formats :: Map String Picture
  }
  deriving (Eq, Show)

data Picture = Picture
  { height :: Size,
    width :: Size,
    url :: Url
  }
  deriving (Eq, Show)

type Url = String

type Size = Int

mapValues :: Map k a -> [a]
mapValues m = fmap snd (M.toList m)

mapToNonEmpty :: Map k a -> Maybe (NonEmpty a)
mapToNonEmpty = N.nonEmpty . mapValues

choosePicture :: Size -> PictureData -> Maybe Picture
choosePicture w (PictureData _ m) = fmap (choosePictureAlg w) (mapToNonEmpty m)

choosePictureAlg :: Size -> NonEmpty Picture -> Picture
choosePictureAlg _ = N.head
