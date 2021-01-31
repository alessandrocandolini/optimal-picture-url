{-# LANGUAGE DerivingVia #-}

module OptimalImageSelector
  ( Size (..),
    PictureHeight (..),
    PictureWidth (..),
    PictureUrl (..),
    PictureData (..),
    Picture (..),
    values,
    choosePicture,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Map (Map)
import Helpers (values)

-- TODO add refined types to sizes?

newtype Size = Size Int
  deriving (Eq, Show)
  deriving (Num, Ord) via Int

newtype PictureHeight = PictureHeight Size
  deriving (Eq, Show)
  deriving (Num, Ord) via Size

newtype PictureWidth = PictureWidth Size
  deriving (Eq, Show)
  deriving (Num, Ord) via Size

newtype PictureUrl = PictureUrl String
  deriving (Eq, Show)

newtype PictureData = PictureData
  { formats :: Map String Picture
  }
  deriving (Eq, Show)

data Picture = Picture
  { height :: PictureHeight,
    width :: PictureWidth,
    url :: PictureUrl
  }
  deriving (Eq, Show)

choosePicture :: PictureWidth -> PictureData -> Maybe Picture
choosePicture w (PictureData f) = closestPoint width w (values f)

closestPoint :: (Num w, Ord w) => (a -> w) -> w -> [a] -> Maybe a
closestPoint t w = fmap (closestPointNonEmpty t w) . N.nonEmpty

closestPointNonEmpty :: (Num w, Ord w) => (a -> w) -> w -> NonEmpty a -> a
closestPointNonEmpty t w = N.head . N.sortWith (distance t w)
  where
    distance t' w' = abs . (w' -) . t'
