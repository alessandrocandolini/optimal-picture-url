{-# LANGUAGE DerivingVia #-}

module OptimalImageSelector where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Map (Map)
import qualified Data.Map as M

-- TODO add refined types to sizes

newtype Size = Size Int
  deriving (Eq, Show)
  deriving (Num, Ord) via Int

newtype PictureHeight = PictureHeight Size
  deriving (Eq, Show)
  deriving (Num, Ord) via Size

newtype PictureWidth = PictureWidth Size
  deriving (Eq, Show)
  deriving (Num, Ord) via Size

newtype PictureUrl = PictureUrl String deriving (Eq, Show)

data PictureData = PictureData
  { id :: Integer,
    formats :: Map String Picture
  }
  deriving (Eq, Show)

data Picture = Picture
  { height :: PictureHeight,
    width :: PictureWidth,
    url :: PictureUrl
  }
  deriving (Eq, Show)

values :: Map k a -> [a]
values = fmap snd . M.toList

choosePicture :: PictureWidth -> PictureData -> Maybe Picture
choosePicture w (PictureData _ m) = choosePictureFromList w (values m)

choosePictureFromList :: PictureWidth -> [Picture] -> Maybe Picture
choosePictureFromList w l = choosePictureFromNonEmpty w <$> N.nonEmpty l

choosePictureFromNonEmpty :: PictureWidth -> NonEmpty Picture -> Picture
choosePictureFromNonEmpty w l = N.head (N.map fst (N.sortBy g (N.map f l)))
  where
    f p@(Picture _ w' _) = (p, abs (w' - w))
    g (_, d) (_, d') = compare d d'
