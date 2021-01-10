module OptimalImageSelectorSpec where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import OptimalImageSelector
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Property

-- TODO orphan instances

instance Arbitrary Picture where
  arbitrary = pictureGen

instance Arbitrary Size where
  arbitrary = Size <$> arbitrary

instance Arbitrary PictureWidth where
  arbitrary = PictureWidth <$> arbitrary

instance Arbitrary PictureHeight where
  arbitrary = PictureHeight <$> arbitrary

instance Arbitrary PictureUrl where
  arbitrary = PictureUrl <$> arbitrary

pictureGen :: Gen Picture
pictureGen =
  Picture <$> arbitrary <*> arbitrary <*> arbitrary

forAllValues :: (a -> Bool) -> Map k a -> Bool
forAllValues p = all p . values

isNotEmpty :: Map k a -> Bool
isNotEmpty = not . M.null

spec :: Spec
spec =
  describe "choosePicture" $ do
    prop "should return no picture IF no picture is provided as input" $
      \w -> isNothing (choosePicture w (PictureData M.empty))

    prop "should return no picture ONLY IF there are no input pictures (ie, always return somethign when there are input pictures)" $
      \w m -> isNotEmpty m ==> isJust (choosePicture w (PictureData m))

    prop "should always return the picture matching the desired width, whenever there is one and only one picture in the input that matches the expected size" $
      \w h u k m ->
        forAllValues ((w /=) . width) m
          ==> let p = Picture h w u
                  m' = M.insert k p m
               in choosePicture w (PictureData m') == Just p

    prop "should return the picture with the size closer to the desire size" $
      \w m ->
        isNotEmpty m
          ==> let distance = abs . (w -) . width
                  p = fromJust $ choosePicture w (PictureData m)
                  d = distance p
               in (all (>= d) . fmap distance . values) m
