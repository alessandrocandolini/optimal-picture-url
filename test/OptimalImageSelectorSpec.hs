module OptimalImageSelectorSpec where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import OptimalImageSelector
import Test.Hspec
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

allValues :: (a -> Bool) -> Map k a -> Bool
allValues p = all p . values

spec :: Spec
spec =
  describe "choosePicture" $ do
    it "should return no picture if no picture is provided as input" $
      property $
        \w pId -> isNothing (choosePicture w (PictureData pId M.empty))

    it "should return no picture only if there are no input pictures (ie, always return a picture when there are input pictures)" $
      property $
        \w m pId -> (not . null) m ==> isJust (choosePicture w (PictureData pId m))

    it "should always return the picture matching the width, whenever there is one and only one picture in the input that matches the expected size" $
      property $
        \w h u k m pId ->
          allValues ((w /=) . width) m
            ==> let p = Picture h w u
                    m' = M.insert k p m
                 in choosePicture w (PictureData pId m') == Just p

    it "should return the picture with the size closer to the desire size" $
      property $
        \w m pId ->
          let p = choosePicture w (PictureData pId m)
           in isJust p
                ==> let distance = abs . (-) w . width
                        p' = fromJust p -- todo unsafe runtime cast
                        d = distance p'
                     in (not . any ((< d) . distance) . filter (p' /=)) (values m)
