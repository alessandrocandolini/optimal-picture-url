module OptimalImageSelectorSpec where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import OptimalImageSelector
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property

instance Arbitrary Picture where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Picture a b c)

spec :: Spec
spec =
  describe "choosePicture" $ do
    it "should return Nothing if there are no pictures" $
      property $
        \w id -> isNothing (choosePicture w (PictureData id M.empty))

    it "should always return something if there are input pictures" $
      property $
        \w m id -> (not . null) m ==> isJust (choosePicture w (PictureData id m))
