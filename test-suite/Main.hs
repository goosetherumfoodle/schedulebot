{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Test.Tasty
import Test.Tasty.Hspec
import Data.Aeson (decode)
import Data.Yaml (decodeEither')
import Data.Yaml.Internal
import Text.RawString.QQ


import Example

main :: IO ()
main = do
    test <- testSpec "scheduler-fp" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  describe "parsing" $ do
    it "GCalEvent" $ do
      let json = "{\"summary\": \"this is the summary\", \"description\": \"good description\", \"start\": {\"dateTime\": \"start datetime\"}, \"end\": {\"dateTime\": \"end datetime\"}}"
          expected = (GCalEvent {gCalSummary = "this is the summary", gCalDesc = Just "good description", gCalStart = GCalDateTime "start datetime", gCalEnd = GCalDateTime "end datetime"})
          decoded = decode json
      decoded `shouldBe` Just expected

    it "ShiftWeek RawShiftTime" $ do
      let input = [r|
monday:
        - ["10:30", "16:00", "first"]
        - ["16:00", "21:00", "second"]
tuesday:
        - ["10:30", "16:00", "third"]
wednesday:
        - ["6:30", "16:00"]
thursday:
        - ["10:30", "16:00"]
friday:
        - ["10:30", "16:00"]
saturday: []
sunday:
        - ["10:30", "16:00"]
|]
          expected = Right $ ShiftW
            {
              wkMon =
              [
                Period {
                  periodName = Just "first"
                  , periodStart = RawShiftTime 10 30
                  , periodEnd = RawShiftTime 16 00
                  }
              , Period {
                  periodName = Just "second"
                  , periodStart = RawShiftTime 16 00
                  , periodEnd = RawShiftTime 21 00
                  }

              ]
            , wkTue =
              [
                Period {
                  periodName = Just "third"
                  , periodStart = RawShiftTime 10 30
                  , periodEnd = RawShiftTime 16 00
                  }
              ]
            , wkWed =
              [
                Period {
                  periodName = Nothing
                  , periodStart = RawShiftTime 6 30
                  , periodEnd = RawShiftTime 16 00
                  }
              ]
            , wkThu =
              [
                Period {
                  periodName = Nothing
                  , periodStart = RawShiftTime 10 30
                  , periodEnd = RawShiftTime 16 00
                  }
              ]
            , wkFri =
              [
                Period {
                  periodName = Nothing
                  , periodStart = RawShiftTime 10 30
                  , periodEnd = RawShiftTime 16 00
                  }
              ]
            , wkSat = []
            , wkSun =
              [
                Period {
                  periodName = Nothing
                  , periodStart = RawShiftTime 10 30
                  , periodEnd = RawShiftTime 16 00
                  }
              ]
            }

          decoded = decodeEither' input :: Either ParseException (ShiftWeek RawShiftTime)
      decoded `shouldBe` expected


-- helpers

instance Eq Data.Yaml.Internal.ParseException where
  _ == _ = False
