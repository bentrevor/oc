module OC.OrgDaySpec where

import OC.OrgDay
import OC.Checkbox
import qualified Data.Time.Calendar as Cal
import Data.Ratio
import Test.Hspec

main :: IO ()
main = hspec spec

event = Event { name = "some event with end time"
              , startTime = MilTime { hours = 12, minutes = 0 }
              , endTime = Just (MilTime { hours = 14, minutes = 30 })
              }

endlessEvent = Event { name = "some event"
              , startTime = MilTime { hours = 12, minutes = 0 }
              , endTime = Nothing
              }

sundayTodo = Todo { todoCB = Checkbox { isChecked = True, description = "do something" }
            , todoStart = Nothing, todoEnd = Nothing
            }

unfinishedSundayTodo = Todo { todoCB = Checkbox { isChecked = False, description = "another thing to do" }
                            , todoStart = Nothing, todoEnd = Nothing
                            }

breakfastHabit = Habit { habitCB = Checkbox { isChecked = True, description = "breakfast" }
                       , streak = -2, details = "yummy"
                       }

sundayHabits = [ breakfastHabit
               , Habit { habitCB = Checkbox { isChecked = False, description = "lunch" }
                       , streak = -1, details = ""
                       }
               , Habit { habitCB = Checkbox { isChecked = True, description = "dinner" }
                       , streak = 1, details = ""
                       }
               , Habit { habitCB = Checkbox { isChecked = False, description = "dessert" }
                       , streak = 2, details = ""
                       }
               ]

sunday :: Cal.Day
sunday = Cal.fromGregorian 2015 9 13

monday :: Cal.Day
monday = Cal.fromGregorian 2015 9 14

orgSunday = Day { date = sunday
                , mood = [Mood (8, strToMilTime "07:45", "woke up"), Mood (7, strToMilTime "12:15", "ate lunch, cleaned kitchen, napped"), Mood (8, strToMilTime "22:00", "went to bed")]
                , pre = "good morning"
                , post = "good night"
                , todos = [sundayTodo, unfinishedSundayTodo]
                , calendar = [event, endlessEvent]
                , habits = sundayHabits
                }

orgMonday = nextDay orgSunday

orgFileString = unlines ["* September"
                        , "** [1/2] bills"
                        , "- [X] rent {15}"
                        , "- [ ] car {25}"
                        , ""
                        , "** <2015-09-13 Sun>-<2015-09-20 Sun>"
                        , sundayString
                        ]

sundayString = unlines $ ["*** [3/6] <2015-09-13 Sun>"
                         , ":MOOD: 8(07:45__woke up);7(12:15__ate lunch, cleaned kitchen, napped);8(22:00__went to bed)"
                         , ":PRE: good morning"
                         , ":POST: good night"
                         , ""
                         , ":TODO: _"
                         , show sundayTodo
                         , show unfinishedSundayTodo
                         , ":CALENDAR: _"
                         , show event
                         , show endlessEvent
                         , ":HABITS: _"
                         ] ++ map show sundayHabits

spec :: Spec
spec = do
  describe "Event" $ do
    it "can be shown" $ do
      show event `shouldBe` "  - some event with end time {12:00-14:30}"
      show endlessEvent `shouldBe` "  - some event {12:00}"

    it "can be built from a string" $ do
      strToEvent "  - some event with end time {12:00-14:30}" `shouldBe` event
      strToEvent "  - some event {12:00}" `shouldBe` endlessEvent

  context "reading from file" $ do
    specify "getLastDay returns the last subtree in the file" $ do
      getLastDay orgFileString `shouldBe` sundayString

  context "converting string to OrgDay" $ do
    describe "properties" $ do
      it "can get the string for a single-line property" $ do
        parseProperty "mood" sundayString `shouldBe` "8(07:45__woke up);7(12:15__ate lunch, cleaned kitchen, napped);8(22:00__went to bed)"
        parseProperty "bogus" sundayString `shouldBe` "" -- should be Nothing

      it "can get the list of strings for multiline properties" $ do
        parsePropertyList "todo" sundayString `shouldBe` [show sundayTodo, show unfinishedSundayTodo]
        parsePropertyList "calendar" sundayString `shouldBe` [show event, show endlessEvent]
        parsePropertyList "habits" sundayString `shouldBe` map show sundayHabits

      describe "mood" $ do
        describe "instance Show" $ do
          it "is in the format `$mood($time__$desc)`" $ do
            show (Mood (1, MilTime { hours = 7, minutes = 45 }, "ballin'")) `shouldBe` "1(07:45__ballin')"

        -- the semicolons are just so I can use commas in my descriptions without breaking the
        -- splitOn in parseMood
        it "is a (semicolon-separated) list of integers with a time and description" $ do
          moodFromStr "8(07:45__yolo, swag)" `shouldBe` Mood (8, MilTime { hours = 7, minutes = 45 }, "yolo, swag")
          parseMood sundayString `shouldBe` [ Mood (8, MilTime { hours = 7, minutes = 45 }, "woke up")
                                            , Mood (7, MilTime { hours = 12, minutes = 15 }, "ate lunch, cleaned kitchen, napped")
                                            , Mood (8, MilTime { hours = 22, minutes = 0 }, "went to bed")
                                            ]

      describe "pre/post" $ do
        it "is just a string" $ do
          parseProperty "pre" sundayString `shouldBe` "good morning"
          parseProperty "post" sundayString `shouldBe` "good night"

      describe "todos" $ do
        it "is a list of Checkboxes" $ do
          parseTodos sundayString `shouldBe` [sundayTodo, unfinishedSundayTodo]

      describe "calendar" $ do
        it "is a list of Events" $ do
          parseCalendar sundayString `shouldBe` [event, endlessEvent]

    describe "strToDay" $ do
      it "parses all properties" $ do
        strToDay sundayString `shouldBe` orgSunday

  context "converting OrgDay to tomorrow's OrgDay" $ do
    describe "nextDay" $ do
      it "updates the date" $ do
        date orgMonday `shouldBe` monday

      it "has empty properties" $ do
        mood orgMonday `shouldBe` []
        pre orgMonday `shouldBe` "_"
        post orgMonday `shouldBe` "_"

      it "keeps the events from yesterday" $ do
        calendar orgMonday `shouldBe` calendar orgSunday

      it "keeps incomplete todos from yesterday" $ do
        todos orgMonday `shouldBe` [unfinishedSundayTodo]

      describe "updating habits" $ do
        it "clears the details" $ do
          details (nextHabit breakfastHabit) `shouldBe` ""

        it "updates the streak" $ do
          streak (nextHabit (sundayHabits !! 0)) `shouldBe` 1
          streak (nextHabit (sundayHabits !! 1)) `shouldBe` -2
          streak (nextHabit (sundayHabits !! 2)) `shouldBe` 2
          streak (nextHabit (sundayHabits !! 3)) `shouldBe` -1

        it "resets the checkbox mark to a space" $ do
          isChecked (habitCB (nextHabit breakfastHabit)) `shouldBe` False
          any (isChecked . habitCB) (habits orgMonday) `shouldBe` False

  context "converting OrgDay back into string" $ do
    it "can be converted into a string" $ do
      show orgSunday `shouldBe` sundayString
