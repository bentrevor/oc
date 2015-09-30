module OC.CheckboxSpec where

import OC.Checkbox
import Test.Hspec

main :: IO ()
main = hspec spec

habitA = Habit { habitCB = Checkbox { heading = "habit A" , isChecked = True }
               , streak = (2)
               , details = ""
               }

habitB = Habit { habitCB = Checkbox { heading = "habit B" , isChecked = False }
               , streak = (-2)
               , details = "habit details"
               }

todoA = Todo { todoCB = Checkbox { heading = "todo A", isChecked = True }
             , todoStart = Nothing
             , todoEnd = Nothing
             }

todoB = Todo { todoCB = Checkbox { heading = "todo B", isChecked = True }
             , todoStart = Just MilTime { hours = 8, minutes = 0 }
             , todoEnd = Nothing
             }


todoC = Todo { todoCB = Checkbox { heading = "todo C", isChecked = False }
             , todoStart = Just MilTime { hours = 14, minutes = 0 }
             , todoEnd = Just MilTime { hours = 15, minutes = 30 }
             }

billA = Bill { billCB = Checkbox { heading = "bill A"
                                 , isChecked = True
                                 }
             , dueDate = 15
             }

billB = Bill { billCB = Checkbox { heading = "bill B"
                                 , isChecked = False
                                 }
             , dueDate = 20
             }

spec :: Spec
spec = do
  describe "MilTime" $ do
    it "can be printed" $ do
      show (MilTime { hours = 5, minutes = 5 }) `shouldBe` "05:05"

    it "prints time after midnight as > 25:00" $ do
      show (MilTime { hours = 25, minutes = 30 }) `shouldBe` "25:30"

    it "can be built from a string" $ do
      strToMilTime "05:00" `shouldBe` MilTime { hours = 5, minutes = 0 }
      strToMilTimes "05:00" `shouldBe` [MilTime { hours = 5, minutes = 0 }]
      strToMilTimes "05:00,15:00" `shouldBe` [MilTime { hours = 5, minutes = 0 }, MilTime { hours = 15, minutes = 0 }]

  describe "Checkbox" $ do
    it "can be shown" $ do
      show Checkbox { heading = "checkbox heading", isChecked = False } `shouldBe` "  - [ ] checkbox heading"
      show Checkbox { heading = "checkbox heading", isChecked = True } `shouldBe` "  - [X] checkbox heading"

    it "showCheckboxLine" $ do
      showCheckboxLine (Checkbox { heading = "checkbox heading", isChecked = True }) Nothing Nothing `shouldBe` "  - [X] checkbox heading"
      showCheckboxLine (Checkbox { heading = "checkbox heading", isChecked = False }) (Just 1) Nothing `shouldBe` "  - [ ] (1) checkbox heading"
      showCheckboxLine (Checkbox { heading = "checkbox heading", isChecked = True }) Nothing (Just "asdf") `shouldBe` "  - [X] checkbox heading {asdf}"
      showCheckboxLine (Checkbox { heading = "checkbox heading", isChecked = True }) (Just (-5)) (Just "asdf") `shouldBe` "  - [X] (-5) checkbox heading {asdf}"

    it "can be built from a string" $ do
      heading (strToCheckbox "  - [ ] (1) habit name") `shouldBe` "habit name"
      heading (strToCheckbox "  - [ ] (1) habit name {with details}") `shouldBe` "habit name"
      strToCheckbox "  - [ ] (1) habit name {with details}" `shouldBe` Checkbox { heading = "habit name", isChecked = False }
      strToCheckbox "  - [X] habit name" `shouldBe` Checkbox { heading = "habit name", isChecked = True }

  describe "types of checkboxes" $ do
    describe "Habit" $ do
      it "can be shown" $ do
        show habitA `shouldBe` "  - [X] (2) habit A"
        show habitB `shouldBe` "  - [ ] (-2) habit B {habit details}"

      it "can be built from a string" $ do
        strToHabit "  - [X] (2) habit A" `shouldBe` habitA
        strToHabit "  - [ ] (-2) habit B {habit details}" `shouldBe` habitB

    describe "Bill" $ do
      it "can be shown" $ do
        show billA `shouldBe` "  - [X] bill A {15}"
        show billB `shouldBe` "  - [ ] bill B {20}"

      it "can be built from a string" $ do
        strToBill "  - [X] bill A {15}" `shouldBe` billA
        strToBill "  - [ ] bill B {20}" `shouldBe` billB

    describe "Todo" $ do
      it "can be shown" $ do
        show todoA `shouldBe` "  - [X] todo A"
        show todoB `shouldBe` "  - [X] todo B {08:00}"
        show todoC `shouldBe` "  - [ ] todo C {14:00-15:30}"

      it "can be built from a string" $ do
        strToTodo "  - [X] todo A" `shouldBe` todoA
        strToTodo "  - [X] todo B {08:00}" `shouldBe` todoB
        strToTodo "  - [ ] todo C {14:00-15:30}" `shouldBe` todoC
