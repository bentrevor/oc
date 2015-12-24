module OC.Checkbox where

import OC.Utils

import Data.List (find)
import Data.Text (replace)
import Data.Text.Internal (showText)

data MilTime = MilTime { hours :: Int, minutes :: Int } deriving (Eq)

instance Show MilTime where
  show time = hours' ++ ":" ++ minutes'
    where hours' = maybePadWithZero $ hours time
          minutes' = maybePadWithZero $ minutes time
          maybePadWithZero t
            | t < 10    = "0" ++ (show t)
            | otherwise = show t

data Checkbox = Checkbox { isChecked :: Bool
                         , description :: String
                         }
              deriving (Eq)

instance Show Checkbox where
  show checkbox = "  - [" ++ (checkboxMark checkbox) ++ "] " ++ (description checkbox)

data Habit = Habit { habitCB :: Checkbox
                   , streak :: Int
                   , details :: String -- maybe should be a Maybe String ?
                   }
           deriving (Eq)

instance Show Habit where
  show habit = showCheckboxLine (habitCB habit) maybeStreak maybeDetails
    where maybeStreak = Just (streak habit)
          maybeDetails = if length (details habit) == 0 then Nothing else Just (details habit)

data Bill = Bill { billCB :: Checkbox
                 , dueDate :: Int
                 }
          deriving (Eq)

instance Show Bill where
  show bill = showCheckboxLine (billCB bill) Nothing date
    where date = Just (show $ dueDate bill)

data Todo = Todo { todoCB :: Checkbox
                 , todoStart :: Maybe MilTime
                 , todoEnd :: Maybe MilTime
                 }
          deriving (Eq)

instance Show Todo where
  show todo = showCheckboxLine (todoCB todo) Nothing (getDesc todo)

strToMilTime :: String -> MilTime
strToMilTime str = MilTime { hours = hours', minutes = minutes' }
  where hours' = (read (strBefore ':' str)) :: Int
        minutes' = (read (strAfter ':' str)) :: Int

strToMilTimes :: String -> [MilTime]
strToMilTimes str = map strToMilTime timeStrs
  where timeStrs = splitOn ',' str

milTimeRangeStart :: String -> MilTime
milTimeRangeStart str = strToMilTime (splitOn '-' str !! 0)

milTimeRangeEnd :: String -> MilTime
milTimeRangeEnd str = strToMilTime (splitOn '-' str !! 1)

strToCheckbox :: String -> Checkbox
strToCheckbox str = Checkbox { description = stripWhitespace cbDesc
                             , isChecked = parseCheckboxChar str == 'X'
                             }
  where cbDesc
          | hasDetails str = strBetween descStart '{' str
          | otherwise      = strAfter descStart str
        descStart = if hasStreak str then ')' else ']'
        stripWhitespace s = reverse $ dropSpaces $ reverse $ dropSpaces s
        dropSpaces = dropWhile (== ' ')

strToHabit :: String -> Habit
strToHabit str = Habit { habitCB = strToCheckbox str
                       , details = parseDetails str
                       , streak = parseStreak str
                       }

strToBill :: String -> Bill
strToBill str = Bill { billCB = strToCheckbox str
                     , dueDate = read $ parseDetails str
                     }

strToTodo :: String -> Todo
strToTodo str = Todo { todoCB = strToCheckbox str
                     , todoStart = startTime
                     , todoEnd = endTime
                     }
  where (startTime, endTime) = case (hasDetails str, numDashes details) of
                                (False, _) -> (Nothing, Nothing)
                                (_, 0)     -> (Just (strToMilTime details), Nothing)
                                (_, 1)     -> (Just (milTimeRangeStart details), Just (milTimeRangeEnd details))
                                (_, _)     -> error "shouldn't get here"
        details = parseDetails str
        numDashes s = length $ filter (=='-') s

getDesc :: Todo -> Maybe String
getDesc todo = case (todoStart todo, todoEnd todo) of
                (Nothing, Nothing) -> Nothing
                (Just t, Nothing) -> Just (show t)
                (Just t1, Just t2) -> Just (show t1 ++ "-" ++ show t2)
                (Nothing, Just _) -> error "shouldn't have an end time without a start time"

showCheckboxLine :: Checkbox -> Maybe Int -> Maybe String -> String
showCheckboxLine checkbox streak desc = (addDesc . addStreak) checkboxLine
  where checkboxLine = unwords ["  -", "[" ++ maybeX ++ "]", description checkbox]
        maybeX = checkboxMark checkbox
        addStreak = case streak of
                     Nothing -> id
                     Just s -> (\_ -> (strBefore ']' checkboxLine) ++ "] (" ++ (show s) ++ ")" ++ (strAfter ']' checkboxLine))
        addDesc = case desc of
                   Nothing -> id
                   Just d -> (++ " {" ++ d ++ "}")

checkboxMark :: Checkbox -> String
checkboxMark checkbox = if (isChecked checkbox) then "X" else " "

hasDetails :: String -> Bool
hasDetails = elem '{'

hasStreak :: String -> Bool
hasStreak = elem '('

parseCheckboxChar :: String -> Char
parseCheckboxChar = charAfter '['

parseDetails :: String -> String
parseDetails = strBetween '{' '}'

parseStreak :: String -> Int
parseStreak str = read $ strBetween '(' ')' str
