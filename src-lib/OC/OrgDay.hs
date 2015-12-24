{-# LANGUAGE FlexibleInstances,OverlappingInstances #-}
-- TODO figure out what OverlappingInstances is

module OC.OrgDay where

import OC.Checkbox
import OC.Utils

import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Calendar.WeekDate as WD
import qualified Data.Time.Clock as Clk
import Data.Char (digitToInt, toUpper)
import Data.List (find, intercalate, isPrefixOf)
import Data.String.Utils
import qualified Data.Text as DT
import Data.Ratio

data Month = January | February | March | April | May | June | July
           | August | September | October | November | December
           deriving (Eq, Show)

data Event = Event { name :: String
                   , startTime :: MilTime
                   , endTime :: Maybe MilTime
                   }
           deriving (Eq)

instance Show Event where
  show event = "  - " ++ (name event) ++ " {" ++ (show $ startTime event) ++ endTime' ++ "}"
    where endTime' = case endTime event of
                      Just time -> "-" ++ show time
                      Nothing -> ""

strToEvent :: String -> Event
strToEvent str = Event { name = init $ strBefore '{' $ drop 4 str
                       , startTime = start
                       , endTime = end
                       }
  where timeStr = strBetween '{' '}' str
        (start, end) = case find (=='-') timeStr of
                        Nothing -> (strToMilTime timeStr, Nothing)
                        Just _ -> (strToMilTime (take 5 timeStr), Just (strToMilTime (drop 6 timeStr)))

data Mood = Mood (Int, MilTime, String) deriving (Eq)

instance Show Mood where
  show (Mood (i, time, desc)) = show i ++ "(" ++ show time ++ "__" ++ desc ++ ")"

data Day = Day { date :: Cal.Day
               , mood :: [Mood]
               , pre :: String
               , post :: String
               , todos :: [Todo]
               , calendar :: [Event]
               , habits :: [Habit]
               }
         deriving (Eq)

property :: String -> String
property p = ":" ++ map toUpper p ++ ":"

-- this should return a Maybe String (for bogus input)
parseProperty :: String -> String -> String
parseProperty p day = case find (\line -> isPrefixOf (property p) line) (lines day) of
                       Just str -> strAfter ' ' str
                       Nothing -> ""

parsePropertyList :: String -> String -> [String]
parsePropertyList p day = case find (\line -> isPrefixOf (property p) line) (lines day) of
                           Just _ -> linesUntilNextProperty p day
                           Nothing -> []

linesUntilNextProperty :: String -> String -> [String]
linesUntilNextProperty p day = takeWhile (not . isPrefixOf ":") linesAfterProp
  where linesAfterProp = tail $ dropWhile (not . isPrefixOf (property p)) (lines day)

parseDate :: String -> Cal.Day
parseDate s = Cal.fromGregorian y m d
  where wholeDate = splitOn '-' (head $ words $ strBetween '<' '>' s)
        y = read $ wholeDate !! 0
        m = read $ wholeDate !! 1
        d = read $ wholeDate !! 2

parsePre :: String -> String
parsePre = parseProperty "pre"

parsePost :: String -> String
parsePost = parseProperty "post"

parseMood :: String -> [Mood]
parseMood day = map moodFromStr moodStrs
  where moodStrs = splitOn ';' $ parseProperty "mood" day

parseHabits :: String -> [Habit]
parseHabits day = map strToHabit $ parsePropertyList "habits" day

parseTodos :: String -> [Todo]
parseTodos day = map strToTodo $ parsePropertyList "todo" day

parseCalendar :: String -> [Event]
parseCalendar day = map strToEvent $ parsePropertyList "calendar" day

-- ugly, but I'm going to rewrite these using a real parser eventually
moodFromStr :: String -> Mood
moodFromStr str = Mood (int, time, desc)
  where int = read $ strBefore '(' str
        time = strToMilTime $ strBefore '_' $ strBetween '(' ')' str
        desc = strAfter '_' $ strAfter '_' $ strBetween '(' ')' str

strToDay :: String -> Day
strToDay dayStr = Day { date = parseDate $ head $ lines dayStr
                      , mood = parseMood dayStr
                      , pre = parsePre dayStr
                      , post = parsePost dayStr
                      , todos = parseTodos dayStr
                      , calendar = parseCalendar dayStr
                      , habits = parseHabits dayStr
                      }

instance Show Day where
  show day = init $ unlines [
      dayString
    , properties
    , ""
    , checkboxes
    ]
    where dayString = intercalate " " ["***", ratio, date']
          ratio = "[" ++ checkboxesCompleted ++ "/" ++ totalCheckboxes ++ "]"
          checkboxesCompleted = show $ habitsCompleted + todosCompleted
          totalCheckboxes = show $ totalHabits + totalTodos
          todosCompleted = length $ filter (\h -> isChecked (todoCB h)) (todos day)
          habitsCompleted = length $ filter (\h -> isChecked (habitCB h)) (habits day)
          totalTodos = length $ todos day
          totalHabits = length $ habits day
          date' = showDate $ date day
          showDate d = "<" ++ show d ++ " " ++ dayAbbrev d ++ ">"
          dayAbbrev d = getDayName $ dayIndex d
          checkboxes = ":TODO: _\n" ++ dayTodos ++ ":CALENDAR: _\n" ++ dayEvents ++ ":HABITS: _\n" ++ dayHabits
          dayTodos = unlines $ map show $ todos day
          dayEvents = unlines $ map show $ calendar day
          dayHabits = unlines $ map show $ habits day
          getDayName i = case i of
            1 -> "Mon"
            2 -> "Tue"
            3 -> "Wed"
            4 -> "Thu"
            5 -> "Fri"
            6 -> "Sat"
            7 -> "Sun"
            _ -> "SOMETHING WENT WRONG PARSING THE DATE"
          dayIndex date'' = weekDay $ WD.toWeekDate date''
          weekDay (_,_,d) = d
          properties = "\
\:MOOD: " ++ showMoods (mood day) ++ "\n\
\:PRE: " ++ (pre day) ++ "\n\
\:POST: " ++ (post day)

showMoods :: [Mood] -> String
showMoods moods = intercalate ";" (map show moods)

nextDay :: Day -> Day
nextDay day = day { date = Cal.addDays 1 (date day)
                  , mood = []
                  , pre = "_"
                  , post = "_"
                  , todos = filter unfinishedTodo (todos day)
                  , habits = map nextHabit (habits day)
                  }
  where unfinishedTodo t = (not . isChecked) (todoCB t)

newStreak :: Habit -> Int
newStreak h
  | isChecked cb = if s > 0 then s + 1 else 1
  | otherwise    = if s < 0 then s - 1 else -1
  where cb = habitCB h
        s = streak h

nextHabit :: Habit -> Habit
nextHabit habit = habit { habitCB = Checkbox { isChecked = False, description = habitDesc }
                        , streak = newStreak habit, details = ""
                        }
  where habitDesc = description (habitCB habit)

nextDayStr :: String -> String
nextDayStr = show . nextDay . strToDay

getLastDay :: String -> String
getLastDay file = init $ unlines $ reverse $ take lastDayLength reversedLines
  where reversedLines = reverse $ lines file
        lastDayLength = 1 + (length $ takeWhile (\line -> not ("*" `isPrefixOf` line)) reversedLines)
