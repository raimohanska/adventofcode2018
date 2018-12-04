{-# LANGUAGE TupleSections, LambdaCase, FlexibleContexts #-}
module Main where
import Text.Parsec
import Data.Char(digitToInt)
import Data.List(tails, foldl', sort, group, intersect)
import Data.Maybe(maybeToList)
import Data.Either(rights)
import GHC.Exts(sortWith, groupWith)
import qualified Data.Set as S

main = readFile "4.sorted.txt" >>= (return . solve . lines) >>= print

solve lines = 
  let
    logEntries = rights $ map (parse logEntry "") lines
    groupedIntoShifts = groupAt isShiftStart logEntries
    shifts = sortWith fst $ map entriesToShift groupedIntoShifts
    groupedByGuard = groupWith fst shifts
    sleepiestGuard = fst $ head $ reverse $ sortWith snd $ map shiftsToSleepStats groupedByGuard
    sleepyGuardShifts = filter ((==sleepiestGuard) . fst) shifts
    sleepiestMinute = head $ head $ reverse $ sortWith length $ group $ sort $ sleepyGuardShifts >>= snd
  in sleepiestMinute * sleepiestGuard

shiftsToSleepStats :: [Shift] -> (Int, Int)
shiftsToSleepStats shifts =
  let guardId = fst $ head shifts
      sleepMinutes = sum $ map (length . snd) shifts 
  in (guardId, sleepMinutes)

entriesToShift :: [LogEntry] -> Shift
entriesToShift ((LogEntry _ _ (BeginShift id)) : entries) =
  (id, toSleepMinutes entries)

toSleepMinutes :: [LogEntry] -> [Int]
toSleepMinutes entries =
  let step (result, now, True) event = (result ++ [now .. (minuteOf event) - 1], minuteOf event, False) 
      step (result, now, False) event | isStartSleep event = (result, minuteOf event, True)
                                      | otherwise          = (result, minuteOf event, False)
      finalize (result, now, True) = result ++ [now .. 59]
      finalize (result, now, False) = result
  in finalize $ foldl step ([], 0, False) (entries)

isShiftStart (LogEntry _ _ (BeginShift _)) = True
isShiftStart _ = False

isStartSleep (LogEntry _ _ FallAsleep) = True
isStartSleep _ = False

groupAt :: (a -> Bool) -> [a] -> [[a]]
groupAt pred xs = 
  let init = ([], Nothing) :: ([[a]], Maybe [a])
      step (groups, Nothing) x = (groups, Just [x])
      step (groups, Just(group)) x | pred x = (groups ++ [group], Just [x])
                                   | True   = (groups, Just(group ++ [x]))
      finalize (groups, group) = groups ++ (maybeToList group)
  in finalize $ foldl step init xs

minuteOf (LogEntry _ (Timestamp _ m) _) = m

--            id  minutes asleep
type Shift = (Int, [Int])

data LogEntry = LogEntry Datestamp Timestamp LogEvent deriving (Eq, Show) 
data LogEvent = BeginShift Int | FallAsleep | WakeUp deriving (Eq, Show)

data Datestamp = Datestamp { _y :: Int, _mo :: Int, _d :: Int } deriving (Eq, Show)
data Timestamp = Timestamp { _h :: Int, _m :: Int } deriving (Eq, Show)

logEntry :: Monad m => ParsecT String u m LogEntry
logEntry = LogEntry
  <$> (char '[' >> datestamp)
  <*> (char ' ' >> timestamp)
  <*> (string "] " >> logEvent)

datestamp :: Monad m => ParsecT String u m Datestamp
datestamp =  Datestamp
  <$> (int)
  <*> (char '-' >> int)
  <*> (char '-' >> int)

timestamp :: Monad m => ParsecT String u m Timestamp
timestamp = Timestamp 
  <$> int 
  <*> (char ':' >> int) 

fallAsleep :: Monad m => ParsecT String u m LogEvent
fallAsleep = string "falls asleep" >> return FallAsleep

wakeUp :: Monad m => ParsecT String u m LogEvent
wakeUp = string "wakes up" >> return WakeUp

beginShift :: Monad m => ParsecT String u m LogEvent
beginShift = do
  string "Guard #"
  id <- int
  string " begins shift"
  return $ BeginShift id

logEvent :: Monad m => ParsecT String u m LogEvent
logEvent = beginShift <|> fallAsleep <|> wakeUp

int :: Monad m => ParsecT String u m Int
int = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit
