module Lib
    (
      (|>)
    , (<|)
    , parseEntry
    , tPrintEntry
    , tPrintDailyLog
    , addToDailyLog
    , DailyLogList
    ) where

import Control.Monad.IO.Class
import System.Console.Haskeline
import System.Environment ( getArgs )
import Data.Functor
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar
import Test.QuickCheck
import Text.Read

apply :: a -> (a -> b) -> b
apply x f = f x

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = apply x f

infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = apply x f

date :: IO Day -- :: this is sort of a weird Haskell thing in Data.Time.Clock but hey, it's a single number that has nice functions for printing so....
date = utctDay <$> getCurrentTime

loopDefault :: IO ()
loopDefault = do
    args <- getArgs
    putStrLn "The arguments are: "
    mapM_ putStrLn args
    putStrLn ""
    putStrLn "Processing first argument as a daily log entry: "
    dl <- head args |> parseEntry |> addToDailyLog []  -- look how pretty that API is. you just give it a formatted string to parse and an empty list and you're done.
    printDailyLog <| head dl
    return ()

data Log = Log { title :: String, entries :: [Entry] }

newLog title entries = Log {title = title, entries = entries}

emptyLog = Log {title = "", entries = []}

data Token = Urgent | Task | Completed | MigratedDaily | MigratedFuture | Note | Struck | None deriving (Eq)

isSignifier token = token == Urgent

isBullet token = case token of Task           -> True
                               Completed      -> True
                               MigratedDaily  -> True
                               MigratedFuture -> True
                               Note           -> True
                               Struck         -> True
                               _              -> False

instance Show Token where
    show Urgent         = " !"
    show Task           = "."
    show Completed      = "X"
    show MigratedDaily  = ">"
    show MigratedFuture = "<"
    show Note           = "-"
    show Struck         = "~"
    show None           = ""

data Entry = Entry { tokens :: [Token], text :: String } deriving (Show)

printEntry :: Entry -> IO ()
printEntry entry = do
    print' <| tokens entry
    putStr " "
    putStrLn (text entry)
      where
        print' :: [Token] -> IO ()
        print' tokens = do putSig tokens 
                           putBul tokens
          where
            putSig :: [Token] -> IO ()
            putSig tokens = filter isSignifier tokens |> (\list -> if null list then [None] else list) |> head |> show |> putStr
            putBul :: [Token] -> IO ()
            putBul tokens = filter isBullet tokens |> (\list -> if null list then [None] else list) |> head |> show |> putStr

tPrintEntry :: Entry -> InputT IO ()
tPrintEntry entry = liftIO <| printEntry entry

printDailyLog :: DailyLog -> IO ()
printDailyLog dl = do
    print <| ddate dl
    mapM_ printEntry <| dentries dl
    return ()

tPrintDailyLog :: DailyLog -> InputT IO ()
tPrintDailyLog dl = liftIO <| printDailyLog dl

newEntry tokens text = Entry {tokens = tokens, text = text}

emptyEntry = Entry {tokens = [None], text = ""}

pairs :: [a] -> [(a, a)]
pairs []       = []
pairs [_]      = []
pairs (x:y:xs) = (x, y) : pairs xs

parsePair :: (Char, Char) -> Token
parsePair ('#', c) = parseToken c
parsePair (_, _)   = None

parseToken :: Char -> Token
parseToken char
    | char == 'u' = Urgent
    | char == 't' = Task
    | char == 'c' = Completed
    | char == 'd' = MigratedDaily
    | char == 'f' = MigratedFuture
    | char == 'n' = Note
    | char == 's' = Struck
    | otherwise   = None

parseEntryTokens :: String -> [Token]
parseEntryTokens string = pairs string |> take 2 |> map parsePair

parseEntry :: String -> Entry
parseEntry string = newEntry (parseEntryTokens string) <| (words string |> fixNull |> tail |> unwords)
  where 
    fixNull :: [String] -> [String]
    fixNull ws = if length ws < 2 then " " : ws else ws

logAppend :: Log -> Entry -> Log
logAppend log t = (t : entries log) |> newLog (title log)

data DailyLog = DailyLog { ddate :: Day, dentries :: [Entry] } deriving (Show)

newDailyLog :: Day -> [Entry] -> DailyLog
newDailyLog d e = DailyLog {ddate = d, dentries = e}

emptyDailyLog :: DailyLog
emptyDailyLog = newDailyLog (ModifiedJulianDay 0) []

dailyLogAppend :: DailyLog -> Entry -> DailyLog
dailyLogAppend d e = newDailyLog (ddate d) (e : dentries d)

type DailyLogList = [DailyLog]

{-
addToDailyLog :: DailyLogList -> Entry -> IO DailyLogList

1. What day is today?
2. What is the latest day for which a log already exists?
3. If that's today, add the entry to the latest log, otherwise make a new daily log for today with the first entry being the one supplied
-}

addToDailyLog :: DailyLogList -> Entry -> IO DailyLogList
addToDailyLog d e = do
    today <- date
    let latest = getLatestDate d
    if latest /= today then return (newDailyLog today [e] : d) else return (dailyLogAppend (head d) e : tail d)
      where
        getLatestDate :: DailyLogList -> Day
        getLatestDate d = dayList d |> foldl laterDay (ModifiedJulianDay 0)

laterDay :: Day -> Day -> Day
laterDay l d = if l < d then d else l

dayList :: DailyLogList -> [Day]
dayList = map ddate