module Main where

import Lib

import Control.Monad
import Control.Monad.IO.Class
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings <| loop []
  where
    loop :: DailyLogList -> InputT IO ()
    loop d = do
        minput <- getInputLine "|> "
        if minput == Nothing then loop d else do 
          dl <- liftIO (addToDailyLog d <| parseEntry <| pullString minput)
          mapM_ tPrintDailyLog dl
          loop dl
            where
              pullString :: Maybe String -> String
              pullString s = case s of Nothing -> "flip-flop"
                                       Just st -> st