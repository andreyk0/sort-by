module Main
  where

import           Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as U8
import           Data.List
import           Data.Ord
import           Options.Applicative
import           Text.Regex.TDFA
import           Text.Regex.TDFA.ByteString

import Debug.Trace


data GlobalOptions =
  GlobalOptions { optReverse :: Bool
                , optCommand :: Command
                } deriving (Show)

data SortByDateOptions =
  SortByDateOptions { dateFormat :: String
                    } deriving (Show)

data Command = SortBySemVer
             | SortByDate SortByDateOptions
             deriving (Show)
  

parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions = subparser $ cmdSemVer <> cmdDate
  where cmdSemVer  = command "semver" $
                     info semverOptions $
                     progDesc "Sort lines by 'semver' (e.g. 0.1.2)."

        cmdDate    = command "date" $
                     info dateOptions $
                     progDesc "Sort lines by date (e.g. logs)."

        globalOptions = GlobalOptions <$> switch ( long "reverse" <> short 'r' <> help "")

        semverOptions = globalOptions <*> (pure SortBySemVer)

        dateOptions   = globalOptions <*> (SortByDate <$> sortByDateOptions)

        sortByDateOptions = SortByDateOptions <$>
                              (strOption $
                                 long "date-format" <>
                                 short 'f' <>
                                 metavar "DATE_FORMAT" <>
                                 value "haha" <>
                                 help "Date format."
                              )


main :: IO ()
main = execParser opts >>= runSort
  where
    opts = info (helper <*> parseGlobalOptions) $
             fullDesc
             <> progDesc "Sorts lines of text using a few built-in functions: semver, date."
             <> header "Sorts lines of text using a few built-in functions."


runSort :: GlobalOptions -> IO ()
runSort opts =
  case (optCommand opts) of
   SortBySemVer -> runSortBySemVer opts
   _ -> putStrLn (show opts)


data SemVer =
  SemVer { vMajor :: !Int
         , vMinor :: !Int
         , vMicro :: !Int
         , vBuild :: !Int
         } deriving (Eq, Show, Ord)

semVerZero :: SemVer
semVerZero = SemVer 0 0 0 0

runSortBySemVer :: GlobalOptions -> IO ()
runSortBySemVer opts = do
  txt <- LB.getContents
  let lines = U8.lines txt
  let semverToLine = map (parseSemVerLine) lines
  let sortedLines = sortBy (comparing fst) semverToLine
  forM_ (map (LB.toStrict . snd) sortedLines) (C.putStrLn)


parseSemVerLine :: LB.ByteString -> (SemVer, LB.ByteString)
parseSemVerLine l = ( (maybe semVerZero (id) maybeSemVer), l )
  where maybeSemVer =
          case ( l =~~ "([0-9]+)\\.([0-9]+)(\\.([0-9]+))?(\\.([0-9]+))?" :: Maybe [[LB.ByteString]])
            of Just ((_ : vMajor : vMinor : _ : vMicro : vBuild: _):_) -> 
                 SemVer <$> (maybeReadInt vMajor)
                        <*> (maybeReadInt vMinor)
                        <*> (maybe (Just 0) (Just) (maybeReadInt vMicro))
                        <*> (maybe (Just 0) (Just) (maybeReadInt vBuild))

               Just x -> trace ("ERROR: " ++ (show x)) $ Nothing

               Nothing -> Nothing

        maybeReadInt :: LB.ByteString -> Maybe Int
        maybeReadInt s = do
          (sInt, _) <- (C.readInt . LB.toStrict) s
          return sInt
