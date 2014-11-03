module Main
  where

import           Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as U8
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Time.Format
import           Data.Time.LocalTime
import           Options.Applicative
import           System.Locale
import           Text.Regex.TDFA

import           Debug.Trace


data GlobalOptions =
  GlobalOptions { optReverse :: Bool
                , optCommand :: Command
                } deriving (Show)

data SortByDateOptions =
  SortByDateOptions { listExampleDateFormats :: Bool
                    , dateFormat :: String
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

        globalOptions = GlobalOptions <$> switch ( long "reverse" <> short 'r' <> help "Reverse order.")

        semverOptions = globalOptions <*> (pure SortBySemVer)

        dateOptions   = globalOptions <*> (SortByDate <$> sortByDateOptions)

        sortByDateOptions = SortByDateOptions <$>
                              switch (
                                long "list" <>
                                short 'l' <>
                                help "List some example date format strings.") <*>
                              (strOption $
                                 long "date-format" <>
                                 short 'f' <>
                                 metavar "DATE_FORMAT" <>
                                 value "%a %b %e %H:%M:%S %Z %Y" <>
                                 help "Date format, default is '%a %b %e %H:%M:%S %Z %Y'"
                              )


main :: IO ()
main = execParser opts >>= runSort
  where
    opts = info (helper <*> parseGlobalOptions) $
             fullDesc
             <> progDesc "Sorts lines of text using a few built-in functions: semver, date."
             <> header "Sorts lines of text using a few built-in functions."


runSort :: GlobalOptions -> IO ()
runSort gOpts =
  case (optCommand gOpts) of
   SortBySemVer -> runSortBy gOpts (parseSemVerLine)
   SortByDate (SortByDateOptions True _) -> runListExampleDateFormats
   SortByDate dOpts@(SortByDateOptions False _) -> runSortBy gOpts (parseTimeFmt (dateFormat dOpts))


data SemVer =
  SemVer { vMajor :: !Int
         , vMinor :: !Int
         , vMicro :: !Int
         , vBuild :: !Int
         } deriving (Eq, Show, Ord)


semVerZero :: SemVer
semVerZero = SemVer (-1) (-1) (-1) (-1)


parseSemVerLine :: LB.ByteString -> SemVer
parseSemVerLine l = maybe semVerZero (id)  maybeSemVer
  where maybeSemVer =
          case ( l =~~ "([0-9]+)\\.([0-9]+)(\\.([0-9]+))?(\\.([0-9]+))?" :: Maybe [[LB.ByteString]])
            of Just ((_ : vMajor' : vMinor' : _ : vMicro' : vBuild' : _):_) ->
                 SemVer <$> (maybeReadInt vMajor')
                        <*> (maybeReadInt vMinor')
                        <*> (maybe (Just (-1)) (Just) (maybeReadInt vMicro'))
                        <*> (maybe (Just (-1)) (Just) (maybeReadInt vBuild'))

               Just x -> trace ("ERROR: " ++ (show x)) $ Nothing

               Nothing -> Nothing

        maybeReadInt :: LB.ByteString -> Maybe Int
        maybeReadInt s = do
          (sInt, _) <- (C.readInt . LB.toStrict) s
          return sInt


parseTimeFmt :: String -> LB.ByteString -> LocalTime
parseTimeFmt fmt l = fromMaybe earliestPossibleTime (findTime fmt (U8.toString l))
  where earliestPossibleTime = readTime defaultTimeLocale "" "" :: LocalTime


findTime :: String -> String -> Maybe LocalTime
findTime fmt s =
  fmap (fst . last) $ find (not . null) $ map (rTime) $ tails s
  where rTime :: ReadS LocalTime
        rTime = readsTime defaultTimeLocale fmt


runSortBy :: Ord a => GlobalOptions -> (LB.ByteString -> a) -> IO ()
runSortBy gOpts lToOrd = do
  txt <- LB.getContents

  let ordLn = map (\x -> (lToOrd x, x)) (U8.lines txt)
      comp = if (optReverse gOpts)
             then (flip (comparing fst))
             else (comparing fst)
      sortedLines = sortBy (comp) ordLn

  forM_ (map (LB.toStrict . snd) sortedLines) (C.putStrLn)


runListExampleDateFormats :: IO ()
runListExampleDateFormats = do
  putStrLn "'%a %b %e %H:%M:%S %Z %Y' -> 'Sun Nov  2 22:22:17 EST 2014'"
  putStrLn "'%Y-%m-%dT%H:%M:%S%Q'     -> '2014-10-27T09:44:55+00:00'"
