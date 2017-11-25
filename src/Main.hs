module Main where

import           Data.Char
import           Network.HTTP.Conduit
--import qualified Data.ByteString.Lazy as LB
import           Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Maybe

import           Control.Monad
import           System.IO

import           Data.Time.Calendar
import           Data.Time.Clock

import           Data.List.Split

import           System.Directory
import           System.Environment

import           Data.Time.LocalTime
import           Data.Time.Parse
import           OnSale
import           Sold

import           Streaming
import qualified Streaming.Prelude          as S

import qualified Data.ByteString.Lazy.Char8 as LB
import           System.Random              (randomIO)

baseUrl = "https://www.realestate.com.au"
pageDelay = 1000000
soldPropertiesBaseUrls =
  [ "/sold/property-unit+apartment-between-0-600000-in-melbourne%2c+vic+3000/list-1?includeSurrounding=false&misc=ex-no-sale-price&activeSort=solddate&source=refinement"
  , "/sold/property-unit+apartment-between-0-600000-in-melbourne%2c+vic+3004/list-1?includeSurrounding=false&misc=ex-no-sale-price&activeSort=solddate&source=refinement"
  , "/sold/property-unit+apartment-between-0-600000-in-richmond%2c+vic+3121/list-1?includeSurrounding=false&misc=ex-no-sale-price&activeSort=solddate&source=refinement"
  , "/sold/property-unit+apartment-between-0-600000-in-abbotsford%2c+vic+3067/list-1?includeSurrounding=false&misc=ex-no-sale-price&activeSort=solddate&source=refinement"
  , "/sold/property-unit+apartment-between-0-600000-in-hawthorn%2c+vic+3122/list-1?includeSurrounding=false&misc=ex-no-sale-price&activeSort=solddate&source=refinement"
  , "/sold/property-unit+apartment-between-0-600000-in-east+melbourne%2c+vic+3002/list-1?includeSurrounding=false&misc=ex-no-sale-price&activeSort=solddate&source=refinement"
  , "/sold/property-unit+apartment-between-0-600000-in-fitzroy%2c+vic+3065/list-1?includeSurrounding=false&misc=ex-no-sale-price&activeSort=solddate&source=refinement"
  , "/sold/property-unit+apartment-between-0-600000-in-collingwood%2c+vic+3066/list-1?includeSurrounding=false&misc=ex-no-sale-price&activeSort=solddate&source=refinement"
  , "/sold/property-unit+apartment-between-0-600000-in-carlton%2c+vic+3053/list-1?includeSurrounding=false&misc=ex-no-sale-price&activeSort=solddate&source=refinement"
  , "/sold/property-unit+apartment-between-0-600000-in-south+yarra%2c+vic+3141/list-1?includeSurrounding=false&misc=ex-no-sale-price&activeSort=solddate&source=refinement"
  , "/sold/property-unit+apartment-between-0-600000-in-south+melbourne%2c+vic+3205/list-1?includeSurrounding=false&misc=ex-no-sale-price&activeSort=solddate&source=refinement"
  ]
onSalePropertiesBaseUrl = "/buy/between-200000-500000-in-richmond%2c+vic+3121%3b/list-1"

openURL :: String -> IO String
openURL x = do
    manager <- newManager tlsManagerSettings
    result <- openURLWithTimeout manager x 45000000
    return (BS.unpack result)

openURLWithTimeout :: Manager -> String -> Int -> IO LB.ByteString
openURLWithTimeout manager url timeout = do
    req <- parseRequest url
    let req' = req {responseTimeout = responseTimeoutMicro 45000000}
    responseBody <$> httpLbs req' manager

timestamp :: IO String -- :: (year,month,day)
timestamp = fmap (dateAsString . toGregorian . utctDay) getCurrentTime

dateAsString :: (Integer, Int, Int) -> String
dateAsString (year, month, day) = show year ++ "-" ++ show month ++ "-" ++ show day

resultToPrintable :: PageResult -> String
resultToPrintable pageResult =
    show (OnSale.page pageResult) ++ " " ++ show (OnSale.nextPage pageResult)

singlePage :: IO PageResult
singlePage = propertyPageFromLink "/buy/between-200000-500000-in-richmond%2c+vic+3121/list-3"

singleSoldPage :: IO SoldPage
singleSoldPage = soldPageFromLink $ head soldPropertiesBaseUrls

resultsAsStream :: Stream (Of PageResult) IO ()
resultsAsStream = S.unfoldr nextResult (Just onSalePropertiesBaseUrl)

nextResult :: Maybe String -> IO (Either () (PageResult, Maybe String))
nextResult maybeNextLink = case maybeNextLink of
    Just nextPage -> do
        pageResult <- propertyPageFromLink nextPage
        return (Right (pageResult, OnSale.nextPage pageResult))
    _ -> return (Left ())

soldResultsAsStream :: LocalTime -> String -> Stream (Of SoldPage) IO ()
soldResultsAsStream cutOffTime initialUrl = S.unfoldr (nextSoldResult cutOffTime) (Just initialUrl)

nextSoldResult :: LocalTime -> Maybe String -> IO (Either () (SoldPage, Maybe String))
nextSoldResult cutOffTime maybeNextLink =
    case maybeNextLink of
        Just nextPage ->
            do
                soldPageResult <- soldPageFromLink nextPage
                return (
                    if minimum (Sold.dates soldPageResult) < cutOffTime then
                        Left ()
                    else
                        Right (soldPageResult, Sold.nextPage soldPageResult))
        _ -> return (Left ())

propertyPageFromLink :: String -> IO PageResult
propertyPageFromLink link = do
    responseBody <- openURL $ baseUrl ++ link
    _ <- Control.Concurrent.threadDelay pageDelay
    let pageResult = toPageResult link responseBody
    _ <- print (OnSale.page pageResult)
    return pageResult

soldPageFromLink :: String -> IO SoldPage
soldPageFromLink link = do
    responseBody <- openURL $ baseUrl ++ link
    _ <- Control.Concurrent.threadDelay pageDelay
    let soldPage = toSoldPage link responseBody
    _ <- print (Sold.page soldPage)
    return soldPage

writePageResult :: FilePath -> PageResult -> IO ()
writePageResult resultsFolder pageResult =
    writeFile (resultsFolder ++ "/" ++ fileName) (OnSale.content pageResult)
    where
        fileName = last $ splitOn "/" (OnSale.page pageResult)

writeSoldResult :: FilePath -> SoldPage -> IO ()
writeSoldResult resultsFolder pageResult = do
    randomNumber <- randomIO :: IO Int
    writeFile (resultsFolder ++ "/" ++ (fileName randomNumber)) (Sold.content pageResult)
    where
        nameWithQuery = last $ splitOn "/" (Sold.page pageResult)
        fileName randomNumber = (head $ splitOn "?" nameWithQuery) ++ (show randomNumber)

fetchResults :: String -> IO ()
fetchResults folderName = do
    homeDirectory <- getHomeDirectory
    let resultsFolder = homeDirectory ++ "/reaResults/" ++ folderName
    _ <- createDirectoryIfMissing True resultsFolder
    let pageResults = resultsAsStream
    S.mapM_ (writePageResult resultsFolder) pageResults
    return ()

fetchSoldResults :: String -> LocalTime -> IO ()
fetchSoldResults folderName cutOffDate = do
    homeDirectory <- getHomeDirectory
    let resultsFolder = homeDirectory ++ "/reaSoldResults/" ++ folderName
    _ <- createDirectoryIfMissing True resultsFolder
    let pageResults = fmap (soldResultsAsStream cutOffDate) soldPropertiesBaseUrls
    mapM (S.mapM_ (writeSoldResult resultsFolder)) pageResults
    return ()

readLastTimestamp :: IO LocalTime
readLastTimestamp = do
    asString <- getEnv "EARLIEST_SOLD"
    let (stamp, _) = fromJust (strptime "%Y-%m-%d" asString)
    return stamp


listFiles :: String -> IO [FilePath]
listFiles t = do
    homeDirectory <- getHomeDirectory
    let resultsFolder = homeDirectory ++ "/reaResults/" ++ t
    files <- listDirectory resultsFolder
    return $ fmap (\file -> resultsFolder ++ "/" ++ file) files

soldProperties :: IO ()
soldProperties = do
    homeDirectory <- getHomeDirectory
    contents <- readFile $ homeDirectory ++ "/Downloads/sold.htm"
    let dates = findDates contents
    let nextPageLink = findNextPageLink contents
    _ <- print dates
    print nextPageLink

fetchAllSold :: IO ()
fetchAllSold = do
    _ <- print "Fetching sold results"
    stringDate <- timestamp
    t <- readLastTimestamp
    _ <- fetchSoldResults stringDate t
    print "Done fetching sold results"

fetchAllOnSale :: IO ()
fetchAllOnSale = do
    _ <- print "Fetching on sale pages ..."
    stringDate <- timestamp
    _ <- fetchResults stringDate
    files <- listFiles stringDate
    _ <- mapM print files
    print "Done fetching on sale"

main :: IO ()
main = do
    _ <- fetchAllSold
    _ <- fetchAllOnSale
    print "All Done"
