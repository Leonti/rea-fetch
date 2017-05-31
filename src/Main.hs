module Main where

import Data.Char
import Network.HTTP.Conduit
--import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe
import Control.Concurrent

import System.IO
import Control.Monad

import Data.Time.Clock
import Data.Time.Calendar

import Data.List.Split

import System.Directory
import System.Environment

import Data.Time.LocalTime
import Data.Time.Parse
import OnSale
import Sold

import qualified Data.ByteString.Lazy.Char8 as LB

baseUrl = "https://www.realestate.com.au"
pageDelay = 1000000
soldPropertiesBaseUrl = "/sold/with-1-bedroom-in-melbourne+city+-+greater+region%2c+vic/list-1?numParkingSpaces=1&maxBeds=1&misc=ex-no-sale-price&activeSort=solddate&source=refinement"

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

reaResults :: IO [PageResult]
reaResults = do
        result <- propertyPageFromLink "/buy/between-200000-500000-in-richmond%2c+vic+3121%3b/list-1"
        allPropertyUrls [] result

singleSoldPage :: IO SoldPage
singleSoldPage = soldPageFromLink soldPropertiesBaseUrl

soldResults :: LocalTime -> IO [SoldPage]
soldResults cutOffTime = do
    result <- soldPageFromLink soldPropertiesBaseUrl
    allSoldPropertyUrls [] cutOffTime result

allPropertyUrls :: [PageResult] -> PageResult -> IO [PageResult]
allPropertyUrls prevResults currentPageResult = case OnSale.nextPage currentPageResult of
    Just nextPage -> do
        pageResult <- propertyPageFromLink nextPage
        allPropertyUrls (prevResults ++ [currentPageResult]) pageResult
    _ -> return (prevResults ++ [currentPageResult])

allSoldPropertyUrls :: [SoldPage] -> LocalTime -> SoldPage -> IO [SoldPage]
allSoldPropertyUrls prevResults cutOffDate currentSoldPage =
    case Sold.nextPage currentSoldPage of
        Just nextPage -> if minimum (Sold.dates currentSoldPage) < cutOffDate then
                return (prevResults ++ [currentSoldPage])
             else
                 do
                     soldPageResult <- soldPageFromLink nextPage
                     allSoldPropertyUrls (prevResults ++ [currentSoldPage]) cutOffDate soldPageResult
        _ -> return (prevResults ++ [currentSoldPage])

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
writeSoldResult resultsFolder pageResult =
    writeFile (resultsFolder ++ "/" ++ fileName) (Sold.content pageResult)
    where
        nameWithQuery = last $ splitOn "/" (Sold.page pageResult)
        fileName = head $ splitOn "?" nameWithQuery

fetchResults :: String -> IO ()
fetchResults folderName = do
    homeDirectory <- getHomeDirectory
    let resultsFolder = homeDirectory ++ "/reaResults/" ++ folderName
    _ <- createDirectoryIfMissing True resultsFolder
    pageResults <- reaResults
    _ <- mapM (writePageResult resultsFolder) pageResults
    return ()

fetchSoldResults :: String -> LocalTime -> IO ()
fetchSoldResults folderName cutOffDate = do
    homeDirectory <- getHomeDirectory
    let resultsFolder = homeDirectory ++ "/reaSoldResults/" ++ folderName
    _ <- createDirectoryIfMissing True resultsFolder
    pageResults <- soldResults cutOffDate
    _ <- mapM (writeSoldResult resultsFolder) pageResults
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
