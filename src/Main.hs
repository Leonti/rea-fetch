module Main where

import Data.Char
import Network.HTTP
import Text.HTML.TagSoup
import Text.Regex.TDFA
import Data.Maybe
import Control.Concurrent

import System.IO
import Control.Monad

import Data.Time.Clock
import Data.Time.Calendar

import Data.List.Split

import System.Directory

data PageResult = PageResult    { page :: String
                                , content :: String
                                , propertyLinks :: [String]
                                , nextPage :: Maybe String
                                } deriving (Show)

baseUrl :: String
baseUrl = "http://www.realestate.com.au"

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

timestamp :: IO String -- :: (year,month,day)
timestamp = fmap (dateAsString . toGregorian . utctDay) getCurrentTime

dateAsString :: (Integer, Int, Int) -> String
dateAsString (year, month, day) = show year ++ "-" ++ show month ++ "-" ++ show day

resultToPrintable :: PageResult -> String
resultToPrintable pageResult =
    show (page pageResult) ++ " " ++ show (nextPage pageResult)

singlePage :: IO PageResult
singlePage = propertyUrlsFromLink "/buy/between-200000-500000-in-richmond%2c+vic+3121/list-3"

reaResults :: IO [PageResult]
reaResults = do
        result <- propertyUrlsFromLink "/buy/between-200000-500000-in-richmond%2c+vic+3121%3b/list-1"
        allPropertyUrls [result] result

allPropertyUrls :: [PageResult] -> PageResult -> IO [PageResult]
allPropertyUrls prevResults pageResult = case nextPage pageResult of
    Just nextPage -> do
        pageResult <- propertyUrlsFromLink nextPage
        allPropertyUrls (prevResults ++ [pageResult]) pageResult
    _ -> return (prevResults ++ [pageResult])

propertyUrlsFromLink :: String -> IO PageResult
propertyUrlsFromLink link = do
    responseBody <- openURL $ baseUrl ++ link
    _ <- Control.Concurrent.threadDelay 1000000
    let propertyLinks = propertyUrls (parseTags responseBody)
    let nextPage = nextPageUrl (parseTags responseBody)
    _ <- print link
    return PageResult { page=link
                        , content=responseBody
                        , propertyLinks=propertyLinks
                        , nextPage=nextPage
                        }

propertyUrls :: [Tag String] -> [String]
propertyUrls tags = extractUrl <$> filter propertyLink tags
    where
        extractUrl :: Tag String -> String
        extractUrl = fromAttrib "href"

propertyLink :: Tag String -> Bool
propertyLink tag = tag ~== TagOpen "a" []
    && fromAttrib "class" tag =~ "detailsButton"

nextPageUrl :: [Tag String] -> Maybe String
nextPageUrl tags = case filter nextPageLink tags of
    (tag:_) -> Just $ fromAttrib "href" tag
    [] -> Nothing

nextPageLink :: Tag String -> Bool
nextPageLink tag = tag ~== TagOpen "a" []
    && fromAttrib "title" tag == "View the next page of results"

writePageResult :: FilePath -> PageResult -> IO ()
writePageResult resultsFolder pageResult =
    writeFile (resultsFolder ++ "/" ++ fileName) (content pageResult)
    where
        fileName = last $ splitOn "/" (page pageResult)

fetchResults :: String -> IO ()
fetchResults folderName= do
    homeDirectory <- getHomeDirectory
    let resultsFolder = homeDirectory ++ "/reaResults/" ++ folderName
    _ <- createDirectoryIfMissing True resultsFolder
    pageResults <- reaResults
    _ <- mapM (writePageResult resultsFolder) pageResults
    return ()

listFiles :: String -> IO [FilePath]
listFiles t = do
    homeDirectory <- getHomeDirectory
    let resultsFolder = homeDirectory ++ "/reaResults/" ++ t
    files <- listDirectory resultsFolder
    return $ fmap (\file -> resultsFolder ++ "/" ++ file) files

main :: IO ()
main = do
    _ <- print "Fetching pages ..."
    t <- timestamp
    _ <- fetchResults t
    files <- listFiles t
    _ <- mapM print files
    print "Done"
