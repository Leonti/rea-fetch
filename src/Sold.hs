module Sold where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Tree.Selection
import Data.List.Split
import Data.List
import Text.CSS3.Selectors.Parser
import Text.HTML.TagSoup.Tree.Zipper as TagTreeZipper
import Text.CSS3.Selectors.Syntax
import Data.Maybe
import Text.Read
import Data.String.Utils
import Data.Time.Parse
import Data.Time.LocalTime
import Safe
import Debug.Trace

data SoldPage = SoldPage
                        { page :: String
                        , content :: String
                        , dates :: [LocalTime]
                        , nextPage :: Maybe String
                        } deriving (Show)

toSoldPage :: String -> String -> SoldPage
toSoldPage link pageBody = SoldPage
    { page = link
    , Sold.content = pageBody
    , dates = findDates pageBody
    , nextPage = findNextPageLink pageBody
    }

findDates :: String -> [LocalTime]
findDates pageContent =
    case maybeBodyTree of
        Just bodyTree -> fmap (fromJust . toDate) dateTags
            where
                dateTags :: [TagTreePos String]
                dateTags = select (sel ".property-card__with-comma") bodyTree
        Nothing -> []
    where
        pageTagTree :: [TagTree String]
        pageTagTree = parseTree pageContent
        maybeBodyTree = find isBody pageTagTree

findNextPageLink :: String -> Maybe String
findNextPageLink pageContent =
    (>>=) maybeBodyTree linkFromBodyTree
    where
        pageTagTree = parseTree pageContent
        maybeBodyTree = find isBody pageTagTree

linkFromBodyTree :: TagTree String -> Maybe String
linkFromBodyTree bodyTree = extractedUrl
    where
        nextPageTags :: [TagTreePos String]
        nextPageTags = select (sel ".pagination__next a") bodyTree
        maybeTagTreePos :: Maybe (TagTreePos String)
        maybeTagTreePos = headMay nextPageTags
        extractedUrl = (>>=) maybeTagTreePos (extractUrl . TagTreeZipper.content)


extractUrl :: TagTree String -> Maybe String
extractUrl (TagBranch _ attributes _) = lookup "href" attributes
extractUrl (TagLeaf _) = Nothing

toDate :: TagTreePos String -> Maybe LocalTime
toDate tree = fmap fst maybeParsedDate
    where
        flattenedTree = flattenTree [TagTreeZipper.content tree]
        unparsedFullDate = innerText flattenedTree
        dateAsString = replace "Sold on " "" unparsedFullDate
        maybeParsedDate = strptime "%d %b %Y" dateAsString

isBody :: TagTree String -> Bool
isBody tree = not (null (select (sel ".property-card__with-comma") tree))
