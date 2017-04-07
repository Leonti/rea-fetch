module OnSale where

import Data.Char
import Network.HTTP
import Text.HTML.TagSoup
import Text.Regex.TDFA
import Data.Maybe

import Data.List.Split

data PageResult = PageResult    { page :: String
                                , content :: String
                                , propertyLinks :: [String]
                                , nextPage :: Maybe String
                                } deriving (Show)

toPageResult :: String -> String -> PageResult
toPageResult link pageBody =
    PageResult
            { page = link
            , content = pageBody
            , propertyLinks = propertyLinks
            , nextPage = nextPage
            }
    where
        propertyLinks = propertyUrls (parseTags pageBody)
        nextPage = nextPageUrl (parseTags pageBody)

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
