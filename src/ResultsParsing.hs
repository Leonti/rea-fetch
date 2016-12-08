module ResultsParsing where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Tree.Selection
import Text.Regex.TDFA
import Data.List.Split
import Data.List
import Text.CSS3.Selectors.Parser
import Text.HTML.TagSoup.Tree.Zipper
import Text.CSS3.Selectors.Syntax

data Property = Property        { bedrooms :: Int
                                , bathrooms :: Int
                                , parking :: Int
                                , location :: String
                                , price :: Int
                                } deriving (Show)

parsePage :: String -> String
parsePage content =
--    show $ (propertyFeaturesList !! 1)
--    show $ (tagTree !! 9)
    --show $ fmap (\trees -> fmap (\features -> length features) trees) propertyFeaturesTrees
      show $ fmap (\addresses -> addresses) maybeAddresses
--    show maybeBodyTree
--    show ""
    where
        tags = parseTags content
        tagTree = parseTree content
        maybeBodyTree = find hasListings tagTree
        maybeListingTrees = fmap listings maybeBodyTree
        maybeAddresses = fmap (\listingTrees -> fmap addressFromListingTree listingTrees) maybeListingTrees
        propertyFeaturesTrees = fmap (\listingTrees -> fmap propertyFeaturesTree listingTrees) maybeListingTrees
        listingSections = tail $ splitWhen listingInfo tags
        propertyFeaturesList = fmap listingInfoToPropertyFeatures listingSections


addressFromListingTree :: TagTreePos String -> String
addressFromListingTree listingTree =
    head streetAddresses
    where
        addressTrees :: [TagTreePos String]
        addressTrees = select addressSelector (content listingTree)

        streetAddresses :: [String]
        streetAddresses = fmap streetFromAddressTree addressTrees

streetFromAddressTree :: TagTreePos String -> String
streetFromAddressTree addressTree =
    innerText tagList
    where
        tagList :: [Tag String]
        tagList = flattenTree [(content addressTree)]

propertyFromListingTree :: TagTreePos String -> [Property]
propertyFromListingTree listingTree =
    []
    where
        propertyFeatures :: [TagTreePos String]
        propertyFeatures = propertyFeaturesTree listingTree

addressSelector :: Selector
addressSelector = sel "a[rel=listingName]"

listingSelector :: Selector
listingSelector = sel "article.resultBody"

listings :: TagTree String -> [TagTreePos String]
listings bodyTree = select listingSelector bodyTree

propertyFeaturesSelector :: Selector
propertyFeaturesSelector = sel "dl.rui-property-features"

propertyFeaturesTree :: TagTreePos String -> [TagTreePos String]
propertyFeaturesTree listingTree = select propertyFeaturesSelector (content listingTree)

hasListings :: TagTree String -> Bool
hasListings tree = length (select (sel "article.resultBody") tree) > 0

listingInfoToPropertyFeatures :: [Tag String] -> [[Tag String]]
listingInfoToPropertyFeatures tags = tail $ splitWhen propertyFeatures tags

propertyFeatures :: Tag String -> Bool
propertyFeatures tag = tag ~== TagOpen "dl" []
    && fromAttrib "class" tag =~ "rui-property-features"

listingInfo :: Tag String -> Bool
listingInfo tag = tag ~== TagOpen "article" []
    && fromAttrib "class" tag =~ "resultBody"
