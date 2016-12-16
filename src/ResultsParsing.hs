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

data PropertyDetails = PropertyDetails  { bedrooms :: Maybe String
                                        , bathrooms :: Maybe String
                                        , cars :: Maybe String
                                        } deriving (Show)

data Property = Property        { details :: PropertyDetails
                                , location :: String
                                , price :: Maybe String
                                } deriving (Show)

parsePage :: String -> String
parsePage content =
--    show $ (propertyFeaturesList !! 1)
--    show $ (tagTree !! 9)
    --show $ fmap (\trees -> fmap (\features -> length features) trees) propertyFeaturesTrees
--      show $ fmap (\addresses -> addresses) maybeAddresses
      --show  maybeDetails
      show maybeProperties
--    show maybeBodyTree
--    show ""
    where
        tags = parseTags content
        tagTree = parseTree content
        maybeBodyTree = find hasListings tagTree
        maybeListingTrees = fmap listings maybeBodyTree
        maybeAddresses = fmap (fmap addressFromListingTree) maybeListingTrees
        maybeDetails = fmap (fmap detailsFromListingTree) maybeListingTrees
        maybeProperties = fmap (fmap parseListing) maybeListingTrees
        propertyFeaturesTrees = fmap (fmap propertyFeaturesTree) maybeListingTrees
        listingSections = tail $ splitWhen listingInfo tags
        propertyFeaturesList = fmap listingInfoToPropertyFeatures listingSections


parseListing :: TagTreePos String -> [Property]
parseListing listingTree =
    fmap (`combineData` address) details
    where
        address :: String
        address = addressFromListingTree listingTree

        details :: [(PropertyDetails, Maybe String)]
        details = detailsFromListingTree listingTree

        combineData :: (PropertyDetails, Maybe String) -> String -> Property
        combineData (propertyDetails, maybePrice) address =
            Property {details = propertyDetails, price = maybePrice, location = address}

addressFromListingTree :: TagTreePos String -> String
addressFromListingTree listingTree =
    head streetAddresses
    where
        addressTrees :: [TagTreePos String]
        addressTrees = select addressSelector (content listingTree)

        streetAddresses :: [String]
        streetAddresses = fmap streetFromAddressTree addressTrees

detailsFromListingTree :: TagTreePos String -> [(PropertyDetails, Maybe String)]
detailsFromListingTree listingTree =
    extractedDetails
    where
        projectChildTrees :: [TagTreePos String]
        projectChildTrees = select projectChildSelector (content listingTree)

        extractedDetails :: [(PropertyDetails, Maybe String)]
        extractedDetails = if not (null projectChildTrees) then
                processProjectChildren $ head projectChildTrees
            else
                [processSingleProperty listingTree]

processSingleProperty :: TagTreePos String -> (PropertyDetails, Maybe String)
processSingleProperty propertyTree =
    (propertyDetails, propertyPrice)
    where
        propertyDetails :: PropertyDetails
        propertyDetails = extractPropertyDetails propertyTree

        propertyPrice :: Maybe String
        propertyPrice = extractSinglePropertyPrice propertyTree

processProjectChildren :: TagTreePos String -> [(PropertyDetails, Maybe String)]
processProjectChildren projectChildTree =
    processedProjectChildren
    where
        childrenTrees :: [TagTreePos String]
        childrenTrees = select projectChildrenSelector (content projectChildTree)

        processedProjectChildren :: [(PropertyDetails, Maybe String)]
        processedProjectChildren = fmap processProjectChild childrenTrees

processProjectChild :: TagTreePos String -> (PropertyDetails, Maybe String)
processProjectChild projectChild =
    (propertyDetails, propertyPrice)
    where
        propertyDetails :: PropertyDetails
        propertyDetails = extractPropertyDetails projectChild

        propertyPrice :: Maybe String
        propertyPrice = extractProjectChildPrice projectChild

extractPrice :: String -> TagTreePos String -> Maybe String
extractPrice selector tree =
    if not (null priceTree) then
        Just $ innerText $ flattenTree [content $ head priceTree]
    else
        Nothing
    where
        priceTree :: [TagTreePos String]
        priceTree = select (sel selector) (content tree)

extractProjectChildPrice :: TagTreePos String -> Maybe String
extractProjectChildPrice = extractPrice ".price"

extractSinglePropertyPrice :: TagTreePos String -> Maybe String
extractSinglePropertyPrice = extractPrice ".priceText"

iconToValue :: [TagTreePos String] -> Maybe String
iconToValue [] = Nothing
iconToValue (x:xs) =
    Just $ innerText $ flattenTree [Text.HTML.TagSoup.Tree.Zipper.after x !! 1]

extractPropertyDetails :: TagTreePos String -> PropertyDetails
extractPropertyDetails propertyTree =
    PropertyDetails {bedrooms = bedrooms, bathrooms = bathrooms, cars = cars}
    where
        bedrooms = iconToValue $ select bedroomIconSelector (content propertyTree)
        bathrooms = iconToValue $ select bathIconSelector (content propertyTree)
        cars = iconToValue $ select carIconSelector (content propertyTree)


streetFromAddressTree :: TagTreePos String -> String
streetFromAddressTree addressTree =
    innerText tagList
    where
        tagList :: [Tag String]
        tagList = flattenTree [content addressTree]

propertyFromListingTree :: TagTreePos String -> [Property]
propertyFromListingTree listingTree =
    []
    where
        propertyFeatures :: [TagTreePos String]
        propertyFeatures = propertyFeaturesTree listingTree

bedroomIconSelector :: Selector
bedroomIconSelector = sel ".rui-icon-bed"

bathIconSelector :: Selector
bathIconSelector = sel ".rui-icon-bath"

carIconSelector :: Selector
carIconSelector = sel ".rui-icon-car"

addressSelector :: Selector
addressSelector = sel "a[rel=listingName]"

listingSelector :: Selector
listingSelector = sel "article.resultBody"

listingInfoSelector :: Selector
listingInfoSelector = sel "div.listingInfo"

projectChildSelector :: Selector
projectChildSelector = sel "div.project-child-listings"

projectChildrenSelector :: Selector
projectChildrenSelector = sel "div.child"

listings :: TagTree String -> [TagTreePos String]
listings = select listingSelector

propertyFeaturesSelector :: Selector
propertyFeaturesSelector = sel "dl.rui-property-features"

propertyFeaturesTree :: TagTreePos String -> [TagTreePos String]
propertyFeaturesTree listingTree = select propertyFeaturesSelector (content listingTree)

hasListings :: TagTree String -> Bool
hasListings tree = not (null (select (sel "article.resultBody") tree))

listingInfoToPropertyFeatures :: [Tag String] -> [[Tag String]]
listingInfoToPropertyFeatures tags = tail $ splitWhen propertyFeatures tags

propertyFeatures :: Tag String -> Bool
propertyFeatures tag = tag ~== TagOpen "dl" []
    && fromAttrib "class" tag =~ "rui-property-features"

listingInfo :: Tag String -> Bool
listingInfo tag = tag ~== TagOpen "article" []
    && fromAttrib "class" tag =~ "resultBody"
