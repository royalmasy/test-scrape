{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel.Core
import Control.Monad
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)

mainMember = do
  htmlData <- readFile "gbsitemap.txt"
  --I want to find links in this file containing the text "/member/"
  let memList = scrapeStringLike htmlData memSearch
  return (memList)
--memSearch :: Scraper String String
memSearch =
    chroots ("url" // "loc") $ do
        contents <- text anySelector
        guard ("/member/" `isInfixOf` contents)
        text anySelector


data BusinessData = BusinessData
    { catSearch        :: Maybe String
    , nameSearch       :: Maybe String
    , aboutSearch      :: Maybe String
    , streetAddress    :: Maybe String
    , addressLocality  :: Maybe String
    , addressRegion    :: Maybe String
    , postalCode       :: Maybe String
    , linkSearch       :: Maybe String
    , phoneSearch1     :: Maybe String
    , phoneSearch2     :: Maybe String
    , faxSearch2       :: Maybe String
    , repTitleSearch   :: Maybe String
    , repNameSearch    :: Maybe String
    , repPhone         :: Maybe String
    }  deriving (Eq, Show)

main :: IO ()
main = do
  htmlData <- readFile "grandblanc.txt"
  let allFields = scrapeStringLike htmlData (do

      catSearch        <- texts ("ul" @: [hasClass "mn-member-cats"] // "li" )
      nameSearch       <- texts ("div" @: [hasClass "mn-section-content"] // "div")
      aboutSearch      <- texts ("div" @: [hasClass "mn-section-content"] // "p")

      streetAddress    <- texts ("div" @: [hasClass "mn-address1"])
      addressLocality  <- texts ("span" @: [hasClass "mn-cityspan"])
      addressRegion    <- texts ("span" @: [hasClass "mn-stspan"])
      postalCode       <- texts ("span" @: [hasClass "mn-zipspan"])

      linkSearch      <- attrs "href" ("div" @: [hasClass "mn-memberinfo-block-actions"] // "ul" // "li" // "a")
      phoneSearch1    <- texts ("div" @: [hasClass "mn-member-phone1"])
      phoneSearch2    <- texts ("div" @: [hasClass "mn-member-phone2"])
      faxSearch2      <- texts ("div" @: [hasClass "mn-member-fax"])

      repTitleSearch  <- texts ("span" @: [hasClass "mn-rep-prefix"])
      repNameSearch   <- texts ("span" @: [hasClass "mn-rep-fullname"])
      repPhone        <- chroots ("div" @: [hasClass "mn-member-repphone"] // "span") $ do
        contents <- text anySelector
        guard ("-" `isInfixOf` contents)
        text anySelector

      return (BusinessData
             (listToMaybe catSearch)
             (listToMaybe nameSearch)
             (listToMaybe aboutSearch)
             (listToMaybe streetAddress)
             (listToMaybe addressLocality)
             (listToMaybe addressRegion)
             (listToMaybe postalCode)
             (listToMaybe linkSearch)
             (listToMaybe phoneSearch1)
             (listToMaybe phoneSearch2)
             (listToMaybe faxSearch2)
             (listToMaybe repTitleSearch)
             (listToMaybe repNameSearch)
             (listToMaybe repPhone))
        )
  print (allFields)
