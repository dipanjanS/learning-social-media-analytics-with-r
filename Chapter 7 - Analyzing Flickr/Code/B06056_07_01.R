############################################################################
# Chapter      :   7
# Description  :   Use R packages to connect to Flickr app and extract
#                  sample data
############################################################################


library(httr)
library(plyr)
library(dplyr)
library(rlist)
library(pipeR)  
library(jsonlite)

# App Credentials
api_key <- "XXXXXXXXXXXXXXXXXXXXXXXXX"
secret <- "XXXXXXXXXXXXXXXXXX"


# Create connection object
flickr.app <- oauth_app("Flickr Sample App",api_key,secret)


flickr.endpoint <- oauth_endpoint(
  request = "https://www.flickr.com/services/oauth/request_token"
  , authorize = "https://www.flickr.com/services/oauth/authorize"
  , access = "https://www.flickr.com/services/oauth/access_token"
)

# connect using OAuth
tok <- oauth1.0_token(
  flickr.endpoint
  , flickr.app
  , cache = F
)


# get interesting photos from yesterday
raw_sample_data <- GET(url=sprintf(
  "https://api.flickr.com/services/rest/?method=flickr.interestingness.getList&api_key=%s&date=%s&format=json&nojsoncallback=1"
  , api_key
  , format( Sys.Date()-1, "%Y-%m-%d")
  , tok$credentials$oauth_token
  )
  )
  
# check the data type of the response
typeof(raw_sample_data)

# extract relevant photo data 
processed_sample_data <- raw_sample_data %>>% 
                          content(as="text") %>>% 
                          jsonlite::fromJSON ()%>>%
                          ( data.frame(
                            date = format( Sys.Date() - i, "%Y-%m-%d")
                            ,.
                            ,stringsAsFactors=F
                          ))


# Utility function to extract data from Flickr interestingess
getInterestingData <- function(days){
  GET(url=sprintf(
    "https://api.flickr.com/services/rest/?method=flickr.interestingness.getList&api_key=%s&date=%s&format=json&nojsoncallback=1"
    , api_key
    , format( Sys.Date() - days, "%Y-%m-%d")
    , tok$credentials$oauth_token
  )
  )%>>%
    content( as = "text" ) %>>%
    jsonlite::fromJSON () %>>%
    ( .$photos$photo ) %>>%
    ( data.frame(
      date = format( Sys.Date() - days, "%Y-%m-%d")
      ,.
      ,stringsAsFactors=F
    )) %>>%
    return
}


# extract multiple days worth of data
daysAnalyze = 3
  
raw_sample_data <- lapply(1:daysAnalyze,getInterestingData) %>>%
                    ( do.call(rbind, .) )

  