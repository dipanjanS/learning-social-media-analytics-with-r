############################################################################
# Chapter      :   7
# Description  :   Exploring Flickr Data
############################################################################


library(httr)
library(plyr)
library(dplyr)
library(rlist)
library(pipeR)  
library(stringr)
library(ggplot2)
library(reshape2)
library(corrplot)
library(jsonlite)
library(lubridate)


############################################################################
# Flickr APP Auth
############################################################################

# App Credentials
api_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
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


############################################################################
# Utility methods
############################################################################


# utility function to get first element of string and convert to numeric
# this is used to preprocess focal length attribute
get_first <- function(y) as.numeric(strsplit(y," ")[[1]][1])


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

# utility function to get EXIF for an image.
getEXIF<- function(image){  
  exif <-
    GET(url=sprintf(
      "https://api.flickr.com/services/rest/?method=flickr.photos.getExif&api_key=%s&photo_id=%s&secret=%s&format=json&nojsoncallback=1"
      , api_key
      , interestingDF[image,"id"]
      , interestingDF[image,"secret"]
    )
    ) %>>%
    content( as = "text" ) %>>%
    jsonlite::fromJSON ()
}


# get tag and view counts using getInfo endpoint
getInfo<- function(image){  
  tag <-
    GET(url=sprintf(
      "https://api.flickr.com/services/rest/?method=flickr.photos.getInfo&api_key=%s&photo_id=%s&secret=%s&format=json&nojsoncallback=1"
      , api_key
      , interestingDF[image,"id"]
      , interestingDF[image,"secret"]
    )
    ) %>>%
    content( as = "text" ) %>>%
    jsonlite::fromJSON ()
}

############################################################################
# Extract Interesting Images
############################################################################

# extract multiple days worth of data
daysAnalyze = 10
  
interestingDF <- lapply(1:daysAnalyze,getInterestingData) %>>%
                    # combine all the days into a data frame
                    ( do.call(rbind, .) )



############################################################################
# Extract View Counts using API
############################################################################

# get exif information for each image
exifData <- lapply(1:nrow(interestingDF),
                   getEXIF)


# Use EXIF Data to get specific attributes
# ISO
iso_list <- as.numeric(exifData %>>%
                         list.map(as.numeric(
                           as.data.frame(.$photo$exif)[
                             which(.$photo$exif["label"]=="ISO Speed"),
                             "raw"])
                           
                         )
)

# Manufacturer/Make
make_list <- exifData %>>% 
                  list.map(unlist(
                    as.data.frame(
                      .$photo$exif)[
                        which(.$photo$exif["label"]=="Make"),
                        "raw"] )[1] %>>% as.character
                  )%>>% as.character
make_list <- ifelse(make_list=="character(0)",
                    NA,
                    make_list)                         



# Image Focal Length
focal_list <- exifData %>>%
  list.map(
    unlist(
      as.data.frame(.$photo$exif)[
        which(.$photo$exif["label"]=="Focal Length"),
        "raw"] )[1]
    
  )%>>% as.character

focal_list <- ifelse(focal_list=="NULL",
                     NA,
                     focal_list)
focal_list <- unlist(lapply(focal_list,
                            get_first))


# Image White Balance
whiteBalance_list<-exifData %>>% 
  list.map(
    unlist(
      as.data.frame(.$photo$exif)[
        which(.$photo$exif["label"]=="White Balance"),
        "raw"] )[1] %>>% as.character
  ) %>>% as.character
whiteBalance_list <- ifelse(whiteBalance_list=="character(0)",
                            NA,
                            whiteBalance_list)


# Image Metering Mode
meteringMode_list <- exifData %>>% 
  list.map(
    unlist(
      as.data.frame(.$photo$exif)[
        which(.$photo$exif["label"]=="Metering Mode"),
        "raw"] )[1] %>>% as.character
  )%>>% as.character

meteringMode_list <- ifelse(meteringMode_list=="character(0)",
                            NA,
                            meteringMode_list)





############################################################################
# Extract View Counts using API
############################################################################
tagData <- lapply(1:nrow(interesting),
                  getInfo)


# get tags
tags<-tagData[[1]]$photo$tags$tag


# Image View Count
views_list <- as.numeric(tagData %>>% 
                           list.map(
                             unlist(.$photo$views)) %>>% 
                           as.character)


############################################################################
# Extend base dataframe with additional attributes
############################################################################

# Add attributes to main data frame
interestingDF$iso <- iso_list
interestingDF$make <- make_list
interestingDF$focal_length <- focal_list
interestingDF$white_balance <- whiteBalance_list
interestingDF$metering_mode <- meteringMode_list
interestingDF$views <- views_list


############################################################################
# Descriptive analysis
############################################################################

summary(interestingDF[,c('iso',
                       'make',
                       'focal_length',
                       'white_balance',
                       'metering_mode',
                       'views')])



update_geom_font_defaults(font_rc_light)

# Plot images counts by make
ggplot(interestingDF[!is.na(interestingDF$make),], 
       aes(make)) +
  geom_bar() + 
  coord_flip() +
  labs(x="Make", y="Image Counts",
       title="Make wise Image distribution"
        ) + 
  theme_ipsum_rc(grid="XY")


# metering mode
ggplot(interestingDF[!is.na(interestingDF$metering_mode),], 
       aes(metering_mode)) +
  geom_bar() + 
  coord_flip() +
  labs(x="Metering Mode", y="Image Counts",
       title="Metering Mode wise Image distribution"
  ) + 
  theme_ipsum_rc(grid="XY")


# focal length
ggplot(interestingDF[!is.na(interestingDF$focal_length),], aes(focal_length)) +
  geom_bar(fill = "aquamarine4") + 
  ylab("Number of images") + 
  ggtitle("Image counts by Focal Length") + theme_ipsum_rc(grid="XY")
