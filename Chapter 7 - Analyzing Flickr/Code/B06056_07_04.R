############################################################################
# Chapter      :   7
# Description  :   Classifying Flickr Data using Random Forests and SVM
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

library(pROC)
library(e1071)
library(caret)



############################################################################
# Flickr APP Auth
############################################################################

# App Credentials
api_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
secret <- "XXXXXXXXXXXXXXXXXXXXXXXX"


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

# get first entry from a space separated string
get_first <- function(y){
  tryCatch(
    as.numeric(strsplit(y," ")[[1]][1])
    , warning = function(w) {
      0 #handle errors by using 0
    }, error = function(e) {
      0 #handle errors by using 0
    })
}

# extract photos of given user_id
# use flickr.people.getpublicphotos
getPhotosFromFlickr <- function(api_key,
                                token,
                                user_id){
  GET(url=sprintf(
    "https://api.flickr.com/services/rest/?method=flickr.people.getpublicphotos&api_key=%s&user_id=%s&format=json&nojsoncallback=1"
    , api_key
    , user_id
    , token$credentials$oauth_token
  )
  ) %>>%
    content( as = "text" ) %>>%
    jsonlite::fromJSON () %>>%
    ( .$photos$photo ) %>>%
    ( data.frame(
      .
      ,stringsAsFactors=F
    ))
}


# get EXIF data for each photo in a given dataframe
# use flickr.photos.getExif
getEXIF <- function(api_key,
                    photosDF){
  lapply(
    1:nrow(photosDF)
    ,function(photo){  
      exif <-
        GET(url=sprintf(
          "https://api.flickr.com/services/rest/?method=flickr.photos.getExif&api_key=%s&photo_id=%s&secret=%s&format=json&nojsoncallback=1"
          , api_key
          , photosDF[photo,"id"]
          , photosDF[photo,"secret"]
        )
        ) %>>%
        content( as = "text" ) %>>%
        jsonlite::fromJSON ()
    }
  )
}


# get view counts for each photo in given dataframe
# use flickr.photos.getInfo
getViewCounts <- function(api_key,
                          photosDF){
  photos.tagData <- lapply(
    1:nrow(photosDF)
    ,function(photo){  
      tag <-
        GET(url=sprintf(
          "https://api.flickr.com/services/rest/?method=flickr.photos.getInfo&api_key=%s&photo_id=%s&secret=%s&format=json&nojsoncallback=1"
          , api_key
          , photosDF[photo,"id"]
          , photosDF[photo,"secret"]
        )
        ) %>>%
        content( as = "text" ) %>>%
        jsonlite::fromJSON ()
    }
  )
  
  
  
  # Image View Count
  as.numeric(photos.tagData %>>% list.map(unlist(.$photo$views)) %>>% as.character)
}


# extract ISO from EXIF data
extractISO<-function(photos.exifData){
  as.numeric(photos.exifData %>>%
               list.map(as.numeric(
                 as.data.frame(.$photo$exif)[
                   which(.$photo$exif["label"]=="ISO Speed"),
                   "raw"])
               )
              )
}

# extract Make/Manufacturer from EXIF
extractMakes<-function(photos.exifData){
  
  make_list<-photos.exifData %>>% 
    list.map(unlist(
      as.data.frame(
        .$photo$exif)[
          which(.$photo$exif["label"]=="Make"),
          "raw"] )[1] %>>% as.character
    )%>>% as.character
  
  make_list <- ifelse(make_list=="character(0)",NA,make_list)
}


# extract Focal Length from EXIF
extractFocalLength<-function(photos.exifData){
  focal_list <- photos.exifData %>>%
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
}

# extract White Balance from EXIF
extractWB<-function(photos.exifData){
  whiteBalance_list <- photos.exifData %>>% 
    list.map(
      unlist(
        as.data.frame(.$photo$exif)[
          which(.$photo$exif["label"]=="White Balance"),
          "raw"] )[1] %>>% as.character
    ) %>>% as.character
  
  whiteBalance_list <- ifelse(whiteBalance_list=="character(0)",
                              NA,
                              whiteBalance_list)
}


# extract Metering Mode from EXIF
extractMeteringMode <- function(photos.exifData){
  meteringMode_list <- photos.exifData %>>% 
    list.map(
      unlist(
        as.data.frame(.$photo$exif)[
          which(.$photo$exif["label"]=="Metering Mode"),
          "raw"] )[1] %>>% as.character
    )%>>% as.character
  
  meteringMode_list <- ifelse(meteringMode_list=="character(0)",
                              NA,
                              meteringMode_list)
}


# clean up camera makes
mapCamMakes <- function(x) {
  if(!is.na(str_match(tolower(x),"canon"))){
    "canon"
  }else if(!is.na(str_match(tolower(x),"nikon"))){
    "nikon"
  }else if(!is.na(str_match(tolower(x),"fuji"))){
    "fujifilm"
  }else if(!is.na(str_match(tolower(x),"leica")) ){
    "leica"
  }else if(!is.na(str_match(tolower(x),"olympus"))){
    "olympus"
  }else {
    tolower(x)
  }
}



# Get Photos for given user_id
getUserPhotos <- function(api_key,
                          token,
                          user_id){
  
  #get user's photos in a dataframe
  photosDF <- getPhotosFromFlickr(api_key,token,user_id)
  
  # get exif for each photo in dataframe
  photos.exifData <- getEXIF(api_key,photosDF)
  

  # Image ISO
  iso_list <- extractISO(photos.exifData)
  
  # Image Manufacturer/Make
  make_list <- extractMakes(photos.exifData)                         
  
  # Image Focal Length
  focal_list <- extractFocalLength(photos.exifData)
  
  # Image White Balance
  whiteBalance_list<-extractWB(photos.exifData)
  
  # Image Metering Mode
  meteringMode_list <- extractMeteringMode(photos.exifData)
  
  
  # Add attributes to main data frame
  photosDF$iso <- iso_list
  photosDF$make <- make_list
  photosDF$focal_length <- focal_list
  photosDF$white_balance <- whiteBalance_list
  photosDF$metering_mode <- meteringMode_list
  
  # get view counts
  photosDF$views <-   getViewCounts(api_key,photosDF) 
  
  as.data.frame(photosDF)
}

# typecast dataframes for use by classifier
prepareClassifierDF <- function(classifyDF){
  
  # convert white balance to factor and then encode numeric
  classifyDF$white_balance <- as.factor(classifyDF$white_balance)
  
  # convert metering mode to factor
  classifyDF$metering_mode <- as.factor(classifyDF$metering_mode)
  
  # convert make_clean to factor
  classifyDF$make_clean <- as.factor(classifyDF$make_clean)
  
  as.data.frame(classifyDF)
}


############################################################################
# Prepare Dataset
############################################################################

# collect photos which have not featured on 
# Explore page

# get user_id specific data
mortal_userIDS <- c('XXXXXXXXXXX',
                    'XXXXXXXXXXX',
                    'XXXXXXXXXXX',
                    'XXXXXXXXXXX')

neg_interesting_df <- lapply(mortal_userIDS,
                          getUserPhotos,
                          api_key=api_key,token=tok) %>>%
                      ( do.call(rbind, .) )


neg_interesting_df <- na.omit(neg_interesting_df)
neg_interesting_df$make_clean <- sapply(neg_interesting_df$make,
                                        mapCamMakes)
neg_interesting_df$is_interesting <- 0

# Photos from Explore page
pos_interesting_df <- na.omit(interesting)
pos_interesting_df$is_interesting <- 1

# prepare overall dataset
classifyDF <- rbind(pos_interesting_df[,colnames(neg_interesting_df)],
                    neg_interesting_df)

# convert attributes to proper data types
classifyDF <- prepareClassifierDF(classifyDF)

# convert is_interesting to factor -> class target
classifyDF$is_interesting <- as.factor(classifyDF$is_interesting)



# restrict columns 
req_cols <- c('is_interesting',
             'iso',
             'focal_length',
             'white_balance',
             'metering_mode',
             'views',
             'make_clean')

classifyDF <- classifyDF[,req_cols]


############################################################################
# Prepare Train and Test Data Sets
############################################################################

# train - test split
set.seed(42)
samp <- sample(nrow(classifyDF), 0.6 * nrow(classifyDF))
train <- classifyDF[samp, ]
test <- classifyDF[-samp, ]



############################################################################
# Random Forest based Classifier
############################################################################

# train model
rfModel <- train(is_interesting ~ ., train,
                      preProcess = c("scale"),
                      tuneLength = 8,
                      trControl = trainControl(method = "cv"))

# Prediction
predictedProb <- predict(rfModel, test[,-1], type="prob") 

# Draw ROC curve.
resultROC <- roc(test$is_interesting, predictedProb$"1")

plot(resultROC, 
     print.thres="best", 
     print.thres.best.method="closest.topleft")

#to get threshold and accuracy
resultCoords <- coords(resultROC, 
                        "best", 
                        best.method="closest.topleft", 
                        ret=c("threshold", "accuracy"))
print(resultCoords)

# Confusion Matrix
confusionMatrix(test$is_interesting, 
                predict(rfModel, test[,-1]))



############################################################################
# SVM based Classifier
############################################################################

# standard svm
svm_model <- svm(is_interesting ~ ., data=train)
summary(svm_model)

# get model performance
pred <- predict(svm_model,test[,-1])
confusionMatrix(test$is_interesting, pred)


# Tune SVM and cross validate
svm_tune <- tune(svm, is_interesting ~ ., data=train,
                 ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

summary(svm_tune)

# get model performance
pred <- predict(svm_tune$best.model,test[,-1])
confusionMatrix(test$is_interesting, pred)


