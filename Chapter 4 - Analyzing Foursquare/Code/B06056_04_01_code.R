############################################################################
# Chapter      :   4
# Description  :   Connect to Foursquare and collect all the categories 
############################################################################
library(RCurl)
library(jsonlite)
library(magrittr)
library(tidyjson)
library(geosphere)


#Sequence for extracting all categories in foursquare
json_res <- NULL

# Initialize the necessary authorization information
token="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
secret="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

#Create the necessary API endpoint URL
uriTemplate <-"https://api.foursquare.com/v2/venues/categories?v=20131016&client_id=%s&client_secret=%s"

#Update the API URL with authorization information
apiurl<- sprintf(uriTemplate, token,secret)

#Call the end point for JSON response
json_res <- getURL(apiurl, .mapUnicode=TRUE)

#Parsing the JSON object for extracting the data frame
category_df <- as.data.frame(json_res %>% enter_object("response") 
                             %>% enter_object("categories") 
                             %>% gather_array() 
                             %>% spread_values(super_cat_id = jstring("id"), super_cat_name = jstring("name")) 
                             %>% enter_object("categories") 
                             %>% gather_array() 
                             %>% spread_values(cat_id = jstring("id"), cat_name = jstring("name"), cat_pluralName = jstring("pluralName"), cat_shortName = jstring("shortName"))
                             %>% enter_object("categories") 
                             %>% gather_array() 
                             %>% spread_values(cat_id2 = jstring("id"), cat_name2 = jstring("name"), cat_pluralName2 = jstring("pluralName"), cat_shortName2 = jstring("shortName")))

#Removing extra variables from the data frame
category_df$array.index <- NULL
category_df$document.id <- NULL

