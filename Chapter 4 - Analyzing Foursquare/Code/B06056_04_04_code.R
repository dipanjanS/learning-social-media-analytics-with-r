############################################################################
# Chapter      :   4
# Objective    :   Graph data extraction and preceeding analysis
# Note         :   Please make sure all required files are in the same
#                  as the script
############################################################################
library(dplyr)
library(ggplot2)
library(reshape2)
library(RCurl)
library(jsonlite)
library(magrittr)
library(tidyjson)
library(geosphere)
library(readr)
library(igraph)

############################################################################
#       Utility Functions
############################################################################
extract_venue_details <- function(venue_id){
  venue_detail_uri_template <- "https://api.foursquare.com/v2/venues/%s?v=20131016&client_id=%s&client_secret=%s"
  token="DUS3ZEILSWJJ12P3Y1C4O0VKZYSEX3WMUHLP5Y1GU4FMEOMW"
  secret="PWVK5E3EQR2JILSOSTYNLWJYZQGBQJCXC3YEHFCEFAXNXU4J"
  apiurl<- sprintf(venue_detail_uri_template, venue_id,token,secret)
  json_res <- getURL(apiurl, .mapUnicode=TRUE)
  json_response <- as.data.frame(json_res %>% enter_object("meta") %>% spread_values(result = jnumber("code")))["result"]
  if (json_response == 200){
    stats_df <- json_res %>% 
      enter_object("response") %>% 
      enter_object("venue") %>% 
      spread_values(venue.name = jstring("name"), venue.id = jstring("id")) %>% 
      enter_object("stats") %>% 
      spread_values(venue.checkins = jnumber("checkinsCount"),venue.users = jnumber("usersCount"),venue.tips = jnumber("tipCount")) 
    stats_df$document.id <- NULL
    location_df <- json_res %>% 
      enter_object("response") %>% 
      enter_object("venue") %>% 
      enter_object("location") %>%
      spread_values(venue.lat = jnumber("lat"), venue.long = jnumber("lng"))
    location_df$document.id <- NULL
    price_df <- json_res %>% 
      enter_object("response") %>% 
      enter_object("venue") %>% 
      enter_object("price") %>%
      spread_values(venue.tier = jnumber("tier"), venue.price.msg = jstring("message"))
    price_df$document.id <- NULL
    category_df <- json_res %>% 
      enter_object("response") %>% 
      enter_object("venue") %>% 
      enter_object("categories") %>% 
      gather_array() %>% 
      spread_values(venue_category_id = jstring("id"))
    category_df$document.id <- NULL
    category_df$array.index <- NULL
    if (nrow(price_df)> 0){
      venue_df <- cbind(stats_df,location_df, price_df,category_df[1,])
    } else {
      price_df <- as.data.frame(cbind('NA', 'NA'))
      colnames(price_df) <- c("venue.tier", "venue.price.msg")
      venue_df <- cbind(stats_df,location_df,price_df,category_df[1,])
    }
    
    return(venue_df)
  } else{
    print("Unsuccessful API call")
    return(FALSE)
  }
}
extract_next_venue_details<- function(source_venue_id){
  next_venue_url_template = "https://api.foursquare.com/v2/venues/%s/nextvenues?v=20131016&client_id=%s&client_secret=%s"
  apiurl<- sprintf(next_venue_url_template, source_venue_id,token,secret)
  json_res <- getURL(apiurl, .mapUnicode=TRUE)
  json_response <- as.data.frame(json_res %>% enter_object("meta") %>% spread_values(result = jnumber("code")))["result"]
  if (json_response == 200){
    stats_df <- json_res %>% 
      enter_object("response") %>% 
      enter_object("nextVenues") %>% 
      enter_object("items") %>% 
      gather_array() %>%
      spread_values(venue.name = jstring("name"), venue.id = jstring("id")) %>% 
      enter_object("stats") %>% 
      spread_values(venue.checkins = jnumber("checkinsCount"),venue.users = jnumber("usersCount"),venue.tips = jnumber("tipCount")) 
    stats_df$document.id <- NULL
    stats_df$array.index <- NULL
    location_df <- json_res %>% 
      enter_object("response") %>% 
      enter_object("nextVenues") %>% 
      enter_object("items") %>% 
      gather_array() %>%
      enter_object("location") %>%
      spread_values(venue.lat = jnumber("lat"), venue.long = jnumber("lng"))
    location_df$document.id <- NULL
    location_df$array.index <- NULL
    category_df <- json_res %>% 
      enter_object("response") %>% 
      enter_object("nextVenues") %>% 
      enter_object("items") %>% 
      gather_array() %>% 
      enter_object("categories") %>% 
      gather_array() %>% 
      spread_values(venue_category_id = jstring("id"))
    category_df = category_df[category_df$array.index == 1, ]
    category_df$document.id <- NULL
    category_df$array.index <- NULL
    price_df <- as.data.frame(cbind('NA', 'NA'))
    colnames(price_df) <- c("venue.tier", "venue.price.msg")
    next_venue_df <- cbind(stats_df,location_df,price_df,category_df)
    return(next_venue_df)
  } else{
    print("Unsuccessful API call")
    return(FALSE)
  }
}
extract_dfs_data <- function(venues_to_crawl, depth = 10){
  venues_crawled = c()
  venue_details_df = data.frame()
  new_venues_to_crawl = c()
  edge_list_df = data.frame()
  venue_list = c()
  for (i in 1: depth){
    print(paste( "depth is  ", i, ": ",length(venues_crawled)," locations and ",nrow(edge_list_df)," links. ", length(venues_to_crawl)," venues to go."))
    for (venue in venues_to_crawl){
      if(!(venue %in% venues_crawled)){
        possibleError <- tryCatch( df <- extract_venue_details(venue),error=function(e) e)
        if(!inherits(possibleError, "error")){
          out_df <- df
        } else {
          print("Skipped a point")
        }
        venue_list <- c(venue_list, out_df[,"venue.id"])
        venue_details_df <- rbind(venue_details_df, out_df)
        possibleError <- tryCatch( df <- extract_next_venue_details(venue),error=function(e) e)
        if(!inherits(possibleError, "error")){
          next_venues <- df
        } else {
          print("Skipped a point")
        }
        for (i in 1: nrow(next_venues)){
          new_venue_id = next_venues[i,"venue.id"]
          if(!( new_venue_id %in% venue_list)){
            possibleError <- tryCatch( df <- extract_venue_details(new_venue_id),error=function(e) e)
            if(!inherits(possibleError, "error")){
              out_df <- df
            } else {
              print("Skipped a point")
            }
            venue_list <- c(venue_list, new_venue_id)
            venue_details_df <- rbind(venue_details_df, out_df)
          }
          if((!(new_venue_id %in% venues_crawled)) && !(new_venue_id %in% venues_to_crawl) && !(new_venue_id %in% new_venues_to_crawl)){
            new_venues_to_crawl <- c(new_venues_to_crawl, new_venue_id)
          }
          edge <- cbind(venue, new_venue_id)
          colnames(edge) <- c("NodeFrom", "NodeTo")
          print(edge)
          edge_list_df <- rbind(edge_list_df, edge)
          colnames(edge_list_df) <- c("NodeFrom", "NodeTo")
        }
      }
      venues_crawled <- c(venues_crawled, venue)
    } 
    venues_to_crawl <- new_venues_to_crawl
    write.csv2(edge_list_df, file = "edges_list.csv", row.names = FALSE)
    write.csv2(venue_details_df, file = "venue_details.csv", row.names = FALSE)
  }
  write.csv2(edge_list_df, file = "edges_list_final.csv", row.names = FALSE)
  write.csv2(venue_details_df, file = "venue_details_final.csv", row.names = FALSE)
}
get_distances_between_nodes <- function(x){
  source_lat_long <- venue_details_final[venue_details_final$venue.id == x[1],c("venue.lat","venue.long")]
  source_lat_long <-c(return_mantissa(source_lat_long$venue.lat),return_mantissa(source_lat_long$venue.long))
  tail_lat_long <- venue_details_final[venue_details_final$venue.id == x[2],c("venue.lat","venue.long")]
  tail_lat_long <-c(return_mantissa(tail_lat_long$venue.lat),return_mantissa(tail_lat_long$venue.long))
  length_node <-distCosine(source_lat_long, tail_lat_long)
  return(length_node)
}
return_mantissa <- function(value){
  a<-strsplit(format(value, scientific=T),"e")[[1]] 
  return(as.numeric(a[1])*10)
}

# Graph data extraction 

jfk_id = "43a52546f964a520532c1fe3"
statue_ofLiberty_id = "42893400f964a52054231fe3"
central_park_id = "412d2800f964a520df0c1fe3"
venues_to_crawl = c(jfk_id,statue_ofLiberty_id,central_park_id)
extract_dfs_data(venues_to_crawl, depth = 10)

edges_list_final <- read_delim("edges_list_final.csv",";", escape_double = FALSE, trim_ws = TRUE)
venue_details_final <- read_delim("venue_details_final.csv",",", escape_double = FALSE, trim_ws = TRUE)

# Clean up the edge list to remove nodes for information was not extracted

venue_details_final <- venue_details_final[!duplicated(venue_details_final$venue.id),]
edges_list_final <-edges_list_final[edges_list_final$NodeFrom %in% venue_details_final$venue.id,]
edges_list_final <-edges_list_final[edges_list_final$NodeTo %in% venue_details_final$venue.id,]
edges_list_final$distance <- apply(edges_list_final, 1, get_distances_between_nodes)

# Most visited locations in New York
prominent_next_venues <- edges_list_final %>% 
             group_by(NodeTo) %>% 
             summarise(avg_distance_from_last_venue = mean(distance),num_targets = n()) %>%
             arrange(desc(num_targets)) %>%
             top_n(10)
colnames(prominent_next_venues)[1]<- "venue.id"

prominent_next_venues <- prominent_next_venues %>% 
                         inner_join(venue_details_final) %>% 
                         select(venue.id, venue.name, avg_distance_from_last_venue, num_targets)
