############################################################################
# Chapter      :   4
# Objective    :   City data extraction and preceeding analysis
# Note         :   Please make sure all required files are in the same
#                  as the script
############################################################################
library(dplyr)
library(ggplot2)
library(recosystem)
library(reshape2)
library(RCurl)
library(jsonlite)
library(magrittr)
library(tidyjson)
library(geosphere)

token="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
secret="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

##################################DATA EXTRACTION CODE##########################################

############################################################################
#       Utility Functions
############################################################################

explore_around_point <- function(lat_long_matrix, radius = 500){
  #Function to get all options around the point represented by lat_long_matrix in a specified radius
  json_res <- NULL
  token="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  secret="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  uriTemplate <- "https://api.foursquare.com/v2/venues/explore?v=20131016&ll=%s%%2C%%20%s&radius=%s&limit=1000&openNow=0&client_id=%s&client_secret=%s"
  lat = lat_long_matrix[1,"lon"]
  long = lat_long_matrix[1,"lat"]
  apiurl<- sprintf(uriTemplate, long,lat,radius, token,secret)
  json_res <- getURL(apiurl, .mapUnicode=TRUE)
  # data frame containing venue and their corresponding stats
  json_response <- as.data.frame(json_res %>% enter_object("meta") %>% spread_values(result = jnumber("code")))["result"]
  if (json_response == 200){
    number_of_results <-as.data.frame(json_res %>% enter_object("response") %>% spread_values(result = jnumber("totalResults")))["result"]
    #print(sprintf("%s result recieved", number_of_results))
    DF1 <- json_res %>% enter_object("response") %>% enter_object("groups") %>% gather_array() %>% enter_object("items") %>% gather_array() %>% enter_object("venue") %>% spread_values(venue.name = jstring("name"), venue.id = jstring("id")) %>% enter_object("stats") %>% spread_values(venue.stats = jnumber("checkinsCount"), venue.usersCount = jnumber("usersCount"), venue.tipCount = jnumber("tipCount")) 
    DF1$array.index <- NULL
    DF1$document.id <- NULL
    #Data frame containing venue and their category id
    catDF1 <- json_res %>% enter_object("response") %>% enter_object("groups") %>% gather_array() %>% enter_object("items") %>% gather_array() %>% enter_object("venue") %>% spread_values(venue.name = jstring("name")) %>% enter_object("categories") %>% gather_array() %>% spread_values(venue.categoryid = jstring("id"))
    catDF1$array.index <- NULL
    catDF1$document.id <- NULL
    catDF1 <- as.data.frame(catDF1)
    DF1 <- as.data.frame(DF1)
    DF1[,"venue.categoryid"] = catDF1[,"venue.categoryid"]
    return(as.data.frame(DF1))
  } else {
    print(json_response)
    print("Unsuccessful API call")
    return(FALSE)
  }
}
span_a_arc <- function(lat_long_matrix, radius_level = 500, degree_step = 20, start_deg = 0, end_deg = 360){
  current_deg = start_deg
  points = c()
  while(current_deg != end_deg){
    d <- destPoint(lat_long_matrix, b=current_deg,d= radius_level*2)
    points <- rbind(points, d)
    current_deg = current_deg + degree_step
    #print(current_deg)
  }
  return(points)
}
get_data_for_points <- function(new_centers){
  out_df <- data.frame()
  for (i in 1:nrow(new_centers)) {
    #Try catch for skipping any error
    possibleError <- tryCatch( df <- explore_around_point(new_centers[i,,drop = FALSE]),error=function(e) e)
    if(!inherits(possibleError, "error")){
      out_df <- rbind(out_df,df)
      #print(nrow(out_df))
    } else {
      print("Skipped a point")
    }
  }
  return(out_df)
}

convert_user_percent_to_rating <- function(x){
  ranges <- quantile(x, seq(0,1,0.2))
  labs <- c(1)
  for(j in 1:length(ranges)-1){
    if(ranges[j] >0 && j >1){
      labs <-c(labs,j)
    }
  }
  ranges <- unique(ranges)
  rating_x <- cut(x, ranges, labels = labs)
  return(t(rating_x))
}

#City data extraction for Istanbul

#Get longitude latitude for Istanbul

istanbul_city_center = cbind(28.9784,41.0082)
colnames(istanbul_city_center) = c("lon","lat")

#Name the output csv file 

file_level1 = "istanbul_data_l1.csv"
file_level2 = "istanbul_final_data.csv"

#Function to repeatedly generate the new centres and get data for these centres
# Takes the number of levels we want to traverse as an argument

get_data_for_city <- function(city_center, levels_to_traverse = 2){
  new_centers_l1 = span_a_arc(city_center)
  out_df = data.frame()
  df_cent = explore_around_point(city_center)
  out_df = rbind(out_df, df_cent)
  df_l1 = get_data_for_points(new_centers_l1)
  out_df = rbind(out_df, df_l1)
  for(j in 1:levels_to_traverse){
    new_centers_lu = c()
    df_l2 = data.frame()
    for(i in 1:nrow(new_centers_l1)){
      new_cent = span_a_arc(new_centers_l1[i,], start_deg = -25, end_deg = 25, degree_step = 25)
      new_centers_lu = rbind(new_centers_lu, new_cent)
      df_l2 = get_data_for_points(new_centers_lu)
      sum_points = sum_points + nrow(new_centers_lu)
      new_centers_l1 = new_centers_lu
      out_df = rbind(out_df, df_l2)
      write.csv(out_df, file = file_level2, row.names = FALSE)
    }
  }
  write.csv(out_df, file = file_level2, row.names = FALSE)
  return(out_df)
}

# Function call for final data collection

final_data_istanbul = get_data_for_city(istanbul_city_center, levels_to_traverse = 2)

#Get the Category data extracted and join it with the city data frame
join_city_category <-function(city_df){
    four_sq_categories <- read.csv("foursquare_categories.csv", stringsAsFactors = FALSE)
    four_sq_categories2 <- read.csv("foursquare_categories2.csv", stringsAsFactors = FALSE)
    out_df <- inner_join(city_df, four_sq_categories2, by = c("venue.categoryid" = "cat_id2"))
    out_df2 <- inner_join(city_df, four_sq_categories, by = c("venue.categoryid" = "cat_id"))
    out_df[,c("cat_id","cat_name2", "cat_pluralName2", "cat_shortName2")] <- NULL
    out_df <- rbind(out_df, out_df2)
}

# Getting data for all the city we have extracted data for
city_names <- c("ny", "istanbul", "paris", "la", "seattle", "london", "chicago")
all_city_data_with_category <- data.frame()
for ( city in city_names){
    #combine all cities data and remove duplicated
    city_data <- read.csv(file = paste(city,"_final_data.csv", sep = ''),stringsAsFactors = FALSE)
    # Removing duplicated data points 
    city_data <- city_data[!duplicated(city_data$venue.id),]
    # Combining with the category ids
    city_data_with_category <- join_city_category(city_data)
    city_data_with_category["cityname"] <- city
    all_city_data_with_category <- rbind(all_city_data_with_category, city_data_with_category)
}

# Plot for city wise checkins

summary_df <- all_city_data_with_category %>% 
              group_by(cityname) %>% 
              summarise(total_checkins = sum(venue.stats), city_user= sum(venue.usersCount), city_tips = sum(venue.tipCount))

ggplot(summary_df, aes(x=cityname, y=total_checkins)) +  
  geom_bar(stat="identity") +
  ggtitle("City wise check-ins") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))

# Total category count for different cities

cat_rep_summary <- all_city_data_with_category %>% 
                  group_by(cityname) %>% 
                  summarise(city_category_count = n_distinct(cat_name))
ggplot(cat_rep_summary, aes(x=cityname, y=city_category_count), col(cityname))+  
  geom_bar(stat="identity")+
  ggtitle("Total categories for each city") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))

# Distribution of city check-ins across the major categories

super_cat_summary_detail <- all_city_data_with_category %>% 
    group_by(cityname,super_cat_name) %>% 
    summarise(city_category_count = sum(venue.stats)) %>%
    mutate(category_percentage = city_category_count/sum(city_category_count))

# For brevity we will only plot the two cities with most check-ins
p5 <- ggplot(subset(super_cat_summary_detail, cityname %in% c("ny", "istanbul")), aes(x=super_cat_name, y=category_percentage))
(p5 <- p5 + geom_bar(stat="identity") + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
        facet_wrap(~cityname, ncol = 1)) +
  ggtitle("category distribution for NY and Istanbul") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))

# Top 10 category distribution for each city
cat_summary_detail <- all_city_data_with_category %>% 
    group_by(cityname,cat_name) %>% 
    summarise(city_category_count = sum(venue.stats)) %>% 
    mutate(category_percentage = city_category_count/sum(city_category_count)) %>%
    top_n(5)
p5 <- ggplot(cat_summary_detail, aes(x=cat_name, y=category_percentage))
(p5 <- p5 + geom_bar(stat="identity") + ylab("Check-in percentage") + xlab("Categories") +
       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      facet_wrap(~cityname, ncol = 1)) +
  ggtitle("category distribution for all cities") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))


##################################Recommender system##########################################

# Select the relVeant data and subset for "Food" Category
selected_cols <- c("cityname", "cat_name", "venue.stats")
reco_data <- all_city_data_with_category[all_city_data_with_category$super_cat_name == "Food",selected_cols]
# Change the format of data
reco_data_reshaped <- reshape(reco_data, 
                              timevar = "cat_name",
                              idvar = c("cityname"),
                              direction = "wide")
# Substitute the NAs with 0
reco_data_reshaped[is.na(reco_data_reshaped)] <- 0
reco_final <- cbind(reco_data_reshaped[,"cityname"],round(reco_data_reshaped[,(!colnames(reco_data_reshaped) %in% c("cityname"))]/rowSums(reco_data_reshaped[,(!colnames(reco_data_reshaped) %in% c("cityname"))]), 4))
colnames(reco_final)[1] <- "cityname"

# Converting percents into ratings and recosystem required format
reco_ratings <- apply(reco_final[,!(colnames(reco_final) %in% c("cityname"))], 1, convert_user_percent_to_rating)
reco_ratings <- t(reco_ratings)
city_id_name_map <- as.factor(reco_final[,"cityname"])
colnames(reco_ratings) <- colnames(reco_final[,(!colnames(reco_final) %in% c("cityname"))])
reco_ratings <- cbind(as.data.frame(reco_final[,"cityname"]), reco_ratings)
colnames(reco_ratings)[1] <- "cityname"
rest_type_id = colnames(reco_final)[!(colnames(reco_ratings) %in% c("cityname"))]
reco_rating_long <-reshape(reco_ratings,varying = !(colnames(reco_final) %in% c("cityname")), v.names = "rating", direction = "long")
reco_rating_long[,"id"] <- NULL
colnames(reco_rating_long)[2] <- "restaurant_type"
reco_rating_long$cityname <-as.numeric(reco_rating_long$cityname)

# Seperate the training data and predcition data and persist them as files
reco_rating_long_predict <- reco_rating_long[is.na(reco_rating_long$rating),]
reco_rating_long_train <- reco_rating_long[!is.na(reco_rating_long$rating),]
write.table(reco_rating_long_train, file = "restaurant_reco_train.txt", quote = FALSE,sep = " ", row.names = FALSE, col.names = FALSE)
write.table(reco_rating_long_predict, file = "restaurant_reco_test.txt", quote = FALSE,sep = " ", row.names = FALSE, col.names = FALSE)

# Train a recommendation system using recosystem
r = Reco()
train_set = data_file("restaurant_reco_train.txt")
opts = r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                     costp_l1 = 0, costq_l1 = 0,
                                     nthread = 1, niter = 10))
r$train(train_set, opts = c(opts$min, nthread = 1, niter = 50))
pred_file = tempfile()
test_set = data_file("restaurant_reco_test.txt")
r$predict(test_set, out_file(pred_file))
pred_ratings <- scan(pred_file)
reco_rating_long_predict$predicted_ratings <- as.data.frame(pred_ratings)
reco_rating_long_predict$restaurant_type_name <-rest_type_id[reco_rating_long_predict$restaurant_type]
reco_rating_long_predict$restauracity_name <-rest_type_id[reco_rating_long_predict$cityname]
reco_rating_long_predict$city_name <- city_id_name_map[reco_rating_long_predict$cityname]
reco_rating_long_test[,c("cityname", "restaurant_type", "rating","restauracity_name")] <- NULL 
View(subset(reco_rating_long_test, city_name %in% "ny"))