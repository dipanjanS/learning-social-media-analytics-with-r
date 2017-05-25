############################################################################
# Chapter      :   4
# Objective    :   Tips data extraction and preceeding analysis
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
library(syuzhet)

############################################################################
#       Utility Functions
############################################################################
extract_all_tips_by_venue <- function(venue_id){
  tip_uri_template = "https://api.foursquare.com/v2/venues/%s/tips?v=20131016&limit=200&offset=%s&client_id=%s&client_secret=%s"
  token="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  secret="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  tips_processed = 0
  apiurl<- sprintf(tip_uri_template, venue_id,tips_processed,token,secret)
  json_res <- getURL(apiurl, .mapUnicode=TRUE)
  json_response <- as.data.frame(json_res %>% enter_object("meta") %>% spread_values(result = jnumber("code")))["result"]
  if (json_response == 200){
    number_of_tips = as.data.frame(json_res %>% enter_object("response") %>% enter_object("tips") %>% spread_values(result = jnumber("count")))["result"]
    out_df = data.frame()
    out_df <- rbind(out_df, extract_tips_from_json(json_res))
    tips_processed = 200
    while(tips_processed < number_of_tips) {
      apiurl<- sprintf(tip_uri_template, venue_id,tips_processed,token,secret)
      json_res <- getURL(apiurl, .mapUnicode=TRUE)
      json_response <- as.data.frame(json_res %>% enter_object("meta") %>% spread_values(result = jnumber("code")))["result"]
      if (json_response == 200){
        out_df <- rbind(out_df, extract_tips_from_json(json_res))
        tips_processed = tips_processed + 200
      } else {
        print("Unsuccessful API call")
        #return(FALSE)
      }
    }
  } else{
    print("Unsuccessful API call")
    return(FALSE)
  }
  return(out_df)
}
extract_tips_from_json <- function(json_response){
  tips_df <- as.data.frame(json_response %>% 
                             enter_object("response") %>% 
                             enter_object("tips") %>% 
                             enter_object("items") %>% 
                             gather_array() %>% 
                             spread_values(tip_id = jstring("id")) %>% 
                             spread_values(tip_text = jstring("text")) %>% 
                             enter_object("user") %>% 
                             spread_values(tip_user_id = jstring("id")) %>% 
                             spread_values(tip_user_gender = jstring("gender")))
  tips_df$array.index <- NULL
  tips_df$document.id <- NULL
  return(tips_df)
}

##################################DATA EXTRACTION CODE##########################################

# Extract data for Vasa Museum
vasa_museum_id = "4adcdaeff964a520135b21e3"
tips_df <- extract_all_tips_by_venue(vasa_museum_id)
write.csv(tips_df, file = "vasa_museum_tips.csv", row.names = FALSE)

# Combine all the Tips data in a single data frame
tips_files <-list.files(pattern="*tips.csv")
complete_df <- data.frame()
for(file in tips_files)
{
    perpos <- which(strsplit(file, "")[[1]]==".")
    museum_name <- gsub(" ","",substr(file, 1, perpos-1))
    df <- read.csv(file, stringsAsFactors = FALSE)
    df$museum <- museum_name
    complete_df <- rbind(complete_df, df)
}

# Basic Descriptive statistics

# Count statistics for Tips
review_summary <- complete_df %>% group_by(museum) %>% summarise(num_reviews = n())
ggplot(data = review_summary, aes(x = reorder(museum, num_reviews), y = num_reviews)) +
    geom_bar(aes(fill = museum), stat = "identity") +
    theme(legend.position = "none") +
    xlab("Museum Name") + ylab("Total Reviews Count") + ggtitle("Reviews for each Museum") +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + coord_flip()

# Gender based count statistics
review_summary_gender <- complete_df %>% group_by(museum,tip_user_gender) %>% 
    summarise(num_reviews = n()) %>% 
    mutate(review_percent = num_reviews/sum(num_reviews))
ggplot(data = review_summary_gender, aes(x = reorder(museum, num_reviews), y = num_reviews)) +
    geom_bar(aes(fill = museum), stat = "identity") +
    theme(legend.position = "none") +
    xlab("Museum Name") + ylab("Total Reviews Count") + ggtitle("Reviews for each Museum") +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + facet_grid(~tip_user_gender) + coord_flip()


# Sentiments across different museums

sentiments_df <- get_nrc_sentiment(complete_df[,"tip_text"])
sentiments_df <- cbind(sentiments_df, complete_df[,c("tip_user_gender","museum")])
sentiments_summary_df <-sentiments_df %>% select(-c(positive,negative)) %>%
    group_by(museum) %>% summarise(anger = sum(anger),anticipation = sum(anticipation),disgust = sum(disgust), fear= sum(fear) , joy = sum(joy) , sadness = sum(sadness), surprise = sum(surprise), trust =sum(trust))
    
sentiments_summary_df_reshaped <- reshape(sentiments_summary_df,varying = c(colnames(sentiments_summary_df)[!colnames(sentiments_summary_df) %in% c("museum")]), v.names = "count", direction = "long",new.row.names = 1:1000)
sentiment_names <-c(colnames(sentiments_summary_df)[!colnames(sentiments_summary_df) %in% c("museum")])
sentiments_summary_df_reshaped$sentiment <- sentiment_names[sentiments_summary_df_reshaped$time]
sentiments_summary_df_reshaped[,c("time", "id")] <- NULL

p5 <- ggplot(sentiments_summary_df_reshaped, aes(x=sentiment, y=count))
(p5 <- p5 + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
        facet_wrap(~museum, ncol = 1)) +
        ylab("Percent sentiment score") + 
        ggtitle("Sentiment variation across museums")

# Sentiment analysis based ranking
# Ranking based on cumulative review sentiments
for (i in 1:nrow(complete_df)){
    review <- complete_df[i,"tip_text"]
    poa_word_v <- get_tokens(review, pattern = "\\W")
    syuzhet_vector <- get_sentiment(poa_word_v, method="bing")
    complete_df[i,"sentiment_total"] <- sum(syuzhet_vector)
}
rank_sentiment_score <- complete_df %>% group_by(museum)%>% summarise(total_sentiment = sum(sentiment_total), total_reviews = n()) %>% mutate(mean_sentiment = total_sentiment/total_reviews) %>% arrange(mean_sentiment)
rank_sentiment_score$museum.rank <- rank(-rank_sentiment_score$mean_sentiment)

rank_sentiment_score_gender <- complete_df %>% group_by(museum, tip_user_gender)%>% summarise(total_sentiment = sum(sentiment_total), total_reviews = n()) %>% mutate(mean_sentiment = total_sentiment/total_reviews) %>% arrange(mean_sentiment)

rank_sentiment_score_gender_female <- subset(rank_sentiment_score_gender, tip_user_gender == "female")
rank_sentiment_score_gender_female$museum.rank <- rank(-rank_sentiment_score_gender_female$mean_sentiment)

rank_sentiment_score_gender_male <- subset(rank_sentiment_score_gender, tip_user_gender == "male")
rank_sentiment_score_gender_male$museum.rank <- rank(-rank_sentiment_score_gender_male$mean_sentiment)

rank_sentiment_score_gender_none <- subset(rank_sentiment_score_gender, tip_user_gender == "none")
rank_sentiment_score_gender_none$museum.rank <- rank(-rank_sentiment_score_gender_none$mean_sentiment)

# All the ranks in one data frame
combined_rank <- museum_tripadvisor %>% inner_join(rank_sentiment_score[,c("museum", "museum.rank")])
colnames(combined_rank)[ncol(combined_rank)] <- "overall_sentiment_rank"
combined_rank<- combined_rank %>% inner_join(rank_sentiment_score_gender_female[,c("museum", "museum.rank")], by = "museum")
colnames(combined_rank)[ncol(combined_rank)] <- "female_sentiment_rank"
combined_rank<- combined_rank %>% inner_join(rank_sentiment_score_gender_male[,c("museum", "museum.rank")], by = "museum")
colnames(combined_rank)[ncol(combined_rank)] <- "male_sentiment_rank"
combined_rank<- combined_rank %>% inner_join(rank_sentiment_score_gender_none[,c("museum", "museum.rank")], by = "museum")
colnames(combined_rank)[ncol(combined_rank)] <- "none_sentiment_rank"
                             