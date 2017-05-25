############################################################################
# Chapter      :   8
# Description  :   Sentiment analysis across the years for Narendra Modi
############################################################################

library(readr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tm)
library(wordcloud)
library(syuzhet)
library(reshape2)

############################################################################
# Data Extraction Code
############################################################################

# Extracting all links for the use case

extract_all_links <- function(){
  guardian_key = "864b10a5-c6c8-4a6f-a375-0cc40b7da335"
  ## Get all the links around a query
  uriTemplate <- "http://content.guardianapis.com/search?from-date=2002-01-01&to-date=2017-04-14&page-size=200&order-by=oldest&q=%%22narendra%%20modi%%22&api-key=%s"
  apiurl<- sprintf(uriTemplate,guardian_key)
  json_res <- getURL(apiurl, .mapUnicode=TRUE)
  num_pages = json_res %>% enter_object("response") %>% spread_values(num_pages = jnumber("pages"))
  num_pages = num_pages$num_pages
  out_df = data.frame()
  for (num in 1:num_pages){
    uriTemplate <- "http://content.guardianapis.com/search?from-date=2002-01-01&to-date=2017-04-14&page-size=200&order-by=oldest&page=%s&q=%%22narendra%%20modi%%22&api-key=%s"
    apiurl<- sprintf(uriTemplate,num, guardian_key)
    json_res <- getURL(apiurl, .mapUnicode=TRUE)
    urls <- as.data.frame(json_res %>% enter_object("response") %>% enter_object("results")  %>%
                            gather_array() %>% 
                            spread_values(url = jstring("webUrl"), type = jstring("type"), 
                                          sectionName = jstring("sectionName"), 
                                          webTitle = jstring("webTitle"),
                                          sectionId = jstring("sectionId")
                            ))
    urls$document.id <- NULL
    urls$array.index <- NULL
    out_df = rbind(out_df, urls)
    write.csv2(out_df, file = "modi_articles_news_links.csv", row.names = FALSE, quote = TRUE)
    print(num)
    Sys.sleep(10)
  }
  return(out_df)
}

# Function to scrape data from a url link

extract_url_data <- function(url, sleep_time = 5){
  b <- read_html(curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  nodes<-trimws(html_text(html_nodes(b, xpath = ".//p")))
  nodes<-gsub("\n", "", nodes)
  nodes<-gsub("  ", "", nodes)
  content = paste(nodes, collapse = " ")
  Sys.sleep(sleep_time)
  return(content)
}

# Function to extract data from list of links

extract_all_data <- function(links){
  d <- vector("list", length(links))
  for (i in seq_along(links)) {
    if (!(links[i] %in% names(d))) {
      cat(paste("Doing", links[i], "..."))
      ok <- FALSE
      counter <- 0
      while (ok == FALSE & counter <= 5) {
        counter <- counter + 1
        out <- tryCatch({                  
          extract_url_data(links[i])
        },
        error = function(e) {
          Sys.sleep(10)
          e
        }
        )
        if ("error" %in% class(out)) {
          cat(".")
        } else {
          ok <- TRUE
          cat(" Done.")
        }
      }
      cat("\n")
      d[[i]] <- out
      out_df[i,"content"] <- out
      if(i %% 50 == 0){
        write.csv2(out_df, file = "modi_articles.csv", quote = TRUE, row.names = FALSE, sep = "|")
      }
      names(d)[i] <- links[i]
      Sys.sleep(2)
    }
  } 
  write.csv2(out_df, file = "modi_articles.csv", quote = TRUE, row.names = FALSE, sep = "|")
  return(out_df)
}

# Extract all links data frame
links_df <- extract_all_links()

# Seperate the links fromt the data frame

links <- links_df[,"url"]

# Get me all the data
data_df <- extract_all_data(links)

############################################################################
# Sentiment Analysis Code
############################################################################

articles_df <- data_df

for (i in 1: nrow(articles_df)){
    m <- regexpr("\\d{1,2} \\w{3,9} \\d{4}", articles_df[i, "content"], perl=TRUE)
    if(m !=-1){
        articles_df[i,"article_date"] <- regmatches(articles_df[i, "content"],m)
    }
}
# Sections in which Modi has appeared in 

section_summary <- articles_df %>% 
                   group_by(sectionName) %>% 
                   summarise(num_articles = n())

ggplot(data = section_summary, aes(x = sectionName, y = num_articles)) +
    geom_bar(aes(fill = sectionName), stat = "identity") + guides(fill=FALSE) +
    xlab("Section") + ylab("Total Articles Count") + ggtitle("Articles distribution by Section") + theme_bw()+
    theme(axis.text.x = element_text(size = 15,angle = 90, hjust = 1,vjust=0.5))

# Filter top sections

section_names <- as.data.frame(articles_df %>% group_by(sectionName) %>% summarise(num_articles = n()) %>% 
                                 filter(num_articles>20) %>% select(sectionName))
selected_section_articles <- articles_df[articles_df$sectionName %in% section_names[,"sectionName"],]
articles_df <- selected_section_articles


articles_df[,"article_date_formatted"] <- as.Date(articles_df[,"article_date"],"%d %B %Y")
articles_df[,"article_year"]<- format(as.Date(articles_df[,"article_date_formatted"]),"%Y")

# Number of articles over the years

year_article_summary <- articles_df %>% group_by(article_year) %>% summarise(num_articles = n())
ggplot(data = year_article_summary, aes(x = article_year, y = num_articles)) + guides(fill=FALSE) +
    geom_bar(aes(fill = article_year), stat = "identity") +
    theme(legend.position = "none") +
    xlab("Year") + ylab("Total Articles Count") + ggtitle("Articles distribution by Year") +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + theme_bw()

sentiments_df <- get_nrc_sentiment(articles_df[,"content"])
articles_sentiment_df <- cbind(articles_df,sentiments_df)

# Overall sentiments across all articles

sentiments_summary_df <-articles_sentiment_df %>% 
                        select(-c(positive,negative)) %>% 
                        summarise(anger = sum(anger),anticipation = sum(anticipation),disgust = sum(disgust), fear= sum(fear) , joy = sum(joy) , sadness = sum(sadness), surprise = sum(surprise), trust =sum(trust))

sentiments_summary_df <- as.data.frame(t(sentiments_summary_df))
sentiments_summary_df <- cbind(row.names(sentiments_summary_df), sentiments_summary_df)

colnames(sentiments_summary_df)<- c("sentiment", "count")

ggplot(data = sentiments_summary_df, aes(x = reorder(sentiment, count), y = count)) +
    geom_bar(aes(fill = sentiment), stat = "identity") +
    theme(legend.position = "none") + guides(fill=FALSE)+ coord_flip() +
    xlab("Sentiment") + ylab("Total Sentiment Count") + 
    ggtitle("Overall sentiment across all articles") +
    theme(axis.text=element_text(size = 100,angle=90,hjust=1,vjust=0.5)) + theme_bw()

# Yearly sentiment variation
sentiments_year_summary_df <- articles_sentiment_df %>% 
                              select(-c(positive,negative)) %>%
                              group_by(article_year) %>% 
                              summarise(anger = mean(anger),anticipation = mean(anticipation),disgust = mean(disgust), fear= mean(fear) , joy = mean(joy) , sadness = mean(sadness), surprise = mean(surprise), trust =mean(trust)) %>%
                              melt

names(sentiments_year_summary_df) <- c("Year", "sentiment", "Sum_value")

ggplot(data = sentiments_year_summary_df, aes(x = Year, y = Sum_value, group = sentiment)) +
    geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
    geom_point(size = 0.5) +theme(legend.text = element_text(size = 20))+
    ylim(0, NA) +
    ylab("Mean sentiment score") + 
    ggtitle("Sentiment across years")+ theme_bw()

# Section wise sentiment variation

sentiments_year_summary_df <- articles_sentiment_df %>% 
                              select(-c(positive,negative)) %>%
                              group_by(sectionName) %>% 
                              summarise(anger = mean(anger),anticipation = mean(anticipation),disgust = mean(disgust), fear= mean(fear) , joy = mean(joy) , sadness = mean(sadness), surprise = mean(surprise), trust =mean(trust))%>%
                              melt

p <- ggplot(data=sentiments_year_summary_df, mapping=aes(x=variable, y=value)) + geom_bar(aes(fill = variable), stat = "identity")
p <- p + facet_grid(facets = sectionName ~ ., margins = FALSE) + theme_bw() + guides(fill = FALSE) +
  theme(axis.text.x = element_text(size = 15)) +
  ylab("Mean sentiment score") + xlab("Emotion") +
  ggtitle("Emotion across Section") + theme(strip.text = (element_text(size = 15)))
print(p)

p <- ggplot(data=sentiments_year_summary_df, mapping=aes(x=variable, y=value)) + geom_bar(aes(fill = variable), stat = "identity")
p <- p + facet_wrap(facets = ~sectionName, ncol = 2) + theme_bw() + guides(fill = FALSE) +
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1)) +
  ylab("Mean sentiment score") + xlab("Emotion") +
  ggtitle("Emotion across Section") + theme(strip.text = (element_text(size = 15)))
print(p)

ggplot(data=sentiments_year_summary_df, mapping=aes(x=variable, y=value)) + geom_bar(aes(fill = variable), stat = "identity")
# Numerical sentiment trends

sentiments_df <- get_nrc_sentiment(articles_df[,"content"])

articles_sentiment_df <- cbind(articles_df,sentiments_df)

articles_sentiment_df$sentiment <- articles_sentiment_df$positive - articles_sentiment_df$negative

sentiments_year_summary_df <- articles_sentiment_df %>% 
                              select(-c(positive,negative)) %>%
                              group_by(article_year) %>% 
                              summarise(mean_sentiment = mean(sentiment)) 

ggplot(data=sentiments_year_summary_df, aes(x=article_year, y=mean_sentiment, group=1)) + 
    geom_line(colour="red", linetype="dashed", size=1.5) + 
    geom_point(colour="red", size=2, shape=21, fill="white")+
    ylab("Sentiment score") + 
    ggtitle("Numeric Sentiment across years")+ theme_bw()

# Numerical year sentiment trends

yearmon_num_sentiment <- articles_sentiment_df %>% filter(article_year > 2013 & article_year <2017)
yearmon_summary_df <-yearmon_num_sentiment %>% select(-c(positive,negative)) %>%
    group_by(date_year_month) %>% summarise(mean_sentiment = mean(sentiment))

ggplot(data=yearmon_summary_df, aes(x=date_year_month, y=mean_sentiment, group=1)) + 
    geom_line(colour="red", linetype="dashed", size=1.5) + 
    geom_point(colour="red", size=4, shape=21, fill="white")+
    ylab("Sentiment score") + 
    ggtitle("Numeric Sentiment across years")+ theme_bw()

# Numerical year month sentiment trends

articles_sentiment_df[,"date_year_month"] = as.yearmon(articles_sentiment_df[,"article_date_formatted"])
yearmon_num_sentiment <- articles_sentiment_df
yearmon_summary_df <- yearmon_num_sentiment %>% 
  select(-c(positive,negative)) %>%
  group_by(date_year_month) %>% 
  summarise(mean_sentiment = mean(sentiment))

ggplot(data=yearmon_summary_df, aes(x=as.Date(date_year_month), y=mean_sentiment, group=1)) + 
  geom_line(colour="black", size=1.5) + 
  geom_point(colour="red", size=1.5, shape=21, fill="white")+
  ylab("Sentiment score") + xlab("Year-Month") +
  scale_x_date(labels = function(x) format(x, "%Y-%b"), date_breaks = "1 year") +
  theme(axis.text.x=element_text(size = 15,angle=-90,hjust=1))+
  ggtitle("Numeric Sentiment across years broken by months")+ theme_bw()
