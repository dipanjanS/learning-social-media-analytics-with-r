############################################################################
# Chapter      :   8
# Description  :   Topic modeling across the presidency timeline for Donald Trump
############################################################################

library(readr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tm)
library(wordcloud)
library(reshape2)
library(zoo)
library(topicmodels)

############################################################################
# Data Extraction Code
############################################################################

endpoint_url = "http://content.guardianapis.com/search?from-date=2016-04-01&to-date=2016-07-01&section=commentisfree&page=1&page-size=200&q=brexit&api-key=xxxxxxxx"

# Extracting all links for the use case

extract_all_links <- function(){
  guardian_key = "864b10a5-c6c8-4a6f-a375-0cc40b7da335"
  ## Get all the links around a query
  uriTemplate <- "http://content.guardianapis.com/search?from-date=2016-04-01&to-date=2016-07-01&section=commentisfree&page=1&page-size=200&q=brexit&api-key=%s"
  apiurl<- sprintf(uriTemplate,guardian_key)
  json_res <- getURL(apiurl, .mapUnicode=TRUE)
  num_pages = json_res %>% enter_object("response") %>% spread_values(num_pages = jnumber("pages"))
  num_pages = num_pages$num_pages
  out_df = data.frame()
  for (num in 1:num_pages){
    uriTemplate <- "http://content.guardianapis.com/search?from-date=2016-04-01&to-date=2017-07-01&section=commentisfree&page=%s&page-size=200&q=brexit&api-key=%s"
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
    write.csv2(out_df, file = "trump_articles_news_links.csv", row.names = FALSE, quote = TRUE)
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
        write.csv2(out_df, file = "trump_articles.csv", quote = TRUE, row.names = FALSE, sep = "|")
      }
      names(d)[i] <- links[i]
      Sys.sleep(2)
    }
  } 
  write.csv2(out_df, file = "trump_articles.csv", quote = TRUE, row.names = FALSE, sep = "|")
  return(out_df)
}

# Extract all links data frame
links_df <- extract_all_links()

# Seperate the links fromt the data frame

links <- links_df[,"url"]

# Get me all the data
data_df <- extract_all_data(links)

############################################################################
# Data Analysis Code: Basic descriptive Statistics 
############################################################################

setwd('C:\\Users\\tushar\\Google Drive\\Book Project\\code_ns')
trump_phase1 = read.csv2("trump_articles1.csv", stringsAsFactors = FALSE, encoding = "utf-8")
trump_phase2 = read.csv2("trump_articles2.csv", stringsAsFactors = FALSE)

trump_phase1[,'time_interval'] <- 'Phase 1'
trump_phase2[,'time_interval'] <- 'Phase 2'

trump_df <- rbind(trump_phase1,trump_phase2)

articles_df <- trump_df

for (i in 1: nrow(articles_df)){
  m <- regexpr("\\d{1,2} \\w{3,9} \\d{4}", articles_df[i, "content"], perl=TRUE)
  if(m !=-1){
    articles_df[i,"article_date"] <- regmatches(articles_df[i, "content"],m)
  }
}

articles_df[,"article_date_formatted"] <- as.Date(articles_df[,"article_date"],"%d %B %Y")
articles_df[,"date_year_month"] = as.yearmon(articles_df[,"article_date_formatted"])


articles_across_phases <- articles_df %>% 
                          group_by(time_interval) %>% 
                          summarise(average_article_count = n()/n_distinct(date_year_month))

p <- ggplot(data=articles_across_phases, mapping=aes(x=time_interval, y=average_article_count)) + geom_bar(aes(fill = time_interval), stat = "identity")
p <- p + theme_bw() + guides(fill = FALSE) +
  theme(axis.text.x = element_text(size = 15)) +
  ylab("Mean number of articles") + xlab("Phase") +
  ggtitle("Average article count across phases") + theme(strip.text = (element_text(size = 15)))
print(p)

trump_wc <- Corpus(VectorSource(articles_df[,"content"]))
temp_articles <-tm_map(trump_wc,content_transformer(tolower))
dtm <- DocumentTermMatrix(temp_articles)
word_count <- rowSums(as.matrix(dtm))
articles_df[,"word_count"] <- word_count

wordcount_across_time <- articles_df %>% 
  group_by(date_year_month) %>% 
  summarise(average_article_count = mean(word_count))


ggplot(data=wordcount_across_time, aes(x=as.Date(date_year_month), y=average_article_count)) + 
  geom_line(colour="orange", size=1.5) + 
  ylab("Average word count") + xlab("Month - Year") +
  ggtitle("Word count across the time line")+ theme_bw() +
  scale_x_date(date_labels = "%b %Y",date_breaks = "2 month") +
  geom_vline(xintercept = as.numeric(as.Date("2016-07-01")), linetype=4)+
  theme(axis.text.x=element_text(size = 15,hjust=1))

############################################################################
# Data cleaning for topic modeling 
############################################################################

articles_phase1 <- articles_df %>% filter(time_interval == "Phase 1")
articles_phase1[,"content"] <- str_replace_all(articles_phase1[,"content"], "[^[:alnum:]]", " ")
articles_phase1 <- Corpus(VectorSource(articles_phase1[,"content"]))

# Convert all text to lower case
articles_phase1 <-tm_map(articles_phase1,content_transformer(tolower))

# Remove some special characters from the text
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
articles_phase1 <- tm_map(articles_phase1, toSpace, "-")
articles_phase1 <- tm_map(articles_phase1, toSpace, "Ã")
articles_phase1 <- tm_map(articles_phase1, toSpace, "â")
articles_phase1 <- tm_map(articles_phase1, toSpace, "â")
articles_phase1 <- tm_map(articles_phase1, toSpace, "â¢")
articles_phase1 <- tm_map(articles_phase1, toSpace, "â")
articles_phase1 <- tm_map(articles_phase1, toSpace, "â")

# Remove all punctuations from the text
articles_phase1 <- tm_map(articles_phase1, removePunctuation)

# Remove all numbers from the text
articles_phase1 <- tm_map(articles_phase1, removeNumbers)

# Remove all english stopwords from the text
articles_phase1 <- tm_map(articles_phase1, removeWords, stopwords("english"))

# Remove all whitespaces from the text
articles_phase1 <- tm_map(articles_phase1, stripWhitespace)

articles_phase2 <- articles_df %>% filter(time_interval == "Phase 2")
articles_phase2[,"content"] <- str_replace_all(articles_phase2[,"content"], "[^[:alnum:]]", " ")
articles_phase2 <- Corpus(VectorSource(articles_phase2[,"content"]))

# Convert all text to lower case
articles_phase2 <-tm_map(articles_phase2,content_transformer(tolower))

# Remove some special characters from the text
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
articles_phase2 <- tm_map(articles_phase2, toSpace, "-")
articles_phase2 <- tm_map(articles_phase2, toSpace, "Ã")
articles_phase2 <- tm_map(articles_phase2, toSpace, "â")
articles_phase2 <- tm_map(articles_phase2, toSpace, "â")
articles_phase2 <- tm_map(articles_phase2, toSpace, "â¢")
articles_phase2 <- tm_map(articles_phase2, toSpace, "â")
articles_phase2 <- tm_map(articles_phase2, toSpace, "â")

# Remove all punctuations from the text
articles_phase2 <- tm_map(articles_phase2, removePunctuation)

# Remove all numbers from the text
articles_phase2 <- tm_map(articles_phase2, removeNumbers)

# Remove all english stopwords from the text
articles_phase2 <- tm_map(articles_phase2, removeWords, stopwords("english"))

# Remove all whitespaces from the text
articles_phase2 <- tm_map(articles_phase2, stripWhitespace)

############################################################################
# Data Pre-Processing for topic modeling 
############################################################################

# Stem all word in the text
articles_phase1 <- tm_map(articles_phase1,stemDocument)

# Creating a Document term Matrix
dtm_trump_phase1 <- DocumentTermMatrix(articles_phase1)

# Stem all word in the text
articles_phase2 <- tm_map(articles_phase2,stemDocument)

# Creating a Document term Matrix
dtm_trump_phase2 <- DocumentTermMatrix(articles_phase2)

############################################################################
# Topic modelling for both phases 
############################################################################
burnin <- 4000
iter <- 200
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 5

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm_trump_phase1,k, method="Gibbs", 
             control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter))
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.terms <- as.matrix(terms(ldaOut,10))
ldaOut.terms

ldaOut_phase2 <-LDA(dtm_trump_phase2,k, method="Gibbs", 
                    control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter))
ldaOut_phase2.topics <- as.matrix(topics(ldaOut_phase2))
ldaOut_phase2.terms <- as.matrix(terms(ldaOut_phase2,10))
ldaOut_phase2.terms
