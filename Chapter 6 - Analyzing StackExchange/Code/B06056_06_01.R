############################################################################
# Chapter      :   6 
# Description  :   Understand and anylyze posts.xml from 
#                  datascience.stackexchange.com
############################################################################

library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(XML)
library(lubridate)
library(reshape2)
library(corrplot)


# utility function to load XML data
loadXMLToDataFrame<- function(xmlFilePath){
  
  doc <- xmlParse(xmlFilePath)  
  xmlList<- xmlToList(doc)
  
  total<-length(xmlList)
  
  data<-data.frame()
  
  for(i in 1: total){
    data <- rbind.fill(data,as.data.frame(as.list( xmlList[[i]])))
  }
  
  return(data)
}

# mapping function to extract programming langauge from tag list
ds_language <- function(x) {
  if(!is.na(match("r",tolower(x)))){
    "r"
  }else if(!is.na(match("python",tolower(x)))){
    "python"
  }else if(!is.na(match("java",tolower(x)))){
    "java"
  }else if(!is.na(match("c++",tolower(x)))){
    "c++"
  }else if(!is.na(match("go",tolower(x)))){
    "go"
  }else if(!is.na(match("matlab",tolower(x)))){
    "matlab"
  }else if(!is.na(match("octave",tolower(x)))){
    "octave"
  }else if(!is.na(match("c",tolower(x)))){
    "c"
  }else {
    "rest_of_the_world"
  }
}


# utility function to sort data
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}


# path to required files
path = ""

# load posts.xml
PostsDF <- loadXMLToDataFrame(paste0(path,"Posts.xml"))

# change data type
PostsDF$CreationDate <- strptime(PostsDF$CreationDate, 
                                 "%Y-%m-%dT%H:%M:%OS")

# cleanup the tag column
PostsDF$tag_list <- lapply(str_split(PostsDF$Tags,"<|>"),
                           function(x){x%>%unlist()}) %>% 
  lapply(.,function(x){x[x!=""]})

# extract language tags from tag list
PostsDF$prog_lang <- sapply(PostsDF$tag_list,ds_language)

# AnswerCount as numeric
PostsDF$AnswerCount <- as.numeric(levels(PostsDF$AnswerCount))[PostsDF$AnswerCount]



# number of Posts
dim(PostsDF)


# number of questions
sum(na.omit(PostsDF$PostTypeId) == 1)


# number of answers per question
dim(PostsDF[(PostsDF$PostTypeId==2),])[1]/
  dim(PostsDF[(PostsDF$PostTypeId==1),])[1]



# Posts by date
ggplot(data = PostsDF, aes(x = CreationDate)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of Posts") + 
  ggtitle("Posts By Time") +
  theme_bw()



# Posts by date for R
ggplot(data = PostsDF[(PostsDF$prog_lang=='r') & (PostsDF$prog_lang !="rest_of_the_world"),], aes(x = CreationDate)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of Posts") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") + 
  theme_bw()


# posts by language over time
langDF <- PostsDF[,c('CreationDate', 'prog_lang')]
langDF$date <- format(langDF$CreationDate, '%b-%Y')
langDF <- langDF[langDF$prog_lang != 'rest_of_the_world',]
aggLangDF <- aggregate(langDF$prog_lang, by=list(langDF$date, langDF$prog_lang), length)
colnames(aggLangDF) <- c('date', 'tag', 'count')
aggLangDF$date <- as.Date(paste("01",aggLangDF$date, sep = "-"),"%d-%b-%Y")

ggplot(aggLangDF, aes(x=date, y=count, group=tag)) + 
  geom_point(aes(shape=tag)) +
  geom_line(aes(color=tag)) + 
  ggtitle("Language wise Post Trends over Time") +
  theme_bw()


# Posts by language
ggplot(PostsDF[PostsDF$prog_lang !="rest_of_the_world",], aes(reorder_size(prog_lang))) +
  geom_bar(aes(fill = prog_lang)) +
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of Posts") +
  xlab("Programming Languages")+
  ggtitle("Posts By Language") + 
  theme_bw()






# Language wise avg time to get answers
mergeddf <-merge(PostsDF[,c('Id',
                            'CreationDate',
                            'PostTypeId',
                            'Score',
                            'ViewCount',
                            'OwnerUserId',
                            'ParentId',
                            'AcceptedAnswerId',
                            'prog_lang')],
                 PostsDF[,c('Id',
                            'CreationDate',
                            'PostTypeId',
                            'Score',
                            'ViewCount',
                            'OwnerUserId',
                            'ParentId',
                            'AcceptedAnswerId',
                            'prog_lang')],by.x='AcceptedAnswerId',by.y='Id')

mergeddf$time_to_answer <- difftime(mergeddf$CreationDate.y,
                                    mergeddf$CreationDate.x,
                                    units = "mins")

outlierdf <- mergeddf[mergeddf$time_to_answer>129600,]

mergeddf <- mergeddf[mergeddf$time_to_answer<=129600,]

agg_time <- aggregate(mergeddf$time_to_answer, 
                      by=list(year(mergeddf$CreationDate.x),
                              mergeddf$prog_lang.x), 
                      mean)
colnames(agg_time) <- c('year','language', 'avg_time_to_answer')

# average time to get answers by language per year
ggplot(data=agg_time[agg_time$language!='rest_of_the_world',], 
       aes(x=language, y=as.numeric(avg_time_to_answer)/60,
           fill=as.factor(year))) +
  geom_bar(stat="identity", 
           position=position_dodge())+
  theme(legend.position="right",
        axis.title.x = element_blank()) +
  ylab("Avg Hours to Accepted Answer") +
  labs('custom text')+
  ggtitle("Avg Time to get answers by Language")+ 
  theme_bw()



# min max and mean for every language
temp <- aggregate(mergeddf$time_to_answer, 
                  by=list(year(mergeddf$CreationDate.x),
                          mergeddf$prog_lang.x), 
                  function(x) c( mean(x, trim = 0, na.rm = T, weights=NULL),
                                 min(x, na.rm=TRUE),
                                 max(x,na.rm=TRUE)))

temp<-cbind(temp,as.data.frame(temp$x))
temp<- temp[,c('Group.1','Group.2','V1','V2','V3')]
colnames(temp) <- c('year', 'language', 'mean','min','max')
temp.long<-melt(temp,id.vars=c("year","language"))

ggplot(temp.long[temp.long$language!='rest_of_the_world',],aes(language,value,fill=as.factor(variable)))+
  geom_bar(position="dodge",stat="identity")


# correlation
col_list <- c('Id',
              'PostTypeId',
              'CreationDate',
              'Score',
              'ViewCount',
              'AnswerCount',
              'CommentCount',
              "FavoriteCount",
              'Title','Body',
              'OwnerUserId',
              'ParentId',
              'AcceptedAnswerId',
              'prog_lang')

mergedLangDf <-merge(PostsDF[,col_list],PostsDF[,col_list],by.x='AcceptedAnswerId',by.y='Id')

mergedLangDf$time_to_answer <- difftime(mergedLangDf$CreationDate.y,mergedLangDf$CreationDate.x)

corr_lang_list<-c("Score.x",
                  "ViewCount.x",
                  "AnswerCount.x",
                  "CommentCount.x",
                  "FavoriteCount.x",
                  "qlength",
                  "anslength",
                  "time_to_answer")

mergedLangDf$qlength<-str_length(mergedLangDf$Title.x)
mergedLangDf$anslength<-str_length(mergedLangDf$Body.x)

mergedLangDf$Score.x <- as.numeric(mergedLangDf$Score.x)
mergedLangDf$ViewCount.x <- as.numeric(mergedLangDf$ViewCount.x)
mergedLangDf$AnswerCount.x <- as.numeric(mergedLangDf$AnswerCount.x)
mergedLangDf$CommentCount.x <- as.numeric(mergedLangDf$CommentCount.x)
mergedLangDf$FavoriteCount.x <- as.numeric(mergedLangDf$FavoriteCount.x)
mergedLangDf$time_to_answer <- as.numeric(mergedLangDf$time_to_answer)

mergedLangDf[is.na(mergedLangDf$FavoriteCount.x),'FavoriteCount.x']<-0

M <- cor(mergedLangDf[mergedLangDf$prog_lang.x!='rest_of_the_world',
                      corr_lang_list], 
         method = "pearson")

corrplot(M, method="number",type = "lower")