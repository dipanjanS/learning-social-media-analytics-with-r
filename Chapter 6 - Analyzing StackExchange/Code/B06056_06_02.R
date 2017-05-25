############################################################################
# Chapter      :   6 
# Description  :   Understand and anylyze demographic data from  
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
library(ggmap)
library(rworldmap)


# use this to check free query hits remaining for the day geocodeQueryCheck()


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


# function to perform reverse geocoding
# based on solution submited at:
# http://stackoverflow.com/questions/21708488/get-country-and-continent-from-longitude-and-latitude-point-in-r
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)

  as.character(indices$ADMIN)  #returns country name
}


# mapping function to set country for given location
postLocation <- function(locationName){
  if(!is.na(locationName)){
    tryCatch(coords2country(geocode(locationName)),
             warning = function(w) {
               print("warning"); 
               # handle warning here
             },
             error = function(e) {
               print("error");
               # handle error here
             })
  }
}





# path to required files
path = ""

# load users.xml
UsersDF <- loadXMLToDataFrame(paste0(path,"Users.xml"))

# perform type conversion
UsersDF$Age <- as.numeric(levels(UsersDF$Age))[UsersDF$Age]
UsersDF$Reputation <- as.numeric(levels(UsersDF$Reputation))[UsersDF$Reputation]
UsersDF$Views <- as.numeric(levels(UsersDF$Views))[UsersDF$Views]
UsersDF$UpVotes <- as.numeric(levels(UsersDF$UpVotes))[UsersDF$UpVotes]
UsersDF$DownVotes <- as.numeric(levels(UsersDF$DownVotes))[UsersDF$DownVotes]


# dimensions of user dataframe
dim(UsersDF)

# Average age of user on data.stack exchange
mean(as.numeric(UsersDF[!is.na(UsersDF$Age),'Age']))

# Average reputation of user on data.stack exchange
max(as.numeric(UsersDF[!is.na(UsersDF$Reputation),'Reputation']))

# Average reputation of user on data.stack exchange
mean(as.numeric(UsersDF[!is.na(UsersDF$Views),'Views']))


# join post and user details
PostUserDF <-merge(PostsDF[,c('Id',
                            'CreationDate',
                            'PostTypeId',
                            'Score',
                            'ViewCount',
                            'OwnerUserId',
                            'ParentId',
                            'AcceptedAnswerId',
                            'prog_lang')],
                 UsersDF[,c('Id',
                            'CreationDate',
                            'Reputation',
                            'DisplayName',
                            'Location',
                            'Views',
                            'UpVotes',
                            'DownVotes',
                            'Age')],by.x='OwnerUserId',by.y='Id')



# Average age of user answering on data.stack exchange
mean(as.numeric(PostUserDF[!is.na(PostUserDF$Age) & (PostUserDF$PostTypeId==2),
                           'Age']))

# Average age of R user on data.stack exchange
mean(as.numeric(PostUserDF[!is.na(PostUserDF$Age) & (PostUserDF$prog_lang=='r'),'Age']))




filteredPostUserDf <-PostUserDF[(PostUserDF$OwnerUserId!=-1)&(PostUserDF$prog_lang!='rest_of_the_world'),]


# get country
filteredPostUserDf$Country <- sapply(as.character(filteredPostUserDf$Location),postLocation)
filteredPostUserDf$Country <- as.character(filteredPostUserDf$Country)
filteredPostUserDf$Counter <- 1

CountryLangDF <- aggregate(filteredPostUserDf$Counter,
                           by=list(filteredPostUserDf$Country,
                                   filteredPostUserDf$prog_lang),
                           sum)

colnames(CountryLangDF) <- c('country','language', 'num_posts')


# question tags by country
ggplot(data=CountryLangDF[(CountryLangDF$country!='NULL') & 
                            (CountryLangDF$country!='warning') & 
                            (CountryLangDF$num_posts>2),],
       aes(x=reorder(country,num_posts), 
           y=as.numeric(num_posts),
           fill=as.factor(language))) +
  geom_bar(stat="identity", 
           position=position_dodge()) + 
  coord_flip()+
  theme(legend.position="right",
        axis.title.x = element_blank()) +
  ylab("Post Count") +
  xlab("Country")+
  ggtitle("Posts by Country and Language")+theme_bw()



CountryLangAgeDF <- aggregate(filteredPostUserDf[!is.na(as.numeric(filteredPostUserDf$Age)),'Age'],
                           by=list(filteredPostUserDf[!is.na(filteredPostUserDf$Age),'Country'],
                                   filteredPostUserDf[!is.na(filteredPostUserDf$Age),'prog_lang']),
                           mean)

colnames(CountryLangAgeDF) <- c('country','language', 'avg_age')


# question tags by age and country
ggplot(data=CountryLangAgeDF[(CountryLangAgeDF$country!='NULL') & 
                            (CountryLangAgeDF$country!='warning') ,],
       aes(x=reorder(country,-avg_age), 
           y=as.numeric(avg_age),
           fill=as.factor(language))) +
  geom_bar(stat="identity", 
           position=position_dodge()) + 
  coord_flip()+
  theme(legend.position="right",
        axis.title.x = element_blank()) +
  ylab("Avg Age") +
  xlab("Country")+
  ggtitle("Avg Age by Country and Language")+theme_bw()



# reputation age relation

CountryRepAgeDF <- aggregate(filteredPostUserDf[!is.na(as.numeric(filteredPostUserDf$Age)),'Age'],
                              by=list(filteredPostUserDf[!is.na(filteredPostUserDf$Age),'Country'],
                                      filteredPostUserDf[!is.na(filteredPostUserDf$Age),'Reputation']),
                              mean)


colnames(CountryRepAgeDF) <- c('country','reputation', 'avg_age')


# create levels/bin reputations
CountryRepAgeDF$reputation<-cut(CountryRepAgeDF$reputation,breaks = c(0,100,200,400,600,800,1000,2000,4000))


# question tags by age and country
ggplot(data=CountryRepAgeDF[(CountryRepAgeDF$country!='NULL') & 
                               (CountryRepAgeDF$country!='warning') ,],
       aes(x=reputation, 
           y=avg_age)) +
  geom_bar(stat="identity", 
           position=position_dodge()) + 
  coord_flip()+
  theme(legend.position="right",
        axis.title.x = element_blank()) +
  ylab("Avg Age") +
  ggtitle("Avg Age Vs Reputation")+theme_bw()
