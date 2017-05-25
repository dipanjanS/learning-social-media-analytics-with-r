############################################################################
# Chapter      :   2 
# Description  :   Connect to Twitter app and build a follower graph
# Note         :   Your results may be different than the ones discussed 
#                  in the chapter due to dyanmic nature of Twitter
############################################################################

library(twitteR)
library(data.table)
library(igraph)
library(RColorBrewer)

CONSUMER_SECRET = "XXXXXXXXXXXXX"
CONSUMER_KEY = "XXXXXXXXXXXXXXXXXXXXXXXXXX"


############################################################################
#       Utility Functions
############################################################################

# get follower names
get_follower_list <- function(userName){
  # get user data
  twitterUser <- getUser(userName)
  twitterUserFollowerIDs<-twitterUser$getFollowers(retryOnRateLimit=1)
  
  # extract the list of followers
  return (sapply(twitterUserFollowerIDs,screenName))
}

# append rows to dataframe
append_to_df<-function(dt, elems)
{ 
  return(rbindlist(list(dt,  elems),use.names = TRUE))
}

############################################################################
#             Follower Graph Analysis
############################################################################
setup_twitter_oauth(consumer_key = CONSUMER_KEY,consumer_secret = CONSUMER_SECRET)

# Begin with a certain username
coreUserName <- "jack"
twitterUser <- getUser(coreUserName)


# Extract Followers for the core user
twitterUser_follower_IDs <- twitterUser$getFollowers(retryOnRateLimit=10)
twitterUser_followers_df = rbindlist(lapply(
                              twitterUser_follower_IDs,as.data.frame
                              ))

# filter dummy accounts 
filtered_df <- subset(twitterUser_followers_df, 
                    followersCount < 100 & 
                      statusesCount > 100 & 
                      statusesCount < 5000 & 
                      protected==FALSE) 

filtered_follower_IDs <- filtered_df$screenName

# prepare edge data frame
edge_df<-data.frame(from=filtered_follower_IDs,
                    to=rep('rghv_bali', 
                           length(filtered_follower_IDs)), 
                    stringsAsFactors=FALSE)


# Iterate and extract list of followers of followers
counter = 1
for(follower in filtered_follower_IDs){
  # fetch follower list for current user
  followerScreenNameList <- get_follower_list(follower)
  
  print(paste("Processing completed for:",
            follower,
            "(",counter,"/",
            length(filtered_follower_IDs),")"
            ))
  # append to edge list
  edge_df <- append_to_df(edge_df,list(from=followerScreenNameList,
                                       to=rep(follower, 
                                              length(followerScreenNameList))))
  counter <- counter + 1
}


# prepare network object
net <- graph.data.frame(edge_df, directed=T)

# simplify network
net <- simplify(net, remove.multiple = F, remove.loops = T)

# adjust the size of nodes based on in and out degrees
deg <- degree(net, mode="all")
V(net)$size <- deg*0.05 + 1
V(net)[name == "core_user"]$size <- 15

# node coloring
pal3 <- brewer.pal(10, "Set3")

# overall follower graph
plot(net, edge.arrow.size=0.1,
     vertex.label = ifelse(V(net)$size >= 15, V(net)$name, NA),
     vertex.color = pal3)


############################################################################
#             Friends Among Follwors
############################################################################


# Plot to highlight Followers with large number of followers
deg <- degree(net, mode="out")
V(net)$size <- deg*0.05+2

# Highlight the coreUser
V(net)['core_user']$size <- 15

# identify friend vertices
friendVertices <- ends(net, es=E(net)[from('core_user')])[,2]


# Generate edge color variable:
ecol <- rep("grey80", ecount(net))
ecol[which (V(net)$name %in% friendVertices)] <- 'red'

# Generate edge width variable:
ew <- rep(2, ecount(net))
ew[which (V(net)$name %in% friendVertices)] <- 4

# add core_user for vertex coloring
friendVertices <- append(friendVertices,"core_user")

# Generate node color variable:
vcol <- rep("gray80", vcount(net))
vcol[which (V(net)$name %in% friendVertices)] <- "gold"

# vertex label size
V(net)$label.cex <- 1.2

plot(net, 
     vertex.color=vcol, 
     edge.color=ecol, 
     edge.width=ew, 
     edge.arrow.mode=0, 
     vertex.label = ifelse(V(net)$name %in% friendVertices, V(net)$name, NA), 
     vertex.label.color="black",
     vertex.label.font=2,
     edge.curved=0.1
     )





