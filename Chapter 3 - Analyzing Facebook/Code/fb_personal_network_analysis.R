library(Rfacebook)
library(gridExtra)
library(dplyr)

token = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
# get my personal information
me <- getUsers("me", token=token, 
               private_info = TRUE)

View(me[c('name', 'id', 'gender', 'birthday')])


anonymous_names <- c('Johnny Juel', 'Houston Tancredi', 'Eddie Artist', 'Paula Bauder',
           'Noble Towe', 'Ira Denman', 'Patricia Shook', 'Phillip Saunder',
           'Eldon Solano', 'Julian Aye', 'Todd Mangini', 'Perry Rocamora',
           'Lonny Waynick', 'Cornell Messier', 'Neal Gallagher', 'Rufus Dutta',
           'Leon Hottinger', 'Stacy Pillsbury', 'Burl Wieczorek', 'Zackary Millan',
           'Murray Bulmer', 'Brendan Noga', 'Hipolito Naranjo', 'Jacob Fidler',
           'Cortez Neville', 'Jamey Ferebee', 'Carlo Mcgown', 'Agustin Lacour',
           'Paris Mcmillon', 'Cliff Orduno', 'Claudio Degreenia', 'Rigoberto Ros',
           'Stuart Tibbs', 'Israel Hartson', 'Rosario Keala', 'Felix Francis',
           'Jae Matherly', 'Julius Henrichs', 'Yong Sprayberry', 'Johann Paul')


# getting friends information
friends <- getFriends(token, simplify=TRUE)
friends$name <- anonymous_names
View(head(friends))

# get personal information
friends_info <- getUsers(friends$id, token, private_info = TRUE)

View(table(friends_info$gender))

View(table(friends_info$location))

View(table(friends_info$relationship_status))

View(table(friends_info$relationship_status, friends_info$gender))


# extract top liked pages in your network
get_friends_likes <- function(id, token){
  df <- try(getLikes(user=id, token=token, n=1000))
  if(inherits(df, "try-error"))
  {
    #inset error handling code if needed
    # I am skipping this because I just need valid page likes
  }else{
    return(df)
  }
}

# get node ids of friends
ids <- friends$id
# get likes of all friends
likes_df <- data.frame()
for (id in ids){
  likes_df <- rbind(likes_df, get_friends_likes(id, token))
}


friend_likes <- tbl_df(likes_df) %>% count(names, sort=TRUE)
friend_likes <- as.data.frame(friend_likes)
colnames(friend_likes) <- c('names', 'freq')

# total liked pages by friends
nrow(friend_likes)

# top mutual liked pages among friends
View(head(friend_likes[order(friend_likes$freq, decreasing=TRUE),],10))

# get my likes
my_likes <- get_friends_likes("me", token)
# view total liked pages
nrow(my_likes)

# get liked pages in common with my friends
my_liked_pages <- my_likes$names
common_likes <- friend_likes[friend_likes$names %in% my_liked_pages,]

# get total mutual liked pages between me and my friends
nrow(common_likes)

View(head(common_likes[order(common_likes$freq, decreasing=TRUE),],10))






library(igraph)

# get friend network
friend_network <- getNetwork(token, format="adj.matrix")

# just to verify if everything is available
nrow(friend_network)

# anonymize friend names
anonymize_names <- function(friend_network, names=anonymous_names){
  rownames(friend_network) <- names
  colnames(friend_network) <- names
  return(friend_network)
}
friend_network <- anonymize_names(friend_network=friend_network)

# get singleton friends (only friends with me)
singletons <- rowSums(friend_network)==0 

table(singletons)

# remove singleton friends from graph
friend_network_graph <- graph.adjacency(friend_network[!singletons,!singletons])

# view graph details
str(friend_network_graph)

names <- strsplit(V(friend_network_graph)$name, " ")
f_names <- sapply(names, head, 1)




tkplot(friend_network_graph, 
       vertex.size = 15,
       vertex.color="lightblue",
       vertex.frame.color= "white",
       vertex.label.color = "black",
       vertex.label.family = "sans",
       edge.width=1,  
       edge.arrow.size=0, 
       edge.color="black",
       edge.curved=TRUE,
       layout=layout.fruchterman.reingold)

# get total degree of nodes
degree_fng <- degree(friend_network_graph, mode="total")
degree_fng_df <- data.frame(Name=V(friend_network_graph)$name, 
                            "Degree"=as.vector(degree_fng))
degree_fng_df <- degree_fng_df[order(degree_fng_df$Degree, 
                                     decreasing=TRUE),]

# get total indegree of nodes
indegree_fng <- degree(friend_network_graph, mode="in")
indegree_fng_df <- data.frame(Name=V(friend_network_graph)$name, 
                              "Indegree"=as.vector(indegree_fng))
indegree_fng_df <- indegree_fng_df[order(indegree_fng_df$Indegree, 
                                         decreasing=TRUE),]

# get total outdegree of nodes
outdegree_fng <- degree(friend_network_graph, mode="out")
outdegree_fng_df <- data.frame(Name=V(friend_network_graph)$name, 
                               "Outdegree"=as.vector(outdegree_fng))
outdegree_fng_df <- outdegree_fng_df[order(outdegree_fng_df$Outdegree, 
                                           decreasing=TRUE),]



table(degree_fng)
degree_distribution(friend_network_graph)

# degree stats
if (dev.cur()!=1){dev.off()}
grid.table(head(degree_fng_df, 10),
           rows=NULL)
# indegree stats
if (dev.cur()!=1){dev.off()}
grid.table(head(indegree_fng_df, 10),
           rows=NULL)
# outdegree stats
if (dev.cur()!=1){dev.off()}
grid.table(head(outdegree_fng_df, 10),
           rows=NULL)

# closeness stats
closeness_fng <- closeness(friend_network_graph, mode="all", normalized=TRUE)
closeness_fng_df <- data.frame(Name=V(friend_network_graph)$name, 
                              "Closeness"=as.vector(closeness_fng))
closeness_fng_df <- closeness_fng_df[order(closeness_fng_df$Closeness, decreasing=TRUE),]
if (dev.cur()!=1){dev.off()}
grid.table(head(closeness_fng_df, 10),
           rows=NULL)


# betweenness
betweenness_fng <- betweenness(friend_network_graph)
betweenness_fng_df <- data.frame(Name=V(friend_network_graph)$name, 
                               "Betweenness"=as.vector(betweenness_fng))
betweenness_fng_df <- betweenness_fng_df[order(betweenness_fng_df$Betweenness, 
                                           decreasing=TRUE),]
if (dev.cur()!=1){dev.off()}
grid.table(head(betweenness_fng_df, 10),
           rows=NULL)


fng_df <- data.frame(degree_fng, closeness_fng, betweenness_fng)
head(fng_df)


# cliques
clique_num(friend_network_graph)
cliques(friend_network_graph, min=4, max=4)


# get communities / clusters in your network 
# build clusters from the network graph
friend_network_graph <- graph.adjacency(friend_network[!singletons,!singletons],
                                        mode='undirected')
layout <- layout_with_fr(friend_network_graph, 
                         niter=500, start.temp=5.744)
fc <- cluster_fast_greedy(friend_network_graph) 
# get community details
communities <- data.frame(layout)
names(communities) <- c("x", "y")
communities$cluster <- factor(fc$membership)
communities$name <- fc$names

# get number of nodes per cluster
table(communities$cluster)

# view friends per community
groups(fc)

# get modularity score
modularity(fc)


comm_plot <- ggplot(communities, aes(x=x, y=y, color=cluster, label=name))
comm_plot <- comm_plot + geom_label(aes(fill = cluster), colour = "white")
comm_plot

plot(fc, friend_network_graph, 
     vertex.size=15, 
     vertex.label.cex=0.8, 
     vertex.label=fc$names, 
     edge.arrow.size=0, 
     edge.curved=TRUE,
     vertex.label.color="black",
     layout=layout.fruchterman.reingold)


# answer to correlation question for centrality measures
rsq <- format(cor(degree_fng, closeness_fng) ^2, digits=3)
corr_plot <- ggplot(fng_df, aes(x=degree_fng, y=closeness_fng))+ theme_bw() +
  geom_jitter(alpha=1/2) +
  scale_y_log10() +
  labs(x="Degree", y="Closeness") +
  annotate("text", label=paste("R-sq =", rsq), x=+Inf, y=1, hjust=1)
corr_plot


rsq <- format(cor(degree_fng, betweenness_fng) ^2, digits=3)
corr_plot <- ggplot(fng_df, aes(x=degree_fng, y=betweenness_fng))+ theme_bw() +
  geom_jitter(alpha=1/2) +
  scale_y_log10() +
  labs(x="Degree", y="Betweenness") +
  annotate("text", label=paste("R-sq =", rsq), x=+Inf, y=1, hjust=1)
corr_plot