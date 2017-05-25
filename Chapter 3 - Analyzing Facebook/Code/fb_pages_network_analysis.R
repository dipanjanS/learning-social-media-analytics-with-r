library(Rfacebook)
library(gridExtra)
library(igraph)
library(ggplot2)

# read in the graph
pl_graph <- read.graph(file="pl.gml", format="gml")

# inspect the page graph object
summary(pl_graph)

pl_df <- data.frame(id=V(pl_graph)$id,
                    name=V(pl_graph)$label,
                    category=V(pl_graph)$category,
                    fans=as.numeric(V(pl_graph)$fan_count),
                    talking_about=as.numeric(V(pl_graph)$talking_about_count),
                    post_activity=as.numeric(V(pl_graph)$post_activity),
                    stringsAsFactors=FALSE)

View(pl_df)

# basic stats about page vertices

# aggregate pages based on their category
if (dev.cur()!=1){dev.off()}
grid.table(as.data.frame(sort(table(pl_df$category),decreasing=TRUE)[1:10]),
           rows=NULL,
           cols=c('Category', 'Count'))

# top pages based on their fan count (likes)
if (dev.cur()!=1){dev.off()}
grid.table(pl_df[order(pl_df$fans, decreasing=TRUE), 
                 c('name', 'category', 'fans')][1:10,],
           rows=NULL)

# top pages based on total people talking about them
if (dev.cur()!=1){dev.off()}
grid.table(pl_df[order(pl_df$talking_about, decreasing=TRUE), 
                 c('name', 'category', 'talking_about')][1:10,],
           rows=NULL)

# top pages based on page posting activity
if (dev.cur()!=1){dev.off()}
grid.table(pl_df[order(pl_df$post_activity, decreasing=TRUE), 
                 c('name', 'category', 'post_activity')][1:10,],
           rows=NULL)

# check correlation between fans and talking about for pages
clean_pl_df <- pl_df[complete.cases(pl_df),]
rsq <- format(cor(clean_pl_df$fans, clean_pl_df$talking_about) ^2, digits=3)
corr_plot <- ggplot(pl_df, aes(x=fans, y=talking_about))+ theme_bw() +
  geom_jitter(alpha=1/2) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x="Fans", y="Talking About") +
  annotate("text", label=paste("R-sq =", rsq), x=+Inf, y=1, hjust=1)
corr_plot



# plot page network using degree filter
degrees <- degree(pl_graph, mode="total")
degrees_df <- data.frame(ID=V(pl_graph)$id,
                          Name=V(pl_graph)$label, 
                          Degree=as.vector(degrees))
ids_to_remove <- degrees_df[degrees_df$Degree < 30, c('ID')]
ids_to_remove <- ids_to_remove / 10
# get filtered graph
filtered_pl_graph <- delete.vertices(pl_graph, ids_to_remove)
# plot the graph
tkplot(filtered_pl_graph, 
       vertex.size = 10,
       vertex.color="orange",
       vertex.frame.color= "white",
       vertex.label.color = "black",
       vertex.label.family = "sans",
       edge.width=0.2,  
       edge.arrow.size=0, 
       edge.color="grey",
       edge.curved=TRUE,
       layout = layout.fruchterman.reingold)


## page network properties

# graph properties

# diameter (length of longest path) of the network
diameter(pl_graph, directed=TRUE)

# get the longest path of the network
get_diameter(pl_graph, directed=TRUE)$label

# mean distance between two nodes in the network
mean_distance(pl_graph, directed=TRUE)

# distance between various important pages(nodes)
node_dists <- distances(pl_graph, weights=NA)
labels <- c("Premier League", pl_df[c(21, 22, 23, 24, 25), 'name'])
filtered_dists <- node_dists[c(1,21,22,23,24,25), c(1,21,22,23,24,25)]
colnames(filtered_dists) <- labels
rownames(filtered_dists) <- labels
if (dev.cur()!=1){dev.off()}
grid.table(filtered_dists)


# edge density of the graph
edge_density(pl_graph)

# Verify edge density of the graph
2801 / (582*581)


# transitivity - clustering coefficient
transitivity(pl_graph)


# compute coreness
page_names <- V(pl_graph)$label
page_coreness <- coreness(pl_graph)
page_coreness_df = data.frame(Page=page_names,
                              PageCoreness=page_coreness)

# max coreness
max(page_coreness_df$PageCoreness)
    
# view the core of the network
View(head(page_coreness_df[
  page_coreness_df$PageCoreness == max(page_coreness_df$PageCoreness),], 20))

# View the periphery of the network
View(head(page_coreness_df[
  page_coreness_df$PageCoreness == min(page_coreness_df$PageCoreness),], 20))


# degree
degree_plg <- degree(pl_graph, mode="total")
degree_plg_df <- data.frame(Name=V(pl_graph)$label, 
                            Degree=as.vector(degree_plg))
degree_plg_df <- degree_plg_df[order(degree_plg_df$Degree, decreasing=TRUE),]

# Closeness
closeness_plg <- closeness(pl_graph, mode="all", normalized=TRUE)
closeness_plg_df <- data.frame(Name=V(pl_graph)$label, 
                               Closeness=as.vector(closeness_plg))
closeness_plg_df <- closeness_plg_df[order(closeness_plg_df$Closeness, 
                                           decreasing=TRUE),]

# Betweenness
betweenness_plg <- betweenness(pl_graph)
betweenness_plg_df <- data.frame(Name=V(pl_graph)$label, 
                                 Betweenness=as.vector(betweenness_plg))
betweenness_plg_df <- betweenness_plg_df[order(betweenness_plg_df$Betweenness, 
                                               decreasing=TRUE),]

# view top pages based on above measures
View(head(degree_plg_df, 10))
View(head(closeness_plg_df, 10))
View(head(betweenness_plg_df, 10))


# correlation plots

plg_df <- data.frame(degree_plg, closeness_plg, betweenness_plg)

# degree vs closeness
rsq <- format(cor(degree_plg, closeness_plg) ^2, digits=3)
corr_plot <- ggplot(plg_df, aes(x=degree_plg, y=closeness_plg))+ theme_bw() +
  geom_jitter(alpha=1/2) +
  scale_y_log10() +
  labs(x="Degree", y="Closeness") +
  annotate("text", label=paste("R-sq =", rsq), x=+Inf, y=1, hjust=1)
corr_plot

# degree vs betweenness
rsq <- format(cor(degree_plg, betweenness_plg) ^2, digits=3)
corr_plot <- ggplot(plg_df, aes(x=degree_plg, y=betweenness_plg))+ theme_bw() +
  geom_jitter(alpha=1/2) +
  scale_y_log10() +
  labs(x="Degree", y="Betweenness") +
  annotate("text", label=paste("R-sq =", rsq), x=+Inf, y=1, hjust=1)
corr_plot





# Eigenvector Centrality
evcentrality_plg <- eigen_centrality(pl_graph)$vector
evcentrality_plg_df <- data.frame(Name=V(pl_graph)$label, 
                               EVcentrality=as.vector(evcentrality_plg))
evcentrality_plg_df <- evcentrality_plg_df[order(evcentrality_plg_df$EVcentrality, 
                                           decreasing=TRUE),]
View(head(evcentrality_plg_df, 10))


# Pagerank
pagerank_plg <- page_rank(pl_graph)$vector
pagerank_plg_df <- data.frame(Name=V(pl_graph)$label, 
                                  PageRank=as.vector(pagerank_plg))
pagerank_plg_df <- pagerank_plg_df[order(pagerank_plg_df$PageRank, 
                                                 decreasing=TRUE),]
View(head(pagerank_plg_df, 10))


# Kleinberg's HITS Score
hits_plg <- authority_score(pl_graph)$vector
hits_plg_df <- data.frame(Name=V(pl_graph)$label, 
                          AuthScore=as.vector(hits_plg))
hits_plg_df <- hits_plg_df[order(hits_plg_df$AuthScore, 
                                         decreasing=TRUE),]
View(head(hits_plg_df, 10))


# finding neighbours of page vertices
pl_neighbours <- neighbors(pl_graph, v=which(V(pl_graph)$label=="Southampton FC"))
pl_neighbours
pl_neighbours$label






## get communities / clusters in your network 

# cliques use different sizes here and experiment
clique_num(pl_graph)

count_max_cliques(pl_graph, min=10, max=10)

clique_list <- cliques(pl_graph, min=10, max=10)
for (clique in clique_list){
  print(clique$label)
  cat('\n\n')
}

# filtering graph to get important nodes based on degree
degrees <- degree(pl_graph, mode="total")
degrees_df <- data.frame(ID=V(pl_graph)$id,
                         Name=V(pl_graph)$label, 
                         Degree=as.vector(degree_plg))
ids_to_remove <- degrees_df[degrees_df$Degree < 30, c('ID')]
ids_to_remove <- ids_to_remove / 10

filtered_pl_graph <- delete.vertices(pl_graph, ids_to_remove)
fplg_undirected <- as.undirected(filtered_pl_graph)


# fast greedy clustering
fgc <- cluster_fast_greedy(fplg_undirected) 
layout <- layout_with_fr(fplg_undirected, 
                         niter=500, start.temp=5.744)
communities <- data.frame(layout)
names(communities) <- c("x", "y")
communities$cluster <- factor(fgc$membership)
communities$name <- V(fplg_undirected)$label

# get total pages in each cluster
table(communities$cluster)

# get page names in each cluster
community_groups <- unlist(lapply(groups(fgc), 
                                  function(item){
                                    pages <- communities$name[item]
                                    i <- 1; lim <- 4; s <- ""
                                    while(i <= length(pages)){
                                      start = i
                                      end = min((i+lim-1), length(pages))
                                      s <- paste(s, paste(pages[start:end], collapse=", "))
                                      s <- paste(s, "\n")
                                      i=i+lim
                                    }
                                    return(substr(s, 1, (nchar(s)-2)))
                                    
                                  })
)
if (dev.cur()!=1){dev.off()}
grid.table(community_groups)

# get modularity score
modularity(fgc)


comm_plot <- ggplot(communities, aes(x=x, y=y, color=cluster, label=name))
comm_plot <- comm_plot + geom_label(aes(fill = cluster), colour = "white")
comm_plot

plot(fgc, fplg_undirected, 
     vertex.size=15, 
     vertex.label.cex=0.8, 
     vertex.label=fgc$names, 
     edge.arrow.size=0, 
     edge.curved=TRUE,
     vertex.label.color="black",
     layout=layout.fruchterman.reingold)



# edge betweenness clustering - for enthusiasts (solution)
ebc <- cluster_edge_betweenness(fplg_undirected) 
layout <- layout_with_fr(fplg_undirected, 
                         niter=500, start.temp=5.744)

communities <- data.frame(layout)
names(communities) <- c("x", "y")
communities$cluster <- factor(ebc$membership)
communities$name <- V(fplg_undirected)$label

table(communities$cluster)

community_groups <- unlist(lapply(groups(ebc), 
                                  function(item){
                                    pages <- communities$name[item]
                                    i <- 1; lim <- 4; s <- ""
                                    while(i <= length(pages)){
                                      start = i
                                      end = min((i+lim-1), length(pages))
                                      s <- paste(s, paste(pages[start:end], collapse=", "))
                                      s <- paste(s, "\n")
                                      i=i+lim
                                    }
                                    return(substr(s, 1, (nchar(s)-2)))
  
                                  })
)
if (dev.cur()!=1){dev.off()}
grid.table(community_groups)


modularity(ebc)


comm_plot <- ggplot(communities, aes(x=x, y=y, color=cluster, label=name))
comm_plot <- comm_plot + geom_label(aes(fill = cluster), colour = "white")
comm_plot


plot(ebc, fplg_undirected, 
     vertex.size=15, 
     vertex.label.cex=0.8, 
     vertex.label=ebc$names, 
     edge.arrow.size=0, 
     edge.curved=TRUE,
     vertex.label.color="black",
     layout=layout.fruchterman.reingold)




