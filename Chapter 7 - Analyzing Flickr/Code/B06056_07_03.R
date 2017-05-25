############################################################################
# Chapter      :   7
# Description  :   Cluster Analysis of Flickr Data
############################################################################


library(plyr)
library(dplyr)
library(cluster)
library(hrbrthemes)
library(ggplot2)



############################################################################
# Utility Methods
############################################################################
binFocalLengths <- function(focal_length){
  if(focal_length<10){
    "<10mm"
  }else if(focal_length>=10 && focal_length<20){
    "10-20 mm"
  }else if(focal_length>=20 && focal_length<50){
    "20-50mm"
  }else if(focal_length==50 ){
    "50mm"
  }else if(focal_length>50 && focal_length<100){
    "50-100mm"
  }else if(focal_length>=100 && focal_length<400){
    "100-400mm"
  }else{
    ">=400mm"
  }
}


# calculate convex hulls
find_hull <- function(df){
  df[chull(df$views, df$iso), ]
}

############################################################################
# Cluster based on views, iso and focal length
############################################################################

# remove NA data
nomissing_interesting <- na.omit(interestingDF[,c('views',
                                                  'focal_length',
                                                  'iso')])

# elbow analysis for kmeans
withiness_vector <- 0.0
for (i in 1:15){
  withiness_vector[i] <- sum(kmeans(nomissing_interesting,
                                     centers=i)$withinss)/(10^10)
}

eblowDF <-data.frame(withiness_vector,1:15)
colnames(eblowDF)<-c("withiness",
                     "cluster_num")

# plot clusters with regions marked out
ggplot(data = eblowDF, 
       aes(x = cluster_num, 
           y = withiness)) +
geom_point() +
geom_line() +
labs(x = "number of clusters", 
     y = "scaled withiness",
     title="Elbow Analysis") +
theme_ipsum_rc() 



# fit kmeans with optimal "k"
optimal_k <- 4
fit <- kmeans(nomissing_interesting,optimal_k)


# silhouette analysis for kmeans
dissimilarity_matrix <- daisy(nomissing_interesting) 
plot(silhouette(fit$cluster, 
                dissimilarity_matrix), 
     col=1:length(unique(fit$cluster)), 
     border=NA)



# add cluster number to dataframe
nomissing_interesting$cluster_num <- fit$cluster

# aggregate results
cluster_summary <- ddply(
                  nomissing_interesting,
                  .(iso, views,cluster_num),
                  summarize,
                  focal_length=max(focal_length)
                  )


# bin focal_lengths for plotting
cluster_summary$fl_bin <- sapply(cluster_summary$focal_length,binFocalLengths)
cluster_summary$fl_bin<- as.factor(cluster_summary$fl_bin)

# calculate cluster regions
hulls <- ddply(cluster_summary, "cluster_num", find_hull)


# plot clusters with regions marked out
ggplot(data = cluster_summary, 
       aes(x = views, 
           y = iso ,
           color=factor(cluster_num))) +
  geom_point(aes(shape=fl_bin)) + 
  scale_shape_manual(values=1:nlevels(cluster_summary$fl_bin)) +
  geom_polygon(data = hulls[hulls$cluster_num==1,], 
               alpha = 0.1,show.legend = FALSE) + 
  geom_polygon(data = hulls[hulls$cluster_num==2,], 
               alpha = 0.1,show.legend = FALSE) +
  geom_polygon(data = hulls[hulls$cluster_num==3,], 
               alpha = 0.1,show.legend = FALSE) +
  geom_polygon(data = hulls[hulls$cluster_num==4,], 
               alpha = 0.1,show.legend = FALSE) +
  labs(x = "views", y = "iso",title="Clustered Images") +
  theme_ipsum_rc() 

table(cluster_summary[,c('fl_bin','cluster_num')])
