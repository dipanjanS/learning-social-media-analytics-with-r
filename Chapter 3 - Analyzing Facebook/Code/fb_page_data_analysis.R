library(Rfacebook)
library(ggplot2)
library(scales)
library(dplyr)
library(magrittr)
source('multiple_plots.R')


token = 'XXXXXXXXXXXXXXXXXXXXXXXXXXX'


# get page stats
man_united <- getPage(page='manchesterunited', n=100000, 
                      token=token,since='2014/01/01', 
                      until='2017/01/17')
man_city <- getPage(page='mancity', n=100000, 
                      token=token,since='2014/01/01', 
                      until='2017/01/17')
arsenal <- getPage(page='Arsenal', n=100000, 
                    token=token,since='2014/01/01', 
                    until='2017/01/17')

# save data for later use
save(man_united, file='man_united.RData')
save(man_city, file='man_city.RData')
save(arsenal, file='arsenal.RData')

# load data for analysis
load('man_united.RData')
load('man_city.RData')
load('arsenal.RData')


# combine data frames
colnames <- c('from_name', 'created_time', 'type', 
              'likes_count', 'comments_count', 'shares_count',
              'id', 'message', 'link')
page_data <- rbind(man_united[colnames], man_city[colnames], arsenal[colnames])
names(page_data)[1] <- "Page"
# format post creation time
page_data$created_time <- as.POSIXct(page_data$created_time, 
                                     format = "%Y-%m-%dT%H:%M:%S+0000", 
                                     tz = "GMT")
# add new time based columns
page_data$month <- format(page_data$created_time, "%Y-%m")
page_data$year <- format(page_data$created_time, "%Y")


# total records
nrow(page_data)

# post counts per page
post_page_counts <- aggregate(page_data$Page, by=list(page_data$Page), length)
colnames(post_page_counts) <- c('Page', 'Count')
ggplot(post_page_counts, aes(x=Page, y=Count, fill=Page)) +   
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label=Count),  vjust=-0.3, position=position_dodge(.9), size=4) +
  scale_fill_brewer(palette="Set1")  +
  theme_bw()


# post counts by post type per page
post_type_counts <- aggregate(page_data$type, by=list(page_data$Page, page_data$type), length)
colnames(post_type_counts) <- c('Page', 'Type', 'Count')
ggplot(post_type_counts, aes(x=Page, y=Count, fill=Type)) +   
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label=Type),  vjust=-0.5, position=position_dodge(.9), size=3) +
  scale_fill_brewer(palette="Set1")  +
  theme_bw()

# average likes per page by post type
likes_by_post_type <- aggregate(page_data$likes_count, 
                                by=list(page_data$Page, page_data$type), mean)
colnames(likes_by_post_type) <- c('Page', 'Type', 'AvgLikes')
ggplot(likes_by_post_type, aes(x=Page, y=AvgLikes, fill=Type)) +   
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label=Type),  vjust=-0.5, position=position_dodge(.9), size=3) +
  scale_fill_brewer(palette="Set1")  +
  theme_bw()

# average shares per page by post type
shares_by_post_type <- aggregate(page_data$shares_count, 
                                by=list(page_data$Page, page_data$type), mean)
colnames(shares_by_post_type) <- c('Page', 'Type', 'AvgShares')
ggplot(shares_by_post_type, aes(x=Page, y=AvgShares, fill=Type)) +   
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label=Type),  vjust=-0.5, position=position_dodge(.9), size=3) +
  scale_fill_brewer(palette="Set1")  +
  theme_bw()


# page engagement over time
page_posts_df <- aggregate(page_data[['type']], by=list(page_data$month, page_data$Page), length)
colnames(page_posts_df) <- c('Month', 'Page', 'Count')
page_posts_df$Month <- as.Date(paste0(page_posts_df$Month, "-15"))
ggplot(page_posts_df, aes(x=Month, y=Count, group=Page)) + 
  geom_point(aes(shape=Page)) + 
  geom_line(aes(color=Page)) +
  theme_bw() + scale_x_date(date_breaks="3 month", date_labels='%m-%Y') +
  ggtitle("Page Engagement over time")


## user engagement with page over time
# create metric aggregation function
aggregate.metric <- function(metric, data) {
  m <- aggregate(data[[paste0(metric, "_count")]], list(month = data$month), 
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}

# get aggregated stats per page
mu_df <- subset(page_data, Page=="Manchester United")
mu_stats_df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric, data=mu_df)
mu_stats_df <- do.call(rbind, mu_stats_df.list)
mu_stats_df <- mu_stats_df[order(mu_stats_df$month), ]

afc_df <- subset(page_data, Page=="Arsenal")
afc_stats_df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric, data=afc_df)
afc_stats_df <- do.call(rbind, afc_stats_df.list)
afc_stats_df <- afc_stats_df[order(afc_stats_df$month), ]

mc_df <- subset(page_data, Page=="Manchester City")
mc_stats_df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric, data=mc_df)
mc_stats_df <- do.call(rbind, mc_stats_df.list)
mc_stats_df <- mc_stats_df[order(mc_stats_df$month), ]

# build visualizations on aggregated stats per page
p1 <- ggplot(mu_stats_df, aes(x=month, y=x, group=metric)) + 
  geom_point(aes(shape = metric)) + 
  geom_line(aes(color = metric)) +
  theme_bw() + scale_x_date(date_breaks="3 month", date_labels='%m-%Y') +
  scale_y_log10("Avg stats/post", breaks = c(10, 100, 1000, 10000, 50000)) +
  ggtitle("Manchester United")

p2 <- ggplot(afc_stats_df, aes(x=month, y=x, group=metric)) + 
  geom_point(aes(shape = metric)) + 
  geom_line(aes(color = metric)) +
  theme_bw() + scale_x_date(date_breaks="3 month", date_labels='%m-%Y') +
  scale_y_log10("Avg stats/post", breaks = c(10, 100, 1000, 10000, 50000)) +
  ggtitle("Arsenal")

p3 <- ggplot(mc_stats_df, aes(x=month, y=x, group=metric)) + 
  geom_point(aes(shape = metric)) +
  geom_line(aes(color = metric)) +
  theme_bw() + scale_x_date(date_breaks="3 month", date_labels='%m-%Y') +
  scale_y_log10("Avg stats/post", breaks = c(10, 100, 1000, 10000, 50000)) +
  ggtitle("Manchester City")

# view the plots together
multiplot(p1, p2, p3)



## trending page content
# trending posts by likes per page
trending_posts_likes <- page_data %>% 
                        group_by(Page, year) %>%
                        filter(likes_count == max(likes_count))
trending_posts_likes <- as.data.frame(trending_posts_likes)
View(trending_posts_likes[,c('Page', 'year', 'month', 'type', 
                             'likes_count', 'comments_count', 'shares_count',
                             'message', 'link')])

# trending posts by shares per page
trending_posts_shares <- page_data %>% 
                         group_by(Page, year) %>%
                         filter(shares_count == max(shares_count))
trending_posts_shares <- as.data.frame(trending_posts_shares)
View(trending_posts_shares[,c('Page', 'year', 'month', 'type', 
                              'likes_count', 'comments_count', 'shares_count',
                              'message', 'link')])


# top influential users on popular posts

# extract post comment data
mu_top_post_2015 <- getPost(post='7724542745_10153390997792746', token=token, 
                         n = 5000, comments = TRUE)
afc_top_post_2014 <- getPost(post='20669912712_10152350372162713', token=token, 
                             n = 5000, comments = TRUE)

# save the data for future analysis
save(mu_top_post_2015, file='mu_top_post_2015.RData')
save(afc_top_post_2014, file='afc_top_post_2014.RData')

# load top post comments
load('mu_top_post_2015.RData')
load('afc_top_post_2014.RData')

# get top influential users for United post
mu_top_post_2015$post[, c('from_name', 'message')] 

mu_post_comments <- mu_top_post_2015$comments
View(mu_post_comments[order(mu_post_comments$likes_count, 
                       decreasing=TRUE),][1:10, 
                                          c('from_name', 'likes_count', 'message')])


# get top influential users for Arsenal post
afc_top_post_2014$post[, c('from_name', 'message')] 
afc_post_comments <- afc_top_post_2014$comments
View(afc_post_comments[order(afc_post_comments$likes_count, 
                             decreasing=TRUE),][1:10, 
                                                c('from_name', 'likes_count', 'message')])


