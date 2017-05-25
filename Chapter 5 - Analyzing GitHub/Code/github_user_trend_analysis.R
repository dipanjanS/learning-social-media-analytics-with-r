source('load_packages.R')

load('trending_repos.RData')

## Visualizing top contributing users on github

# get repository owner\user
trending_repos$user <- sapply(strsplit(trending_repos$full_name, '/'), `[`, 1)

# aggregate repository counts by user
repo_users <- aggregate(trending_repos$user, 
                        by=list(trending_repos$user), length)
colnames(repo_users) <- c('User', 'Count')
top_users_counts <- arrange(repo_users, desc(Count))[1:25,]

# visualize data
ggplot(top_users_counts, aes(x=User, y=Count, fill=User)) +
  geom_bar(stat="identity", position="dodge")+
  coord_flip() +
  geom_text(aes(label=Count), 
            vjust=0.3, 
            hjust=-0.1,
            position=position_dodge(.9), size=3) +
  scale_color_ipsum() +
  labs(x="User", y="Count",
       title="Top contributing users on GitHub",
       subtitle="Users with the most trending repositiories") + 
  theme_ipsum_rc(grid="X") +
  theme(legend.position="NA",
        axis.text.x = element_text(angle = 90, hjust = 1))



## Analyzing user activity metrics

# compute derived metrics
trending_repos$user <- sapply(strsplit(trending_repos$full_name, '/'), `[`, 1)
trending_repos$create_age <- as.integer(difftime(Sys.Date(),
                                                 trending_repos$created_at, 
                                                 units=c("days")))
trending_repos$update_age <- as.integer(difftime(Sys.Date(),
                                                 trending_repos$updated_at, 
                                                 units=c("days")))


# build aggregations
subset_df <- trending_repos[c('id', 'user', 'size', 'stargazers_count',
                              'forks', 'open_issues', 'create_age', 'update_age')]
stats_df <- sqldf("select user, count(*) as repo_count, avg(size) as mean_repo_size, 
                  sum(stargazers_count) as total_stargazers, sum(forks) as total_forks, 
                  sum(open_issues) as total_open_issues, avg(create_age) as mean_create_age, 
                  avg(update_age) as mean_update_age
                  from subset_df group by user
                  order by repo_count desc")

# filter and view stats
top_user_stats <- stats_df[1:20, ]
colnames(top_user_stats) <- c("User", "Total Repos", "Avg. Repo Size", "Total Stargazers", 
                              "Total Forks", "Total Open Issues", "Avg. Repo Create Age", "Avg. Repo Update Age"
)
View(top_user_stats)

# scale metric attributes
scale_col <- function(x){
  round(((x-min(x))/(max(x)-min(x))), 2)
}

scaled_stats <- cbind(top_user_stats[,1], 
                      as.data.frame(apply(top_user_stats[2:8], 
                                          2, scale_col)))
colnames(scaled_stats)[1] <- 'User'
scaled_stats_tf <- melt(scaled_stats, id='User')
colnames(scaled_stats_tf) <- c('User', 'Metric', 'Value')

# visualize data as a heatmap
ggplot(data=scaled_stats_tf, aes(x=Metric, y=User)) +
  geom_tile(aes(fill=Value)) +
  geom_text(aes(label=Value), 
            size=3) +
  scale_fill_gradient(low="#FFB607", high="#DB3D00") +
  theme_ipsum_rc() +
  labs(x="User", y="Metric",
       title="User Activity Metrics Heatmap",
       subtitle="Analyzing trending user activity metrics on GitHub") + 
  theme(legend.position="NA",
        axis.text.x = element_text(angle = 45, hjust = 1))


