source('load_packages.R')

load('trending_repos.RData')

## Analyzing trending repositories created over time

# extract date elements 
trending_repos$created_year <- format(as.Date(trending_repos$created_at), "%Y")
trending_repos$created_monthyear <- format(as.Date(trending_repos$created_at), "%b-%Y")

# build aggregations
repos_by_created_time <- aggregate(trending_repos$created_monthyear, 
                                   by=list(trending_repos$created_monthyear),
                                   length)
colnames(repos_by_created_time) <- c("CreateTime", "Count")

# format dates
repos_by_created_time$CreateTime <- mdy(repos_by_created_time$CreateTime)

# visualize data
ggplot(repos_by_created_time, aes(x=CreateTime, y=Count)) +
  geom_line(aes(color=Count), size=1.5) +
  scale_x_date(date_breaks="3 month", date_labels='%b-%Y')+
  geom_text(aes(label=Count), 
            vjust=-0.3, 
            position=position_dodge(.9), size=3) +
  labs(x="Creation Time", y="Trending Repository Count",
       title="Trending Repositories vs. Creation Time",
       subtitle="Total trending repositories created in GitHub over time") + 
  theme_ipsum_rc(grid="XY") +
  theme(legend.position="right",
        axis.text.x = element_text(angle = 90, hjust = 1))



## Analyzing trending repositories last updated over time

# extract date elements
trending_repos$updated_monthyear <- format(as.Date(trending_repos$updated_at), "%b-%Y")

# build aggregations
repos_by_updated_time <- aggregate(trending_repos$updated_monthyear, 
                                   by=list(trending_repos$updated_monthyear),
                                   length)
colnames(repos_by_updated_time) <- c("UpdateTime", "Count")

# format dates
repos_by_updated_time$UpdateTime <- mdy(repos_by_updated_time$UpdateTime)

# visualize data
ggplot(repos_by_updated_time, aes(x=UpdateTime, y=Count)) +
  geom_line(aes(color=Count), size=1.5) +
  scale_x_date(date_breaks="2 week", date_labels='%b-%Y')+
  geom_text(aes(label=Count), 
            vjust=-0.3, 
            position=position_dodge(.9), size=3) +
  labs(x="Updation Time", y="Trending Repository Count",
       title="Trending Repositories vs. Updation Time",
       subtitle="Total trending repositories last updated in GitHub over time") + 
  theme_ipsum_rc(grid="XY") +
  theme(legend.position="right",
        axis.text.x = element_text(angle = 90, hjust = 1))



## Analyzing repository metrics

# create ageing variables
trending_repos$create_age <- as.integer(difftime(Sys.Date(),
                                                 trending_repos$created_at, 
                                                 units=c("days")))
trending_repos$update_age <- as.integer(difftime(Sys.Date(),
                                                 trending_repos$updated_at, 
                                                 units=c("days")))

# get repository owners
trending_repos$owner <- sapply(strsplit(trending_repos$full_name, '/'), `[`, 1)


# aggregate metrics per user
subset_df <- trending_repos[c('id', 'owner', 'size', 'stargazers_count',
                             'forks', 'open_issues', 'create_age', 'update_age')]
stats_df <- sqldf("select owner, count(*) as repo_count, avg(size) as mean_repo_size, 
                  sum(stargazers_count) as total_stargazers, sum(forks) as total_forks, 
                  sum(open_issues) as total_open_issues, avg(create_age) as mean_create_age, 
                  avg(update_age) as mean_update_age
                  from subset_df group by owner
                  order by repo_count desc")

# transform dataset into attributes & values
corr_df <- stats_df[c('repo_count', 'mean_repo_size', 'total_stargazers', 'total_forks', 
                      'total_open_issues', 'mean_create_age', 'mean_update_age')]
corr_params <- melt(corr_df)
colnames(corr_params) <- c("Attribute", "Value")

# visualize repository metric distributions
ggplot(corr_params, aes(x=Attribute, y=Value, color=Attribute)) +
  geom_boxplot(position='dodge') +
  scale_fill_ipsum() +
  labs(x="Metric", y="Value",
       title="Comparing GitHub repository metric distributions",
       subtitle="Viewing distributions for various repository metrics") + 
  theme_ipsum_rc(grid="Y") +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 100000)) +
  theme(legend.position="NA",
        axis.text.x = element_text(angle = 90, hjust = 1))



## Visualizing repository metric correlations

# compute correlation coefficients
corrs <- cor(corr_df)

# visualize correlation coefficient matrix
corrplot(corrs, method="number", type="lower")



## Analyzing relationship between stargazers and repositories counts

# get correlation coefficient
corr <- format(cor(corr_df$total_stargazers, corr_df$repo_count), digits=3)

# visualize relationship
ggplot(corr_df, aes(x=total_stargazers, y=repo_count))+ 
  theme_ipsum_rc() +
  geom_jitter(alpha=1/2) +
  geom_smooth(method=loess) +
  labs(x="Stargazers", y="Repositories",
       title="Correlation between Stargazers & Total Repositories") +
  annotate("text", label=paste("Corr =", corr), x=+Inf, y=10, 
           hjust=1)


## Analyzing relationship between stargazers and fork counts

# get correlation coefficient
corr <- format(cor(corr_df$total_stargazers, corr_df$total_forks), digits=3)

# visualize relationship
ggplot(corr_df, aes(x=total_stargazers, y=total_forks))+ 
  theme_ipsum_rc() +
  geom_jitter(alpha=1/2) +
  geom_smooth(method=loess) +
  labs(x="Stargazers", y="Forks",
       title="Correlation between Stargazers & Forks") +
  annotate("text", label=paste("Corr =", corr), x=+Inf, y=10, 
           hjust=1)

# build polynomil regression loess model
prm <- loess(total_stargazers ~ total_forks, corr_df)

# view model details
summary(prm)

# predict total stargazers for 5000 forks
predict(prm, 5000)

# check with sample datapoints in dataset
filter(corr_df, total_forks>=4900 & total_forks <= 5100)[, c('total_stargazers', 'total_forks')]



## Analyzing relationship between total forks, repository count and health

# compute repository health
corr_df$repo_health <- ifelse(corr_df$total_open_issues <= 50, 
                              'Healthy', 
                              'Not Healthy')

# get correlation coefficient
corr <- format(cor(corr_df$total_forks, corr_df$repo_count), digits=3)

# visualize relationship
ggplot(corr_df, aes(x=total_forks, y=repo_count, color=repo_health, shape=repo_health)) + 
  theme_ipsum_rc() +
  geom_jitter(alpha=1/2) +
  geom_smooth(method=loess) +
  labs(x="Total Forks", y="Repositories",
       title="Relationship between Forks, Total Repositories & Health") +
  annotate("text", label=paste("Corr =", corr), x=+Inf, y=10, 
           hjust=1)
