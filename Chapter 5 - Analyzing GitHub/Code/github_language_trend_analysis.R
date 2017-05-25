source('load_packages.R')

load('trending_repos.RData')

## Visualizing top trending languages in Github

# aggregate repository counts by language
repo_languages <- aggregate(trending_repos$language, 
                            by=list(trending_repos$language), length)
colnames(repo_languages) <- c('Language', 'Count')

# get top 25 languages in GitHub
top_language_counts <- arrange(repo_languages, desc(Count))[1:25,]

# visualize data
ggplot(top_language_counts, aes(x=Language, y=Count, fill=Language)) +
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=Count), 
            vjust=-0.3, 
            position=position_dodge(.9), size=3) +
  scale_color_ipsum() +
  labs(x="Language", y="Repository Count",
       title="Top Trending Languages in GitHub") + 
  theme_ipsum_rc(grid="Y") +
  theme(legend.position="NA",
        axis.text.x = element_text(angle = 90, hjust = 1))



## Visualizing trending languages in Github over time

# aggregate repository counts by language over time
trending_repos$created_year <- format(as.Date(trending_repos$created_at), "%Y")
top_languages_by_year <- aggregate(trending_repos$language, 
                                   by=list(trending_repos$created_year, trending_repos$language), length)
colnames(top_languages_by_year) <- c('Year', 'Language', 'Count')
top_languages <- arrange(repo_languages, desc(Count))[1:15,c("Language")]
top_languages_by_year <- top_languages_by_year[top_languages_by_year$Language %in% top_languages,]

# visualize data
ggplot(top_languages_by_year, aes(x=Language, y=Count, fill=Year)) +
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=Count), 
            vjust=-0.3, 
            position=position_dodge(.9), size=2.5) +
  scale_color_ipsum() +
  labs(x="Language", y=" Repository Count",
       title="Trending Languages in GitHub over time") + 
  theme_ipsum_rc(grid="Y") +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))



## Analyzing languages with most open issues 

# aggregate mean open issues per language
repo_issues <- aggregate(trending_repos$open_issues, 
                         by=list(trending_repos$language), mean)
colnames(repo_issues) <- c('Language', 'Issues')
repo_issues$Issues <- round(repo_issues$Issues, 2)
top_issues_language_counts <- arrange(repo_issues, desc(Issues))[1:25,]

# visualize data
ggplot(top_issues_language_counts, aes(x=Language, y=Issues, fill=Language)) +
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=Issues), 
            vjust=-0.3, 
            position=position_dodge(.9), size=2) +
  scale_color_ipsum() +
  labs(x="Language", y="Issues",
       title="Languages with most open issues on GitHub",
       subtitle="Depicts top language repositories with highest mean open issue count") + 
  theme_ipsum_rc(grid="Y") +
  theme(legend.position="NA",
        axis.text.x = element_text(angle = 90, hjust = 1))



## Analyzing languages with the most open issues over time

# aggregate mean issues by language over time
top_issue_languages_by_year <- aggregate(trending_repos$open_issues, 
                                         by=list(trending_repos$created_year, trending_repos$language), mean)
colnames(top_issue_languages_by_year) <- c('Year', 'Language', 'Issues')
top_languages <- arrange(repo_issues, desc(Issues))[1:10, c("Language")]
top_issue_languages_by_year <- top_issue_languages_by_year[top_issue_languages_by_year$Language %in% top_languages,]
top_issue_languages_by_year$Issues <- round(top_issue_languages_by_year$Issues, 2)

# visualize data
ggplot(top_issue_languages_by_year, aes(x=Language, y=Issues, fill=Year)) +
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=Issues), 
            vjust=-0.3, 
            position=position_dodge(.9), size=2) +
  scale_color_ipsum() +
  labs(x="Language", y="Issues",
       title="Languages with most open issues in GitHub over time",
       subtitle="Depicts top language repositories with highest mean open issue count over time") + 
  theme_ipsum_rc(grid="Y") +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))


## Analyzing languages with the most helpful repos

# compute helpful repositories
trending_repos$helpful_repo <- (trending_repos$has_wiki & trending_repos$has_pages)

# aggregate helpful repositories by language
helpful_repos_language <- aggregate(trending_repos$helpful_repo, 
                                    by=list(trending_repos$language), sum)
colnames(helpful_repos_language) <- c('Language', 'Count')
top_helpful_repos <- arrange(helpful_repos_language, desc(Count))[1:25,]

# visualize data
ggplot(top_helpful_repos, aes(x=Language, y=Count, fill=Language)) +
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=Count), 
            vjust=-0.3, 
            position=position_dodge(.9), size=3) +
  scale_color_ipsum() +
  labs(x="Language", y="Count",
       title="Most helpful repositories in GitHub by Language") + 
  theme_ipsum_rc(grid="Y") +
  theme(legend.position="NA",
        axis.text.x = element_text(angle = 90, hjust = 1))




## Analyzing languages with the highest popularity score

# compute popularity score
trending_repos$popularity_score <- ((trending_repos$forks*2) + trending_repos$stargazers_count)

# aggregate repository popularity scores by language
popular_repos_languages <- aggregate(trending_repos$popularity_score, 
                                     by=list(trending_repos$language), sum)
colnames(popular_repos_languages) <- c('Language', 'Popularity')
popular_repos_languages$Popularity <- round(popular_repos_languages$Popularity, 1)
top_popular_repos <- arrange(popular_repos_languages, desc(Popularity))[1:25,]

# visualize data
ggplot(top_popular_repos, aes(x=Language, y=Popularity)) +
  geom_bar(stat="identity", position="dodge", fill="steelblue")+
  geom_text(aes(label=Popularity), 
            vjust=-0.3, 
            position=position_dodge(.9), size=2.5) +
  scale_color_ipsum() +
  labs(x="Language", y="Popularity",
       title="Languages with most Popularity Score in GitHub",
       subtitle="Depicts top language repositories with highest popularity score") + 
  theme_ipsum_rc(grid="Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



## Analyzing language correlations

# get repository owner and languages
trending_repos$owner <- sapply(strsplit(trending_repos$full_name, '/'), `[`, 1)
df <- trending_repos[,c('owner', 'language')]
df_pivot <- data.table(df)

# create owner-language usage matrix
owner_lang_matrix <- dcast.data.table(df_pivot, owner ~ language, 
                      fun.aggregate=length, 
                      value.var="language")
owner_lang_df <- as.data.frame(owner_lang_matrix)

# take a peek at the owner-language data frame
View(owner_lang_df)

# build language correlation matrix
lang_mat <- owner_lang_df[,2:length(colnames(owner_lang_df))]
lang_corr <- cor(lang_mat)

# transform language correlations
diag(lang_corr) <- NA
lang_corr[upper.tri (lang_corr)] <- NA
lang_corr_final <- melt(lang_corr)

# get highly correlated languages
filtered_corr <- lang_corr_final[which(lang_corr_final$value >= 0.7),]
View(filtered_corr)
