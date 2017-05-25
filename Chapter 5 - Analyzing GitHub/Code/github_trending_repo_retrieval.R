source('load_packages.R')

## function to get trending repositiories
get.trending.repositories <- function(timeline.dates, auth.id, auth.pwd){
  # set parameters
  base_url <- 'https://api.github.com/search/repositories?'
  api_id_param <- paste0('client_id=', auth.id) 
  api_pwd_param <- paste0('client_secret=', auth.pwd)
  per_page <- 100
  top.repos.df <- data.frame()
  pb <- txtProgressBar(min = 0, max = length(timeline.dates), style = 3)
  # for each pair of dates in the list get all trending repos
  for (i in seq(1,length(timeline.dates), by=2)){
    start_date <- timeline.dates[i]
    end_date <- timeline.dates[i+1]
    query <- paste0('q=created:%22', start_date, '%20..%20',
                    end_date, '%22%20stars:%3E=500')
    url <- paste0(base_url, query, arg_sep, api_id_param,arg_sep,api_pwd_param)
    response <- fromJSON(url)
    total_repos <- min(response$total_count, 1000)
    count <- ceiling(total_repos / per_page)
    # convert data into data frame
    for (p in 1:count){
      page_number <- paste0('page=', p)
      per_page_count <- paste0('per_page=', per_page)
      page_url <- paste0(url, arg_sep, page_number, arg_sep, per_page_count)
      response <- fromJSON(page_url)
      items <- response$items
      items <- items[, c('id', 'name', 'full_name', 'size', 'fork', 
                         'stargazers_count', 'watchers', 'forks', 'open_issues', 
                         'language', 'has_issues', 'has_downloads', 'has_wiki', 
                         'has_pages', 'created_at', 'updated_at', 'pushed_at',
                         'url', 'description')]
      top.repos.df <- rbind(top.repos.df, items)
    }
    setTxtProgressBar(pb, i+1)
  }
  return (top.repos.df)
}

# set timeline
dates <- c('2014-01-01', '2014-03-31', 
           '2014-04-01', '2014-06-30',
           '2014-07-01', '2014-09-30',
           '2014-10-01', '2014-12-31',
           '2015-01-01', '2015-03-31', 
           '2015-04-01', '2015-06-30',
           '2015-07-01', '2015-09-30',
           '2015-10-01', '2015-12-31',
           '2016-01-01', '2016-03-31', 
           '2016-04-01', '2016-06-30',
           '2016-07-01', '2016-09-30',
           '2016-10-01', '2016-12-31')

# retrieve trending repos
trending_repos <- get.trending.repositories(timeline.dates=dates, 
                                            auth.id=auth.id, 
                                            auth.pwd=auth.pwd)

# check total trending repos retrieved
nrow(trending_repos)

# take a peek at the data
View(trending_repos)

# save the dataset
save(trending_repos, file='trending_repos.RData')
