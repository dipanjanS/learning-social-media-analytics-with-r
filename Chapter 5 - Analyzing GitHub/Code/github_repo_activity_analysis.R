source('load_packages.R')


### Mining and analyzing repository activity in Github
### ==================================================


## Analyzing weekly commit frequency

# get weekly commit data
base_url <- 'https://api.github.com/repos/torvalds/linux/stats/commit_activity?'
repo_url <- paste0(base_url, api_id_param,arg_sep,api_pwd_param)
response <- fromJSON(repo_url)

# convert epoch to timestamp
response$week_conv <- as.POSIXct(response$week, origin="1970-01-01")

# visualize weekly commit frequency
ggplot(response, aes(week_conv, total)) + 
  geom_line(aes(color=total), size=1.5) +
  labs(x="Time", y="Total commits",
       title="Weekly GitHub commit frequency",
       subtitle="Commit history for the linux repository") + 
  theme_ipsum_rc()



## Analyzing Commit frequency distribution vs. Day of the week

# curate daily commit history
make_breaks <- function(strt, hour, minute, interval="day", length.out=31) {
  strt <- as.POSIXlt(strt)  
  strt <- ISOdatetime(strt$year+1900L, strt$mon+1L, 
                      strt$mday, hour=hour, min=minute, sec=0, tz="UTC")
  seq.POSIXt(strt, strt+(1+length.out)*60*60*24, by=interval)
}

daily_commits <- unlist(response$days)
days <- rep(c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'), 52)
time_stamp <- make_breaks(min(response$week_conv), hour=5, minute=30, interval="day", length.out=362)
df <- data.frame(commits=daily_commits, day=days, time=time_stamp)

# view the daily commit history dataset
View(df)

# visualize commit frequency distribution vs. day of week
ggplot(df, aes(x=day, y=commits, color=day)) +
  geom_boxplot(position='dodge') +
  scale_fill_ipsum() +
  labs(x="Day", y="Total commits",
       title="GitHub commit frequency distribution vs. Day of Week",
       subtitle="Commit history for the linux repository") + 
  theme_ipsum_rc(grid="Y")



## Analyzing daily commit frequency

# visualize daily commit frequency
ggplot(df, aes(x=time, y=commits, color=day)) + 
  geom_line(aes(color=day)) +
  geom_point(aes(shape=day)) +
  scale_fill_ipsum() +
  labs(x="Time", y="Total commits",
       title="Daily GitHub commit frequency",
       subtitle="Commit history for the linux repository") + 
  theme_ipsum_rc(grid="Y")



## Analyzing weekly commit frequency comparison

# get the commit participation history
base_url <- 'https://api.github.com/repos/torvalds/linux/stats/participation?'
repo_url <- paste0(base_url,api_id_param,arg_sep,api_pwd_param)
response <- fromJSON(repo_url)
response <- as.data.frame(response)

# get contributor frequency & curate dataset
response$contributors <- response$all - response$owner
response$week <- 1:52
response <- response[,c('week', 'contributors', 'owner')]
# format the dataset
df <- melt(response, id='week')

# visualize weekly commit frequency comparison
ggplot(data=df, aes(x=week, y=value, color=variable)) +
  geom_line(aes(color=variable)) +
  geom_point(aes(shape=variable)) +
  scale_fill_ipsum() +
  labs(x="Week", y="Total commits",
       title="Weekly GitHub commit frequency comparison",
       subtitle="Commit history for the linux repository") + 
  theme_ipsum_rc(grid="Y")



## Analyzing weekly code frequency history

# get the code frequency dataset
base_url <- 'https://api.github.com/repos/torvalds/linux/stats/code_frequency?'
code_freq_url <- paste0(base_url,api_id_param,arg_sep,api_pwd_param)
response <- fromJSON(code_freq_url)
df <- as.data.frame(response)

# format the dataset
colnames(df) <- c('time', 'additions', 'deletions')
df$deletions <- abs(df$deletions)
df$time <- as.Date(as.POSIXct(df$time, origin="1970-01-01"))
df <- melt(df, id='time')

# visualize the code frequency timeline
ggplot(df, aes(x=time, y=value, color=variable)) + 
  geom_line(aes(color = variable)) +
  geom_point(aes(shape = variable)) +
  scale_x_date(date_breaks="12 month", date_labels='%Y') +
  scale_y_log10(breaks = c(10, 100, 1000, 10000, 
                           100000, 1000000)) +
  labs(x="Time", y="Total code modifications (log scaled)",
       title="Weekly Github code frequency timeline",
       subtitle="Code frequency history for the linux repository") + 
  theme_ipsum_rc(grid="XY")






