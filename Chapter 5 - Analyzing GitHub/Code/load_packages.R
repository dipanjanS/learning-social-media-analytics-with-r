library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(reshape2)
library(hrbrthemes)
library(sqldf)
library(lubridate)
library(corrplot)
library(data.table)

# add your own tokens here
auth.id <- 'XXXXXXXXXXXXXX'
auth.pwd <- 'XXXXXXXXXXXXXXXXXXXXXXXXX'
api_id_param <- paste0('client_id=', auth.id) 
api_pwd_param <- paste0('client_secret=', auth.pwd)
arg_sep = '&'