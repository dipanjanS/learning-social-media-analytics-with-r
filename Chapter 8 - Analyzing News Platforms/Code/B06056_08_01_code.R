############################################################################
# Chapter      :   8
# Description  :   Connect to Guardian and collect sample articles data 
############################################################################


library(GuardianR)
library(RCurl)
library(jsonlite)
library(magrittr)
library(tidyjson)
library(xml2)
library(curl)
library(rvest)

# The uri extracted from the online page

URI = "http://content.guardianapis.com/search?from-date=1016-06-01&to-date=2016-06-25&section=commentisfree&q=brexit&api-key=864b10a5-c6c8-4a6f-a375-0cc40b7da335"

# Making the API call

json_res <- getURL(URI, .mapUnicode=TRUE)

# Find out the number of pages

num_pages = json_res %>% enter_object("response") %>% spread_values(num_pages = jnumber("pages"))
num_pages = num_pages$num_pages

# Initialize an empty data fram
out_df = data.frame()
for (num in 1:num_pages){
    uriTemplate <- "http://content.guardianapis.com/search?from-date=1016-06-01&to-date=2016-06-25&section=commentisfree&q=brexit&api-key=864b10a5-c6c8-4a6f-a375-0cc40b7da335&page=%s"
    apiurl<- sprintf(uriTemplate,num)
    json_res <- getURL(apiurl, .mapUnicode=TRUE)
    urls <- as.data.frame(json_res %>% enter_object("response") %>% enter_object("results")  %>%
                              gather_array() %>% 
                              spread_values(url = jstring("webUrl"), type = jstring("type"), 
                                            sectionName = jstring("sectionName"), 
                                            webTitle = jstring("webTitle"),
                                            sectionId = jstring("sectionId")
                              ))
    urls$document.id <- NULL
    urls$array.index <- NULL
    out_df = rbind(out_df, urls)
    Sys.sleep(10)
}

# Extracting the HTML data

b <- read_html(curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0")))

# Find the nodes for paragraph tags

paragraph_nodes = html_nodes(b, xpath = ".//p")

# Tidy up by removing whitespaces, newlines and collapsing all nodes into a single node

nodes<-trimws(html_text(paragraph_nodes))
nodes<-gsub("\n", "", nodes)
nodes<-gsub("  ", "", nodes)
content = paste(nodes, collapse = " ")
