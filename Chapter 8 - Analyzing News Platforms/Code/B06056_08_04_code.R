# Chapter      :   8
# Description  :   Text summarization
############################################################################

library(lexRankr)

# read in the news article content
article_file <- 'Article-housing_crisis.txt'
article <- readChar(article_file, file.info(article_file)$size)

# view the news article content
View(article)

lines <- trimws(unlist(strsplit(gsub("[\r\n]", " ", article), '(([.] +)|([.]" +))')))

# view total lines extracted
cat('Total lines: ', length(lines))

# extract influential sentences
influential_sentences <- lexRank(text=lines, threshold=0.1,
                                 n=5, usePageRank=TRUE,
                                 damping=0.85, continuous=FALSE,
                                 sentencesAsDocs=TRUE, removePunc=TRUE,
                                 removeNum=TRUE, toLower=TRUE,
                                 stemWords=TRUE, rmStopWords=TRUE,
                                 Verbose=TRUE, returnTies=TRUE)
# view the influential sentences
View(influential_sentences)

# create text summary
summary <- paste(lines[sort(influential_sentences$docId)], collapse=". ")

# display summary
cat(summary)


# apply lexrank algorithm
article_file <- 'Article-chris_woakes.txt'
article <- readChar(article_file, file.info(article_file)$size)
lines <- trimws(unlist(strsplit(gsub("[\r\n]", " ", article), '(([.] +)|([.]" +))')))
influential_sentences <- lexRank(text=lines, threshold=0.1,
                                 n=5, usePageRank=TRUE,
                                 damping=0.85, continuous=FALSE,
                                 sentencesAsDocs=TRUE, removePunc=TRUE,
                                 removeNum=TRUE, toLower=TRUE,
                                 stemWords=TRUE, rmStopWords=TRUE,
                                 Verbose=TRUE, returnTies=TRUE)

# create summary
summary <- paste(lines[sort(influential_sentences$docId)], collapse=". ")

# display summary
cat(summary)