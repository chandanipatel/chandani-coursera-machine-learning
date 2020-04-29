# Preload necessary R librabires
options(mc.cores=6)
options(java.parameters = "-Xmx40G")
library(NLP)
library(tm)
library(stringi)
library(RWeka)
library(ggplot2)
library(ngram)
library(wordcloud2)
library(RColorBrewer)
library(slam)
library(xtable)

library(here)
library(rio)
library(webshot)
library(htmlwidgets)

datasource_folder <- "final/en_US"

blogs_path <- "final/en_US/en_US.blogs.txt"
news_path <- "final/en_US/en_US.news.txt"
twitter_path <- "final/en_US/en_US.twitter.txt"

# Read blogs data in binary mode
conn <- file(blogs_path, open="rb"); blogs <- readLines(conn, encoding="UTF-8"); close(conn)
# Read news data in binary mode
conn <- file(news_path, open="rb"); news <- readLines(conn, encoding="UTF-8"); close(conn)
# Read twitter data in binary mode
conn <- file(twitter_path, open="rb"); twitter <- readLines(conn, encoding="UTF-8"); close(conn)
# Remove temporary variable
rm(conn)

# Compute statistics and summary info for each data type
stats_for_raw <- data.frame(
  FileName=c("en_US.blogs","en_US.news","en_US.twitter"),
  FileSizeinMB=c(file.info(blogs_path)$size/1024^2,
                 file.info(news_path)$size/1024^2,
                 file.info(twitter_path)$size/1024^2),
  t(rbind(sapply(list(blogs,news,twitter),stri_stats_general),
          WordCount=sapply(list(blogs,news,twitter),stri_stats_latex)[4,]))
)

stats_for_raw

set.seed(2008)
list_blogs <- blogs[sample(1:length(blogs), 12000, replace=FALSE)]
list_News <- news[sample(1:length(news), 12000, replace=FALSE)]
list_twitter <- twitter[sample(1:length(twitter), 12000, replace=FALSE)]

corpus <- VCorpus(DirSource(datasource_folder, encoding = 'UTF-8'))

corpus_lowercase <- tm_map(corpus, content_transformer(tolower))
corpus_low_punct <- tm_map(corpus_lowercase, removePunctuation)
corpus_low_punct_no <- tm_map(corpus_low_punct, removeNumbers)
corpus_low_punct_no_stop <- tm_map(corpus_low_punct_no, removeWords,stopwords("english"))
corpus_low_punct_no_stop_white <- tm_map(corpus_low_punct_no_stop, stripWhitespace)

gram1 = as.data.frame((as.matrix(  TermDocumentMatrix(corpus_low_punct_no_stop_white) )) ) 
gram1v <- sort(rowSums(gram1),decreasing=TRUE)
gram1d <- data.frame(word = names(gram1v),freq=gram1v)
gram1d[1:10,]

letterCloud(gram1d[1:200,], word = "1-G", size = 1)

wordcloud2(gram1d[1:100,], shape = 'star', color = "random-light", backgroundColor = "#F3F3F3")

ggplot(gram1d[1:30,], aes(x=reorder(word, freq),y=freq)) + 
  geom_bar(stat="identity", width=0.5, fill="tomato2") + 
  labs(title="Unigrams")+
  xlab("Unigrams") + ylab("Frequency") + 
  theme(axis.text.x=element_text(angle=65, vjust=0.6))
rm(gram1)
rm(gram1v)
rm(gram1d)
gc()

bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
gram2= as.data.frame((as.matrix(  TermDocumentMatrix(corpus_low_punct_no_stop_white,control = list(tokenize = bigram)) )) ) 
gram2v <- sort(rowSums(gram2),decreasing=TRUE)
gram2d  <- data.frame(word = names(gram2v),freq=gram2v)
gram2d[1:10,]
wordcloud2(gram2d[1:100,], shape = 'diamond', color = "random-light", backgroundColor = "#F3F3F3")


wordcloud2(gram2d[1:100,], shape = 'diamond', color = "random-light", backgroundColor = "#F3F3F3")

trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
gram3 = as.data.frame((as.matrix(  TermDocumentMatrix(corpus_low_punct_no_stop_white,control = list(tokenize = trigram)) )) ) 
gram3v <- sort(rowSums(gram3),decreasing=TRUE)
gram3d <- data.frame(word = names(gram3v),freq=gram3v)
gram3d[1:10,]

wordcloud2(gram3d[1:50,], shape = 'diamond', color = "random-light", backgroundColor = "#F3F3F3")


