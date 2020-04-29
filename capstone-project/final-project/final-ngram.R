options(mc.cores=20)
options(java.parameters = "-Xmx40G")

library(parallel)
library(stringr)
library(tm)
library(ggplot2)
library(ngram)

tidyText <- function(file, tidyfile) {
  
  # read in text
  con <- file(file, open="r")
  lines <- readLines(con)
  close(con)
  
  lines <- tolower(lines)
  
  # replace words that contain "@", "#", "http://", "https://" 
  # with space (especially for Twitter text)
  lines <- gsub("([^[:space:]]*)(@|#|http://|https://)([^[:space:]]*)", " ", lines)
  
  # split at all ".", ",", brackets and etc.
  lines <- unlist(strsplit(lines, "[.,:;!?(){}<>]|[][]+"))
  
  # replace all non-alphanumeric characters with a space at the beginning/end of a word.
  lines <- gsub("^[^a-z0-9]+|[^a-z0-9]+$", " ", lines) # at the begining/end of a line
  lines <- gsub("[^a-z0-9]+\\s", " ", lines) # before space
  lines <- gsub("\\s[^a-z0-9]+", " ", lines) # after space
  
  # split a string at spaces then remove the words 
  # that contain any non-alphabetic characters (excpet "-", "'")
  # then paste them together (separate them with spaces)
  lines <- unlist(lapply(lines, function(line){
    words <- unlist(strsplit(line, "\\s+"))
    words <- words[!grepl("[^a-z'-]", words, perl=TRUE)]
    paste(words, collapse=" ")}))
  
  # remove axcess spaces
  #lines <- gsub("\\s+", " ", lines) # remove mutiple spaces
  lines <- str_trim(lines) # remove spaces at the beginning/end of the line
  
  # drop blank lines
  lines <- lines[nchar(lines)>0]
  
  saveRDS(lines, file=tidyfile) 
}

execute <- function(n) {
  ## constants
  # original texts
  co_twitter_en = "data/en_US/en_US.twitter.txt"
  co_blogs_en = "data/en_US/en_US.blogs.txt"
  co_news_en = "data/en_US/en_US.news.txt"
  
  # cleaned texts 
  co_tidy_twitter_en = "tidy_twitter_en.rds"
  co_tidy_blogs_en = "tidy_blogs_en.rds"
  co_tidy_news_en = "tidy_news_en.rds"
  
  # n-grams
  co_4gram_en = "4gram_en.rds"
  co_3gram_en = "3gram_en.rds"
  co_2gram_en = "2gram_en.rds"
  co_1gram_en = "1gram_en.rds"
  co_3gram_notail_en = "3gram_notail_en.rds"
  
  
  
  # clean texts
  tidyText(co_twitter_en, co_tidy_twitter_en) # 12.52235 mins, 6,658 KB
  tidyText(co_news_en, co_tidy_news_en) # 45.31975 secs, 70,998 KB
  tidyText(co_blogs_en, co_tidy_blogs_en) # 9.873513 mins, 87,014 KB
  
  # merge texts
  df_news <- readRDS(co_tidy_news_en) # 340061 lines
  df_blogs <- readRDS(co_tidy_blogs_en) # 4532671 lines
  df_twitter <- readRDS(co_tidy_twitter_en) # 5030042 lines
  lines_df <- c(df_news, df_blogs, df_twitter)
  # remove lines that contain less than 3 words, or ngram() would throw errors.
  lines <- lines_df[str_count(lines_df, "\\s+")>1] # reduced 9902774 lines to 7607099 lines
  
  
  # get 3-grams
  trigram <- ngram(lines, n=3);# 7.619798 mins
  saveRDS(df, co_3gram_en) # 211,607 KB
  
  # get 2-grams
  bigram <- ngram(lines, n=2);
  saveRDS(df, co_2gram_en) # 211,607 KB
  
  # get 1-grams
  unigram <- ngram(lines, n=1);
  saveRDS(df, co_1gram_en)
  
}

system.time({
  mclapply(1, execute, mc.cores = 20)
})
