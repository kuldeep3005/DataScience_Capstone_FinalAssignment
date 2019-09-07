# Coursera DS Capstone Project
# Preparing data for downstream analysis and models
# 2019-09-05

# Libraries and options ####
ptm <- proc.time()
library(tm)
library(ggplot2)
library(dplyr)
library(SnowballC)
library(stringi)
library(doParallel)
library(text2vec)

setwd("C:/Users/kuldeep.singh.meena/Downloads/R-Data/Capstone/Final_assignment")

source("./used_functions.R")

proc.time() - ptm
################################
ptm <- proc.time()

twitter.data <- "./Data/final/en_US/en_US.twitter.txt"
blog.data <- "./Data/final/en_US/en_US.blogs.txt"
news.data <- "./Data/final/en_US/en_US.news.txt"
twitter.file <- file(twitter.data,"rb")
twitter <- readLines(twitter.file, skipNul = TRUE, encoding = "UTF-8")
close(twitter.file)
blog.file <- file(blog.data,"rb")
blog <- readLines(blog.file, skipNul = TRUE, encoding = "UTF-8")
close(blog.file)
news.file <- file(news.data,"rb")
news <- readLines(news.file, skipNul = TRUE, encoding = "UTF-8")
close(news.file)
profanities = readLines('bad-words.txt')

proc.time()-ptm
###################################
ptm <- proc.time()

all <- c(twitter,blog,news)
rm(twitter,twitter.data,twitter.file,blog,blog.data,blog.file,news,news.data,news.file)

proc.time() - ptm
###################################
ptm <- proc.time()


set.seed(1220)
all2 <- sample(all,100000)
rm(all)

train_tokens <- all2 %>%
  graphToSpace %>%
  tolower %>% 
  removeConfounders %>%
  removePunctuation %>%
  removeNumbers %>%
  removeWords(profanities) %>%
  removeWords(letters[!letters %in% c("a","i")]) %>%
  stripWhitespace

rm(all2)
proc.time() - ptm
####################################
ptm <- proc.time()

saveRDS(train_tokens,"train_tokens.Rdata")
train_tokens <- readRDS("train_tokens.Rdata")

proc.time()-ptm
####################################
ptm <- proc.time()

# register parallel backend 
N_WORKERS <- 4
registerDoParallel(N_WORKERS) 

# define number of splits 
N_SPLITS <- 4

jobs <- train_tokens %>%
  split_into(N_SPLITS) %>%
  lapply(itoken)
rm(train_tokens)

# UNIGRAM text2vec object
vocab <- create_vocabulary(jobs, ngram = c(1L, 1L))
pruned_vocab <- prune_vocabulary(vocab,term_count_min = 10)
rm(vocab)

freqr <- pruned_vocab[1]
rm(pruned_vocab)
freqr <- data.frame(freqr)
freqr <- freqr[, 1:2]
freqr <- freqr %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)

saveRDS(freqr,"unigrams.Rdata")
rm(freqr)
gc()

# BIGRAM text2vec object
vocab2 <- create_vocabulary(jobs, ngram = c(2L, 2L))
pruned_vocab2 <- prune_vocabulary(vocab2,term_count_min = 4)
rm(vocab2)

freqr2 <- pruned_vocab2[1]
rm(pruned_vocab2)
freqr2 <- data.frame(freqr2)
freqr2 <- freqr2[, 1:2]
freqr2 <- freqr2 %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)

spl <- strsplit(freqr2$word,"_")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
bi_df <- data.frame(w1,w2,freqr2$frequency)
rm(spl,w1,w2,freqr2)

saveRDS(bi_df,"bigrams.Rdata")
rm(bi_df)
gc()

# trigram text2vec object
vocab3 <- create_vocabulary(jobs, ngram = c(3L, 3L))
pruned_vocab3 <- prune_vocabulary(vocab3,term_count_min = 2)
rm(vocab3)

freqr3 <- pruned_vocab3[1]
rm(pruned_vocab3)
freqr3 <- data.frame(freqr3)
freqr3 <- freqr3[, 1:2]
freqr3 <- freqr3 %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)

spl <- strsplit(freqr3$word,"_")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
w3 <- sapply(spl,"[[",3)
tri_df <- data.frame(w1,w2,w3,freqr3$frequency)
rm(spl,w1,w2,w3,freqr3)

saveRDS(tri_df,"trigrams.Rdata")
rm(tri_df)
gc()

# quadgram text2vec object
vocab4 <- create_vocabulary(jobs, ngram = c(4L, 4L))
pruned_vocab4 <- prune_vocabulary(vocab4,term_count_min = 2)
rm(vocab4)

freqr4 <- pruned_vocab4[1]
rm(pruned_vocab4)
freqr4 <- data.frame(freqr4)
freqr4 <- freqr4[, 1:2]
freqr4 <- freqr4 %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)

spl <- strsplit(freqr4$word,"_")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
w3 <- sapply(spl,"[[",3)
w4 <- sapply(spl,"[[",4)
quad_df <- data.frame(w1,w2,w3,w4,freqr4$frequency)
rm(spl,w1,w2,w3,w4,freqr4)

saveRDS(quad_df,"quadgrams.Rdata")
rm(quad_df)
gc()
# pentagram text2vec object
vocab5 <- create_vocabulary(jobs, ngram = c(5L, 5L))
pruned_vocab5 <- prune_vocabulary(vocab5,term_count_min = 2)
rm(vocab5)

freqr5 <- pruned_vocab5[1]
rm(pruned_vocab5)
freqr5 <- data.frame(freqr5)
freqr5 <- freqr5[, 1:2]
freqr5 <- freqr5 %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)

spl <- strsplit(freqr5$word,"_")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
w3 <- sapply(spl,"[[",3)
w4 <- sapply(spl,"[[",4)
w5 <- sapply(spl,"[[",5)
penta_df <- data.frame(w1,w2,w3,w4,w5,freqr5$frequency)
rm(spl,w1,w2,w3,w4,w5,freqr5)

saveRDS(penta_df,"pentagrams.Rdata")
rm(penta_df)
gc()

# ectagram text2vec object
vocab6 <- create_vocabulary(jobs, ngram = c(6L, 6L))
pruned_vocab6 <- prune_vocabulary(vocab6,term_count_min = 2)
rm(vocab6)

freqr6 <- pruned_vocab6[1]
rm(pruned_vocab6)
freqr6 <- data.frame(freqr6)
freqr6 <- freqr6[, 1:2]
freqr6 <- freqr6 %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)

spl <- strsplit(freqr6$word,"_")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
w3 <- sapply(spl,"[[",3)
w4 <- sapply(spl,"[[",4)
w5 <- sapply(spl,"[[",5)
w6 <- sapply(spl,"[[",6)
ecta_df <- data.frame(w1,w2,w3,w4,w5,w6,freqr6$frequency)
rm(spl,w1,w2,w3,w4,w5,w6,freqr6)

saveRDS(ecta_df,"ectagrams.Rdata")
rm(ecta_df)
gc()

proc.time() - ptm
######################################
ptm <- proc.time()

uni_df <- readRDS("unigrams.Rdata")
bi_df <- readRDS("bigrams.Rdata")
tri_df <- readRDS("trigrams.Rdata")
quad_df <- readRDS("quadgrams.Rdata")
penta_df <- readRDS("pentagrams.Rdata")
ecta_df <- readRDS("ectagrams.Rdata")

proc.time()-ptm
####################################