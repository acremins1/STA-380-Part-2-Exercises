# Directory name for ReutersCorpus
reuters_dir <- "data/ReutersC50/C50train"
# Get list of subdirectories
subdirs <- list.dirs(reuters_dir, recursive = FALSE, full.names = TRUE)
# Author names
author_names <- list.dirs(reuters_dir, recursive = FALSE, full.names = FALSE)
# Get list of files in each subdirectory
files <- lapply(subdirs, list.files, full.names = TRUE)

# Read in all files and store as a list of lists
# Each list element is a list of documents for a given author
authors <- lapply(files, function(x) lapply(x, readLines))

for (i in 1:length(authors)) {
    # Merge all documents for a given author into a single list
    authors[[i]] <- Reduce(c, authors[[i]])
}


# use tidytext to tokenize
library(tidytext)
library(dplyr)
library(stringr)

for (i in 1:length(authors)) {
# Keep only the top 100 words
  authors[[i]] <- authors[[i]] %>%
    tibble(text = .) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE) %>%
    head(50)
}

# Create a data frame with two columns: author and word
# Each row is a word for a given author
# This is the data frame that will be used for association rule mining
word_counts_raw <- data.frame(author = character(), word = character())
for (i in 1:length(authors)) {
  word_counts_raw <- rbind(word_counts_raw, data.frame(author = author_names[i], word = authors[[i]]$word))
}

# Write out the data frame to a csv file
# write.csv(df, file = "data/ReutersC50/wordCountsFromR.csv", row.names = FALSE)
# print("Hello")

library(tidyverse)
library(igraph)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)

# Association rule mining
# Adapted from code by Matt Taddy
# Adopted from in-class example


# Turn author into a factor
word_counts_raw$author = factor(word_counts_raw$author)

word_counts = split(x=word_counts_raw$word, f=word_counts_raw$author)

## Remove duplicates ("de-dupe")
word_counts = lapply(word_counts, unique)

## Cast this variable as a special arules "transactions" class.
wordtrans = as(word_counts, "transactions")
summary(wordtrans)

# Now run the 'apriori' algorithm
# Look at rules with support > .05 & confidence >.1 & length <= 4
wordrules = apriori(wordtrans, 
                     parameter=list(support=.05, confidence=.1, maxlen=4))


playlists_graph = associations2igraph(subset(wordrules, lift>4), associationsAsNodes = FALSE)
igraph::write_graph(playlists_graph, file='wordCounts.graphml', format = "graphml")