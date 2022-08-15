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
docs <- lapply(files, function(x) lapply(x, readLines))

# Get the name of companies being talked about in each document

