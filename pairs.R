# If you work with free recall tasks and items belonging to categories, 
# it may be useful to know how often an item from a given category is recalled 
# immediately before or after an item from another category. This tool counts 
#the occurence of all possible category-pairs for all your participants.
#
# It expects the database to be in a very specific format; namely:
# - Comma separated values (.csv file)
# - Wide format (one line per participant; one recall per column)
# - Previously coded recalls (numbers in place of categories)
# - First column is your participant key




# 1. Get a sequence from the data

# name your file "input.csv" or change here accordingly
# place file in project's workspace of change path accordingly
data <- read.csv("input.csv")

# create sequence 
sequence <- data.frame(seq = apply(
  data[-1], 1, function(x) paste(
    x[!is.na(x)], 
    collapse = "")
  ))


# 2. Get all possible pairs

# get unique categories
categories <- unique(unlist(data[-1]))
categories <- categories[!is.na(categories)]

# combinations
pairs <- apply(
  expand.grid(categories, categories), 1, paste, collapse = "")


# 3. Function: count occurrences of a pattern 'sub' in a sequence 'string'

count_sub <- function(sub,string) {
  gg <- gregexpr(paste0("(?=",sub,")"), string, perl = TRUE)[[1]]
  if (length(gg)==1 && gg==-1) 0 else length(gg)
}


# 4. Search for every combination in 'categories' in every row of 'sequence'

# nested 'apply'
results <- t(sapply(sequence$seq, function(seq) {
  sapply(pairs, function(pair) {
    count_sub(pair, seq)
  })
}))

# make it prettier
results_df <- as.data.frame(results) 
results_df <- results_df[ , order(colnames(results_df))]

# 5. Add to original data and export

data_final <- cbind(data, results_df)
# exporting to the project's workspace
write.csv(data_final, "output.csv", row.names = FALSE)
      