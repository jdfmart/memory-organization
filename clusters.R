# This code allows you to load a dataset containing participants' recalls, 
# previously coded according to categories, and easily calculate four different 
# indexes of clustering in recall for all participants simultaneously 
# (based on Otani and Senkova, 2012); namely:
#
# - ARC: Adjusted Ratio of Recall (Roenker et al., 1971)
# - RR: Ratio of Repetition (Cohen et al., 1954)
# - MRR: Modified Ratio of Repetition (Wallace & Underwood, 1966)
# - DS: Deviation Score (Bousfield & Bousfield, 1966)

# The function app expects the database to be in a very specific format; namely:

# - Comma separated values (.csv file)
# - Wide format (one line per participant; one recall per column)
# - Previously coded recalls (numbers in place of categories)
# - First column is your participant key


# name your file "input.csv" or change here accordingly
# place file in project's workspace of change path accordingly
clusters <- read.csv("input.csv") 

# the function:
cluster <- function(data, all = FALSE, params = FALSE) {
  #n
  parameters <- data.frame(n = rowSums(!is.na(data)))
  #c
  parameters$c <- apply(data, 1, function(x) length(unique(x[!is.na(x)])))
  #r
  parameters$r <- apply(data, 1, function(x) with(rle(x), sum(lengths[lengths > 1]) - length(lengths[lengths > 1])))
  #rs
  n_categories <- sort(
    unique(data[!is.na(data)]))
  #E(r)
  parameters$er <- 
    rowSums(
      apply(
        data.frame(sapply(n_categories, function(x) rowSums(data == x, na.rm = TRUE))), 
        2, function(x) x^2))/parameters$n - 1 
  
  #ARC
  arc <- (parameters$r - parameters$er)/((parameters$n - parameters$c) - parameters$er)
  
  #RR
  rr <- parameters$r/(parameters$n-1)
  
  #MRR
  mrr <- parameters$r/(parameters$n - parameters$c)
  
  #DS
  ds <- (parameters$r - parameters$er)
  
  indexes <- data.frame(arc = arc)
  
  if(all == TRUE){
    indexes$rr <- rr
    indexes$mrr <- mrr
    indexes$ds <- ds
  }
  
  if(params == TRUE){
    indexes <- cbind(indexes,parameters)
  }
  
  #Print indexes 
  print(indexes)
}

# annexing the resulting indexes to your data
# use 'all == TRUE' for all four indexes (ARC only, otherwise)
# use 'params == TRUE' to output the parameters used in calculating the indexes
clusters <- cbind(clusters, cluster(clusters, all = TRUE))

# exporting to the project's workspace
write.csv(clusters, "output.csv")
