att.data <- read.csv('~/Dropbox/Fall 2016/7331 - Data Mining/Projects/Project1/data/educational_attainment_supplementary_data.csv')


table(is.na(cwu.data))
lapply(lapply(cwu.data, is.na), table)

rm(list = ls())

format.rank <- function(x) {
  
  if(grepl("=", x)){
    x <- substring(x, 2)
  }
  
  if(grepl("-", x)){
    x <- substring(x, 1, 3)
  }
  
  return(x)
  
}