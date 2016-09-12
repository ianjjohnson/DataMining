exp.data <- read.csv('~/Dropbox/Fall 2016/7331 - Data Mining/Projects/Project1/data/education_expenditure_supplementary_data.csv')
exp.data <- exp.data[76:111,]
exp.data <- exp.data[exp.data$direct_expenditure_type == "Public",]
exp.data$mean <- rowMeans(exp.data[4:9], na.rm=TRUE)
exp.data <- exp.data[c(1, 10)]

storage.mode(exp.data$X1995)
!is.na(exp.data[4:9])

na.omit(exp.data[4:9])

exp.data[table(is.na(exp.data[4:9]))]


lapply(lapply(exp.data, is.na), table)

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

aggregate(cbind(alumni, award, hici, ns, pub, pcp) ~ year, sha.data, FUN = length)
aggregate(cbind(teaching,international, research, citations, income, total_score, num_students, student_staff_ratio) ~ year, the.data, FUN = length)
