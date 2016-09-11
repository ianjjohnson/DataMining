library(zoo)

format.rank <- function(x) {
  
  if(grepl("=", x)){
    x <- substring(x, 2)
  }
  
  if(grepl("-", x)){
    x <- substring(x, 1, 3)
  }
  
  return(x)
  
}

the.data <- read.csv('~/Dropbox/Fall 2016/7331 - Data Mining/Project1/data/timesData.csv', stringsAsFactors = FALSE)
the.data$female_male_ratio <- as.numeric(substring(the.data$female_male_ratio, 0, 2)) / as.numeric(substring(the.data$female_male_ratio, 6, 8))
the.data$female_male_ratio <- ave(the.data$female_male_ratio, the.data$country, FUN = na.aggregate)
the.data$num_students <- gsub(",", "", the.data$num_students)
the.data$international_students <- as.numeric(gsub("%", "", the.data$international_students)) / 100
nums <- c('teaching', 'international', 'research', 'citations', 'income', 'total_score', 'num_students', 'student_staff_ratio', 'international_students')
the.data[nums] <- lapply(the.data[nums], as.numeric)
the.data[nums] <- ave(the.data[nums], the.data$country, FUN = na.aggregate)
the.data$world_rank <- as.numeric(sapply(the.data$world_rank, format.rank))
the.data$international_students <- as.numeric(substring(the.data$international_students, 0, 2))
head(the.data)
the.data[c(5, 8, 11, 12, 13)] <- ave(the.data[c(5, 8, 11, 12, 13)], the.data$country, FUN = na.aggregate)
the.data$total_score[is.na(the.data$total_score)] <- mean(the.data$total_score[!is.na(the.data$total_score)])
the.data <- na.omit(the.data)
table(is.na(the.data))

