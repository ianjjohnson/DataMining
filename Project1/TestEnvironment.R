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

aggregate(cbind(alumni_employment, quality_of_faculty, publications, influence, citations, broad_impact, patents, score) ~ year, cwu.data, FUN = length)
aggregate(score ~ year, cwu.data, FUN = length)

summary(exp.data)
summary(exp.data$mean)
summary(att.data$X2005)

my_sample <- function(x) {
  return(sample(scale(x), 100))
}

the.data.sampled <- as.data.frame(lapply(the.data[c(5, 7, 8, 10)], my_sample))
plot(the.data.sampled)
table(the.data$year)
plot(single.year$teaching)
plot.histogram(single.year$total_score)

single.year <- the.data[the.data$year == 2016,]
hist(single.year$total_score, main = "Histogram of THE Total Scores in 2016", col="blanchedalmond")


citation('vioplot')

?vioplot

single.year <- sha.data[sha.data$year == 2015,]
hist(single.year$pcp, col="blanchedalmond", main = "Histogram of Shanghai Total Scores in 2015", xlab = "Total Score")
single.year.the <- the.data[the.data$year == 2016,]

schools <- c(single.year[single.year$pcp > 40,]$university_name, single.year.the[single.year.the$total_score > 60,]$university_name)

a <- as.character(single.year[single.year$pcp > 39,]$university_name)
b <- (single.year.the[single.year.the$total_score > 87,]$university_name)

length(c(a, b))
length(unique(c(a,b)))
c(a,b)

length(schools)
length(unique(schools))

single.year.cwu <- cwu.data[cwu.data$year == max(cwu.data$year),]
single.year.the <- the.data[the.data$year == max(the.data$year),]
single.year.sha <- sha.data[sha.data$year == max(sha.data$year),]

a <- as.character(single.year.sha[single.year.sha$pcp > 30,]$university_name)
b <- (single.year.the[single.year.the$total_score > 69,]$university_name)
c <- as.character(single.year.cwu[single.year.the$total_score > 79,]$institution)

length(c(a, b, c))
table(table(c(a, b, c)))


exp.ord <- exp.data[!is.na(exp.data$mean),]
hist(exp.ord$mean, prob=TRUE, col="blanchedalmond",main="Distribution of Expenditure Scores by Country", xlab="Score")
lines(density(exp.ord$mean), lwd=2, col="green")
lines(density(exp.ord$mean, adjust=2), lty="dotted", col="blue", lwd=2)

the.smu <- the.data[the.data$university_name == "Southern Methodist University",]
sha.smu <- sha.data[sha.data$university_name == "Southern Methodist University",]

mean(c(the.smu$world_rank, sha.smu$world_rank))




the.data.scaled <- the.data

the.data.scaled$num_students <- scale(the.data$num_students)
mean(the.data.scaled[the.data.scaled$university_name == "Southern Methodist University",]$num_students)

the.data.scaled$student_staff_ratio <- scale(the.data$student_staff_ratio)
mean(the.data.scaled[the.data.scaled$university_name == "Southern Methodist University",]$student_staff_ratio)

the.data.scaled$teaching <- scale(the.data$teaching)
mean(the.data.scaled[the.data.scaled$university_name == "Southern Methodist University",]$teaching)

the.data.scaled$citations <- scale(the.data$citations)
mean(the.data.scaled[the.data.scaled$university_name == "Southern Methodist University",]$citations)

sha.data.scaled <- sha.data

sha.data.scaled$pub <- scale(sha.data$pub)
mean(sha.data.scaled[sha.data.scaled$university_name == "Southern Methodist University",]$pub)

sha.data.scaled$alumni <- scale(sha.data$alumni)
mean(sha.data.scaled[sha.data.scaled$university_name == "Southern Methodist University",]$alumni)



single.year.the <- the.data[the.data$year == max(the.data$year),]
single.year.the[c("num_students", "student_staff_ratio", "teaching", "citations")] <- lapply(single.year.the[c("num_students", "student_staff_ratio", "teaching", "citations")], scale)
single.year.the <- single.year.the[1:20,]

single.year.sha <- sha.data[sha.data$year == max(sha.data$year),]
single.year.sha$alumni <- scale(single.year.sha$alumni)
single.year.sha <- single.year.sha[1:20,]

mean(single.year.the$num_students)
mean(single.year.the$student_staff_ratio)
mean(single.year.the$teaching)
mean(single.year.the$citations)
mean(single.year.sha$alumni)

single.year.cwu <- cwu.data[cwu.data$year == max(cwu.data$year),]

single.year.sha <- sha.data[sha.data$year == max(sha.data$year),]

single.year.the <- the.data[the.data$year == max(the.data$year),]
plot(single.year.the[c("teaching", "international", "research", "citations")])

sha.pca.ori <- sha.data[4:9]
sha.pca <- prcomp(sha.pca.ori,
                  center = TRUE,
                  scale. = TRUE) 
plot(sha.pca, type="l", main = "Variance Explained by Principal Components of Shanghai Scores")
title(xlab="Principle Component")

print(sha.pca)

sha.data['pred'] <- (sha.data$alumni * -0.3974386) + (sha.data$award  * -0.4086869) + (sha.data$hici   * -0.4232831) + (sha.data$ns     * -0.4446542) + (sha.data$pub    * -0.3549227) + (sha.data$pcp    * -0.4119315)
sha.data.single <- sha.data[sha.data$year == max(sha.data$year),]
sha.data.single <- sha.data.single[sha.data.single$pred > -200,]
pred <- sha.data.single$pred + 250
world_rank <- sha.data.single$world_rank
world_rank <- order(world_rank)
world_rank[world_rank %% 50 == 0 & world_rank > 199] <- world_rank[world_rank %% 50 == 0 & world_rank > 199] + 1:50
plot(pred, world_rank)

inv <- function(x){
  return(1.0 / x)
}
single.year.cwu <- cwu.data[cwu.data$year == max(cwu.data$year),]
#single.year.cwu[c("quality_of_education", "alumni_employment", "quality_of_faculty", "patents")] <- lapply(single.year.cwu[c("quality_of_education", "alumni_employment", "quality_of_faculty", "patents")], inv)
#single.year.cwu <- single.year.cwu[(single.year.cwu$quality_of_faculty < 0.5 & single.year.cwu$alumni_employment < 0.5 & single.year.cwu$quality_of_education < 0.5 & single.year.cwu$patents < 0.5),]
plot(single.year.cwu[c("quality_of_education", "alumni_employment", "quality_of_faculty", "patents")], log='xy')


corrplot(cor(cwu.data[4:10]), method='ellipse')


cwu.pca <- prcomp(cwu.data[4:10],
                  center = TRUE,
                  scale. = TRUE) 
plot(cwu.pca, type="l", main = "Variance Explained by Principal Components of Shanghai Scores")
