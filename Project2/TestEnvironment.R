library(caret)
library(rpart)
library(rpart.plot)
shanghai <- read.csv("~/Dropbox/Fall 2016/7331 - Data Mining/Projects/Project1/data/shanghaiData.csv")
rnk <- as.character(shanghai$world_rank)
rnk <- sub(pattern = "-.*", "", rnk)
rnk <- as.numeric(rnk)
shanghai$world_rank <- rnk

rnk <- as.character(shanghai$national_rank)
rnk <- sub(pattern = "-.*", "", rnk)
rnk <- as.numeric(rnk)

l <- split(shanghai, shanghai$university_name)
years <- sapply(l, nrow)

r <- lapply(l, FUN = function(x) {
  d_2005 <- x[x$year==2005,]
  d_2015 <- x[x$year==2015,]
  merge(d_2005, d_2015, by = "university_name",
        all = TRUE, suffix = c("_Y2005", "_Y2015"))
})

r <- do.call(rbind, r)

improved <- r$'world_rank_Y2005' - r$'world_rank_Y2015'
improved <- improved > 0
r$improved <- as.factor(improved)

r.copy <- r

r$'university_name' <- NULL
r$'year_Y2015' <- NULL
r$'year_Y2005' <- NULL
r$'world_rank_Y2015' <- NULL
r$'national_rank_Y2015' <- NULL
r$'world_rank_Y2005' <- NULL
r$'national_rank_Y2005' <- NULL







#Classification by Association
library(arulesCBA)
disc <- function(x){
  return(discretize(x, categories=2, method="frequency"))
}
?discretize
r.disc <- as.data.frame(sapply(r[1:14], disc))
r.disc$improved <- r$improved
r.disc <- r.disc[!is.na(r.disc$improved),]
classifier <- CBA(r.disc, "improved", verbose=TRUE)
classes <- predict(classifier, r.disc)
confusionMatrix(classes, r.disc$improved)
print(classifier)
inspect(rules(classifier))


#Decision Tree
library(rpart)
library(rpart.plot)
the.data.disc <- the.data[c(1,4:9)]
the.data.disc$world_rank <- disc(the.data.disc$world_rank)
tree1 <- rpart(world_rank ~ ., data=the.data.disc)
rpart.plot(tree1)
classes <- predict(tree1, the.data.disc, type='class')
confusionMatrix(classes, the.data.disc$world_rank)


#K-Nearest Neighbors
library(class)
r.both <- r[!is.na(r$improved),]
r.both$total_score_Y2005 <- NULL
r.both$total_score_Y2015 <- NULL
r.both <- na.omit(r.both)
classes <- knn(r.both[1:12], r.both[1:12], as.factor(r.both$improved), k=5)
confusionMatrix(classes, as.factor(r.both$improved))

#Linear SVM
library(kernlab)
svp <- ksvm(improved ~ ., data=r.both, type="C-svc", kernel="vanilladot", C=1, scaled=c())
classes <- predict(svp, r.both)
confusionMatrix(classes, r.both$improved)

#non-linear
svp <- ksvm(improved ~ ., data=r.both, type="C-svc", kernel="rbfdot", C=1, scaled=c())
classes <- predict(svp, r.both)
confusionMatrix(classes, r.both$improved)

library("e1071")
svm_model <- svm(improved ~ ., data=r.both)
summary(svm_model)
classes <- predict(svm_model, r.both)
confusionMatrix(classes, r.both$improved)
