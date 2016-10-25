r.both <- r[!is.na(r$improved),]
r.both$total_score_Y2005 <- NULL
r.both$total_score_Y2015 <- NULL
r.both <- na.omit(r.both)
library(class)

classes <- knn(r.both[1:250,][1:12], r.both[251:377,][1:12], as.factor(r.both[1:250,]$improved), k=9)
confusionMatrix(classes, as.factor(r.both[251:377,]$improved))


# 1, 0.693, 0.3717
# 3, 0.708, 0.4089
# 5, 0.685, 0.3474
# 7, 0.669, 0.2907
# 9, 0.677, 0.3224


accuracy <- function(truth, prediction) {
  tbl <- table(truth, prediction)
  sum(diag(tbl))/sum(tbl)
}

svp <- ksvm(improved ~ ., data=r.both[1:250,], type="C-svc", kernel="rbfdot", C=50, scaled=c())
classes <- predict(svp, r.both[251:377,])
confusionMatrix(classes, r.both[251:377,]$improved)


rf <- randomForest(improved ~ ., data=r.both, importance=TRUE,proximity=TRUE)
x <- importance(rf)

svp <- ksvm(improved ~ ., data=r.both[1:250,], type="C-svc", kernel="polydot", C=5, scaled=c())
classes <- predict(svp, r.both[251:377,])
confusionMatrix(classes, r.both[251:377,]$improved)

table(classes)
table(r.both[251:377,]$improved)
