library(caret)
library(kernlab)
library(ggplot2)

data(iris)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
#dim(training); dim(testing)
#foldsTrain <- createFolds(y=doc$tfidf, k=10, list=TRUE, returnTrain = TRUE)
#foldsTest <- createFolds(y=doc$tfidf, k=10, list=TRUE, returnTrain = FALSE)
modlda <- train(Species ~ ., data=training, method = "lda")
modnb <- train(Species ~ ., data=training, method = "nb")
plda <- predict(modlda,testing)
pnb <- predict(modnb,testing)
table(plda,pnb)
eqPredictions <- (plda==pnb)
qplot(Petal.Width, Sepal.Width,colour = eqPredictions, data=testing)
