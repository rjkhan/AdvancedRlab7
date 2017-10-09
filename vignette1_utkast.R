library(caret)
library(mlbench)

data(BostonHousing)

#1 Test and training set

train_index <- createDataPartition(y = BostonHousing$medv,
                                   p = 0.75,
                                   list = FALSE)
training <- BostonHousing[train_index,]
testing <- BostonHousing[-train_index,]

#2 linreg caret

lm1 <- caret::train(medv ~ ., data=training, method = 'leapForward')
summary(lm1)




coef(lm1$finalModel$param)

lmFit <- train(medv ~ .,
               data = training,
               method = "leapForward")

summary(lmFit)
  lmFit$finalModel

  predict(lmFit$finalModel, type="coefficients")

#3 evaluate fit training data set

