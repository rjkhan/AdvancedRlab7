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

lm1 <- caret::train(medv ~ ., data=training, method = 'lm')


summary(lm1)


#3 evaluate fit training data set

?

#4 ridgereg


