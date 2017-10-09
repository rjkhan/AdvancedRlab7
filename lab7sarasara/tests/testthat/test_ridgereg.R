context("ridgereg")

data("iris")


test_that("ridgereg stops with errors", {
  expect_error(ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data="Sara", lambda=1))
  expect_error(ridgereg$new(formula = c(Petal.Length, Sepal.Width, Sepal.Length), data=iris, lambda=1))
  expect_error(ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data=iris[,-1], lambda=1))
  expect_error(ridgereg$new(formula = Species~Sepal.Width+Sepal.Length, data=iris, lambda=1))
  expect_error(ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda="lambda"))
})


test_that("correct class", {
  ridge1 <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.5)
  expect_true(class(ridge1)[1] == "ridgereg")
})


test_that("print() works", {
  ridge1 <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.5)
  expect_output(ridge1$print(),
                "ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris, lambda = 0\\.5\\)")})


test_that("predict() works", {
  ridge1 <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.5)
  expect_equal(round(unname(ridge1$predict()[c(1,20,79)]),2), c(1.84, 1.44, 4.23))
  expect_equal(round(unname(ridge1$predict(newdata = iris[1:3,])),2), c(4.59, 4.29, 2.36))
  expect_error(ridge1$predict(newdata=mtcars))
})


test_that("coef() works", {
  ridge1 <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.5)
  
  #normalized covariates in lm.ridge
  newdata <- iris[,colnames(iris) %in% all.vars(Petal.Length~Sepal.Width+Sepal.Length)[-1]]
  newdata <- scale(newdata)
  newdata <- data.frame(Petal.Length=iris[,colnames(iris) %in% all.vars(Petal.Length~Sepal.Width+Sepal.Length)[1]], newdata)
  ridge2 <- MASS::lm.ridge(formula=Petal.Length~Sepal.Width+Sepal.Length, data=newdata, lambda=0.5)
  
  expect_equal(round(ridge1$coef()[-1],1), round(ridge2$coef,1))
})

