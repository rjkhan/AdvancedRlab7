#' @title Reference class for ridge regression
#' @description Create a ridgereg class containing all necessary methods for fitting a ridge regression model
#' @field formula A formula for the ridge regression
#' @field data A data set for the ridge regression
#' @field lambda The ridge constant, a numeric scalar
#' @importFrom methods new
#' @export ridgereg
#' @exportClass ridgereg


#kom ihåg att använda scale på x variablerna i testen med lm.ridge

ridgereg<-setRefClass("ridgereg",
                    fields=c("formula", 
                             "data", "lambda", "name"),
                    methods= list(
                      
                      initialize = function(formula, data, lambda){
                        
                        if(class(formula)!="formula") stop("formula is not of class 'formula'")
                        if(!is.data.frame(data)) stop("data is not a data frame")
                        if(!all(sapply(data[,colnames(data) %in% all.vars(formula)],is.numeric))) stop("data not numeric")
                        if(!all(all.vars(formula)%in%colnames(data))) stop("formula and data doesn't match")
                        if(!is.numeric(lambda) || length(lambda)!=1) stop("incrorrect lambda value")
                        
                        name <<- deparse(substitute(data))
                        data<<-data
                        formula<<-formula
                        lambda<<-lambda
                      },
                      
                      statistics = function(){
                        "Fits a ridge regression model"
                        
                        results <- list()
                        y<-data[,colnames(data)==all.vars(formula)[1]]
                        X<-model.matrix(object=formula, data=data)
                        X[,2:ncol(X)] <- scale(X[,-1]) #Normalizing
                        results$coef <- solve(t(X) %*% X + diag(lambda, nrow = ncol(X))) %*% (t(X) %*% y)
                        results$fitted <- X %*% results$coef
                        return(results)
                      },
                      
                      print = function(){
                        "Prints out the formula and the estimated coefficients"

                        cat("Call:", "\n")
                        cat("ridgereg(formula = ", deparse(formula), ", data = ", name, ", lambda = ", lambda, ")", "\n", "\n", sep="")
                        cat("Coefficients:", "\n")
                        structure(c(statistics()[[1]]), names=rownames(statistics()[[1]]))
                      },
                      
                      predict = function(newdata=NULL){ #if newdata is used, it should be a data frame
                        "Prints out the predicted values or if newdata is used, prints out predicted values for the new data set"
                        
                        if(is.null(newdata)){
                          result <- structure(c(statistics()[[2]]), names=(1:length(statistics()[[2]])))
                        } else{
                          if(!is.data.frame(newdata)) stop("newdata is not a data frame")
                          if(!all(all.vars(formula)%in%colnames(newdata))) stop("newdata doesn't match formula")
                          if(!all(sapply(newdata[,colnames(newdata) %in% all.vars(formula)],is.numeric))) stop("newdata not numeric")
                          
                          X<-model.matrix(object=formula, data=newdata)
                          X[,2:ncol(X)] <- scale(X[,-1])
                          result <- (X %*% statistics()[[1]])[,1]
                        }
                        return(result)
                      },
                      
                      coef = function(){
                        "Prints out the estimated coefficients"
                        
                        structure(c(statistics()[[1]]), names=rownames(statistics()[[1]]))
                      }
                    ))





#### Methods ####

# lambda <- 1
# data <- iris[,1:4]
# formula <- Sepal.Length ~ Sepal.Width + Petal.Length
# 
# b <- ridgereg(data=data, formula=formula, lambda=lambda)
# 
# b$print()
# b$predict()
# 
# newdata <- iris[45:50,1:4]
# b$predict(newdata=newdata)

