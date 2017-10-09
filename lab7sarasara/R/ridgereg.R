library(MASS)

?lm.ridge


### Least squares
lambda <- 1
data <- iris[,1:4]
formula <- Sepal.Length ~ Sepal.Width + Petal.Length

y<-data[,colnames(data)==all.vars(formula)[1]]

X<-model.matrix(object=formula, data=data)

#Normalizing
X[,2:ncol(X)] <- scale(X[,-1])

#coefficients beta_ridge
beta_ridge <- solve(t(X) %*% X + diag(lambda, nrow = ncol(X))) %*% (t(X) %*% y)

#fitted values
fitted_ridge <- X %*% beta_ridge


#test - använd standardiserade variabler
xxx <- scale(data[,2:4])
daat <- data.frame(Sepal.Length=y, xxx)

lm.ridge(formula, daat, lambda=2)




##### CLASS #####
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
                        results$fitted <- X %*% beta_ridge
                        return(results)
                      }
                    ))


