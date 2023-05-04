rm(list = ls())

seed <- 1234
set.seed(seed) 


# Libraries and Data ---------------------------------------------------------------
library(glmnet)
library(NMOF)

test_set_vero <- read.csv("test.csv")
train_set <- read.csv("train.csv")


# Parameters --------------------------------------------------------------
k <- c(5, 9)
d_grid <- c(1,3) 
q_grid <- c(3,4,5,6,7,8,9,10)
positions <- c(0.3,0.4,0.5,0.7, .9)
lambdas <- 10^seq(-3, -1, .5)
alphas <- c(0, 1)
l_folds <- nrow(train_set) / k  # size of the fold



# Functions ---------------------------------------------------------------
power_functions <- function(d , q , knots , x){
  X <- matrix(NA , length(x) , d +q + 1)
  for( i in 1:length(x)){
    
    for(j in 1:(d +q + 1)){
      
      if ( j <= d + 1){
        X[i,j] <- x[i]^(j-1)
      }
      
      else
        if(  (x[i] - knots[j - (d + 1)])^d > 0){
          X[i,j] <- (x[i] - knots[j - (d + 1)])^d 
        }
      else 
        X[i,j] <- 0
    }
  }
  return(X)
}

cross_val_func <- function(x){
  d <- x[1]
  q <- x[2]
  p <- x[3]
  k <- x[4]
  a <- x[5]
  l <- x[6]
  score <- rep(NA, k)
  for ( i in 1:k){
    idx <- ((i-1)*l_folds + 1): (i*l_folds)
    cv_test <- train_set[idx,]
    cv_train <- train_set[-idx,]
    
    
    knots <- seq(0.01, p , length.out = q)
    
    M_cv_train <- power_functions(d = d, q = q , knots = knots, x = cv_train$x)
    # M_cv_train <- data.frame(M_cv_train , target = cv_train$y)
    
    M_cv_test <-  power_functions(d = d , q = q , knots = knots, x = cv_test$x)

    cv_model <- glmnet(M_cv_train, cv_train$y,family = "gaussian", alpha=a, lambda=l)
    
    cv_predictions <- predict(cv_model , M_cv_test)
    
    score[i] <- sqrt(mean(( cv_test$y- cv_predictions)^2))
    
  }
  return(mean(score))
}



# Grid Search -------------------------------------------------------------
parameters <- list(d_grid, q_grid , positions, k, alphas, lambdas)

res <- gridSearch(cross_val_func, levels= parameters)
best_params <- res$minlevels



# Prediction --------------------------------------------------------------
d_best <- best_params[1]
q_best <- best_params[2]
p_best <- best_params[3]
k_best <- best_params[4]
a_best <- best_params[5]
l_best <- best_params[6]

knots <- seq(0.01, p_best , length.out = q_best)
M_train <- power_functions(d = d_best , q = q_best , knots = knots, x = train_set$x )
M_test <- power_functions( d = d_best , q = q_best , knots = knots, x = test_set_vero$x )
knots_test <- power_functions( d = d_best , q = q_best , knots = knots, x = knots )
final_model <- glmnet(M_train, train_set$y, family ="gaussian", alpha=a_best, lambda=l_best)
predictions <- predict(final_model,M_test)


# Plot --------------------------------------------------------------------
plot(train_set$x,train_set$y,cex = .5, pch = 16 , col = "Green")
points(test_set_vero$x,predictions, col = "blue" , cex = .5, pch=16)
grid()
# abline(v=knots, lwd=.3, lty=2)
points(knots, predict(final_model, knots_test), col='red', pch=3, cex=1, lwd=4)



deviance(final_model)


dataset <- data.frame(id = test_set_vero$id , target = predictions )

best_params

write.csv(dataset, "predictions.csv", row.names=FALSE)
