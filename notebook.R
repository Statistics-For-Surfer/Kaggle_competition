
rm(list = ls())



library(glmnet)
library(NMOF)

test_set_vero <- read.csv("test.csv")
train_set <- read.csv("train.csv")


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

seed <- 1234
set.seed(seed)  # should return (2,7,3) positions of the 3-D array


k <- 5
d_grid <- c(1,3) 
q_grid <- c(3,4,5,6,7,8,9,10)
positions <- c(0.3,0.4,0.5,0.7,1)
l_folds <- nrow(train_set) / k  # size of the fold


cross_val_func <- function(x){
  d <- x[1]
  q <- x[2]
  p <- x[3]
  k <- x[4]
  score <- rep(NA, k)
  for ( i in 1:k){
    idx <- ((i-1)*l_folds + 1): (i*l_folds)
    cv_test <- train_set[idx,]
    cv_train <- train_set[-idx,]
    
    
    knots <- seq(0.1, p , length.out = q)
    
    M_cv_train <- power_functions(d = d, q = q , knots = knots, x = cv_train$x)
    M_cv_train <- data.frame(M_cv_train , target = cv_train$y)
    
    M_cv_test <-  data.frame(power_functions(d = d , q = q , knots = knots, x = cv_test$x))
    
    cv_model <- lm(target ~ . , data = M_cv_train )
    
    cv_predictions <- predict(cv_model , M_cv_test)
    
    score[i] <- sqrt(mean(( cv_test$y- cv_predictions)^2))
    
  }
  return(mean(score))
}

parameters <- list(d_grid, q_grid , positions, k)

res <- gridSearch(cross_val_func, levels= parameters)

best_params <- res$minlevels

d_best <- best_params[1]
q_best <- best_params[2]
p_best <- best_params[3]


knots <- seq(0.1, p_best , length.out = q_best)
M <- power_functions(d = d_best , q = q_best , knots = knots, x = train_set$x  )

M_train <- data.frame(target =train_set$y , M )

M_test <- power_functions( d = d_best , q = q_best , knots = knots, x = test_set_vero$x )
M_test <- data.frame(M_test)


final_model <- lm(target ~ . , data = M_train)


predictions <- predict(final_model,M_test)
plot(train_set$x,M_train$target,cex = .4, pch = 16 , col = "Green")
points(test_set_vero$x,predictions, col = "blue" , cex = .4)
deviance(final_model)


dataset <- data.frame(id = test_set_vero$id , target = predictions )



write.csv(dataset, "predictions.csv", row.names=FALSE)
