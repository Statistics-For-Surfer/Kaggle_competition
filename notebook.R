rm(list = ls())

# Set reproducibility -----------------------------------------------------
seed <- 1234
set.seed(seed) 

# Libraries and Data ---------------------------------------------------------------
library(glmnet)
library(NMOF)
library(parallel)
library(snow)

test_set_vero <- read.csv("test.csv")
train_set <- read.csv("train.csv")

quant <- range(quantile(train_set$y, c(0.25, 0.75)))
Lower <- quant[1] - 1.5*(diff(quant))
Upper <- quant[2] + 1.5*(diff(quant))

train_set <- train_set[(train_set$y > Lower)& (train_set$y < Upper),]

# Functions ---------------------------------------------------------------

# Function used to compute the feature matrix
power_functions <- function(d, q, knots, x){
  X <- matrix(NA, length(x), d+q+1)
  for( i in 1:length(x)){
    
    for(j in 1:(d+q+1)){
      
      if ( j <= d+1){
        X[i,j] <- x[i]^(j-1)
      }
      
      else
        if((x[i] - knots[j-(d+1)])^d > 0){
          X[i,j] <- (x[i] - knots[j-(d+1)])^d 
        }
      else 
        X[i,j] <- 0
    }
  }
  return(X)
}

# Function used for the cross validation
cross_val_func <- function(x){
  d <- x[1]
  q <- x[2]
  k <- x[3]
  a <- x[4]
  l <- x[5] 
  #p <- x[6]
  
  # size of the fold
  l_folds <- nrow(train_set) / k 
  score <- rep(NA, k)
  for ( i in 1:k){
    idx <- ((i-1)*l_folds+1): (i*l_folds)
    cv_test <- train_set[idx,]
    cv_train <- train_set[-idx,]
    
    
    knots <- seq(1/q, 1 , length.out=q)
    
    M_cv_train <- power_functions(d = d, q = q, knots = knots, x = cv_train$x)
    # M_cv_train <- data.frame(M_cv_train , target = cv_train$y)
    
    M_cv_test <-  power_functions(d = d, q = q, knots = knots, x = cv_test$x)

    cv_model <- glmnet(M_cv_train, cv_train$y,family = "gaussian", alpha=a, lambda=l)
    
    cv_predictions <- predict(cv_model, M_cv_test)
    
    score[i] <- sqrt(mean((cv_test$y-cv_predictions)^2))
    
  }
  return(mean(score))
}

# Main function for the nested CV
nested_crossval <- function(x){
  d <- x[1]
  q <- x[2]
  K <- x[3]
  a <- x[4]
  l <- x[5] 
  #p <- x[6]
  R <- 10
  l_folds <<- nrow(train_set) / K
  
  #es <- c()
  a_list <- rep(NA, R*K)
  b_list <- rep(NA, R*K)
  for(r in (1:R)){
    idx <- sample((1:nrow(train_set)),nrow(train_set))
    
    for(k in (1:K)){  
      cv_test <- train_set[idx[((k-1)*l_folds+1): (k*l_folds)],]
      cv_train <- train_set[-idx[((k-1)*l_folds+1): (k*l_folds)],]
      
      # inner cross
      e_in <- inner_crossval(x, cv_train)
      
      # Outer cross
      if(q == 0) knots <- c()
      else knots <- seq(1/q, 0.8, length.out=q)
     
      M_cv_train <- power_functions(d = d, q = q, knots = knots, x = cv_train$x)
      M_cv_test <-  power_functions(d = d, q = q, knots = knots, x = cv_test$x)
      
      cv_model <- glmnet(M_cv_train, cv_train$y,family = "gaussian", alpha=a, lambda=l)
      cv_predictions <- predict(cv_model, M_cv_test)
      
      e_out <- (cv_test$y-cv_predictions)^2
      
      a_list[(r-1)*K+k] <- (mean(e_in)-mean(e_out))^2 
      b_list[(r-1)*K+k] <- (sd(e_out)^2)/l_folds
      #es <- c(es, e_in)   
    }
  }
  
  mse <- mean(a_list)-mean(b_list)
  #err <- mean(es)
  return(mse)
}

# Secondary function for nested CV
inner_crossval <- function(x, train_set){
  d <- x[1]
  q <- x[2]
  K <- x[3]
  a <- x[4]
  l <- x[5] 
  #p <- x[6]
  
  e_in <- c()
  
  for(k in (1:(K-1))){
    idx <- ((k-1)*l_folds+1): (k*l_folds)
    cv_test <- train_set[idx,]
    cv_train <- train_set[-idx,]
    
    if(q == 0) knots <- c()
    else knots <- seq(1/q, 0.8, length.out=q)
    
    M_cv_train <- power_functions(d = d, q = q, knots = knots, x = cv_train$x)
    M_cv_test <-  power_functions(d = d, q = q, knots = knots, x = cv_test$x)
    
    cv_model <- glmnet(M_cv_train, cv_train$y,family = "gaussian", alpha=a, lambda=l)
    cv_predictions <- predict(cv_model, M_cv_test)
    
    e_temp <- (cv_test$y-cv_predictions)^2
    e_in <- c(e_in, e_temp)
  }
  
  return(e_in)
}


# Parameters -------------------------------------------------------------
k <- c(4)
d_grid <- c(1, 3) 
q_grid <- seq(8, 12, 1)
#positions <- c(0.3, 0.4, 0.5, 0.7)
lambdas <- 10^seq(-2.5, -1.5, .25)
alphas <- seq(0, 1)

# Set the parameter for the CV
parameters <- list(d_grid, q_grid, k, alphas, lambdas)

# CV vanilla --------------------------------------------------------------
# Select the best combination of parameters
cl = makeCluster(detectCores())
clusterExport(cl, c('train_set', 'power_functions', 'glmnet'))
res <- gridSearch(cross_val_func, levels=parameters, method = 'snow', cl=cl)
stopCluster(cl)
best_params <- res$minlevels
names(best_params) <- c('d', 'q', 'k', 'alpha', 'lambda')


# CV nested --------------------------------------------------------------
cl = makeCluster(detectCores())
clusterExport(cl, c('train_set', 'inner_crossval', 'power_functions', 'glmnet'))
res <- gridSearch(nested_crossval, levels=parameters, method = 'snow', cl=cl)
stopCluster(cl)
best_params <- res$minlevels
names(best_params) <- c('d', 'q', 'k', 'alpha', 'lambda')


# Prediction --------------------------------------------------------------

# Using the best parameters
d_best <- best_params[1]
q_best <- best_params[2]
k_best <- best_params[3]
a_best <- best_params[4]
l_best <- best_params[5]
#p_best <- best_params[6]

# Compute the predictions
knots <- seq(1/q_best, 0.8, length.out=q_best)
M_train <- power_functions(d = d_best, q = q_best, knots = knots, x = train_set$x)
M_test <- power_functions( d = d_best, q = q_best, knots = knots, x = test_set_vero$x)
knots_test <- power_functions( d = d_best, q = q_best, knots = knots, x = knots)
final_model <- glmnet(M_train, train_set$y, family ="gaussian", alpha=a_best, lambda=l_best)
predictions <- predict(final_model,M_test)



# Plot --------------------------------------------------------------------

# Simple plot
plot(train_set$x,train_set$y,cex = .5, pch = 16, col = "Green")
points(test_set_vero$x,predictions, col = "blue", cex = .5, pch=16)
grid()
points(knots, predict(final_model, knots_test), col='red', pch=3, cex=1, lwd=4)


# More beautiful plot ;)

# install.packages('tidyverse')
# install.packages('manipulate')
# 
# library(tidyverse)
# library(manipulate)
# 
# colors <- c("Real Data" = "green", "Predicted" = "blue", "Knots" = "red")
# green_point_size <- 1.3
# blue_point_size <- 1.3
# red_cross_size <- 2
# 
# plot_fun <- function(x_min, x_max){
#   ggplot() +
#     geom_point(aes(x = train_set$x, y = train_set$y, color = 'Real Data'), size = green_point_size, shape=16) +
#     geom_point(aes(x = test_set_vero$x, y = predictions, color = 'Predicted'), size = blue_point_size) +
#     geom_point(aes(x = knots, y = predict(final_model, knots_test), color = 'Knots'), shape = 4, stroke = 1.7, size = red_cross_size) +
#     theme_minimal() +
#     labs(x = "x", y = "y", color = "Legend", title = 'Prediction on WMAP data', shape = "", color="") +
#     scale_color_manual(values = colors) +
#     theme(legend.title = element_text(size=12), legend.text = element_text(size=11), plot.title = element_text(hjust = 0.5)) +
#     coord_cartesian(xlim = c(x_min, x_max))+
#     guides(color = guide_legend(override.aes=list(shape = c(4, 16, 16), size = 2)))}
# 
# manipulate(plot_fun(x.min, x.max), x.min = slider(0,.9, 0, step = .1), x.max = slider(.1, 1, 1, step = .1))
# 



# Output ------------------------------------------------------------------

best_params
deviance(final_model)


dataset <- data.frame(id = test_set_vero$id, target = predictions[2])

write.csv(dataset, "predictions.csv", row.names=FALSE)
