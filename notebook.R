##### HW1  STATISTICAL LEARNING######
##### Author: Barba Paolo, Candi Matteo , Costantini Silvia , Maria Vittoria Vestini

rm(list = ls())
# Install packets
library(glmnet)
test_set_vero <- read.csv("test.csv")
train_set <- read.csv("train.csv")


d <- 3
q <- 3
qs <- quantile(train_set$x , probs =  c(.33,.66,1) )  # Per ora
xs <- seq(0,1,by = 0.01)
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
M <- power_functions(d = d , q = q , knots = qs, x = train_set$x  )
M_train <- data.frame(target =train_set$y , M )

M_test <- power_functions(d = d , q = q , knots = qs, x = test_set_vero$x )
M_test <- data.frame(M_test)
str(M_test)
model <- lm(target ~ . , data = M_train)


predictions <- predict(model,M_test)
plot(train_set$x,M_train$target)
points(test_set_vero$x,predictions, col = "red")





# Divisione dataset train e test

train_set <- train_set[151:length(train_set$x),]
test_set <-  train_set[1:150,]



# Cross-Validation ------------------------------------------------------------------------------------------------------------------------------


### FOLD
B <- 500  # simulation size / the larger the better

# Init
cvscore    <- rep(NA, B)   # CV-scores 
trainscore <- rep(NA, B)   # Apparent/Training error
truescore  <- rep(NA, B)   # Exact MSE



###

k <- 5
d_grid <- c(1,3) 
q_grid <- c(3,4,5,6,7,8,9,10)
positions <- c(0.3,0.4,0.5,0.7,1)
l_folds <- nrow(train_set) / k  # size of the fold

rmse_score <- array(rep(NA, length(d_grid)*length(q_grid)*length(positions)), dim=c(length(d_grid), length(q_grid), length(positions)))


for(d in 1:length(d_grid)){
  for (q in 1:length(q_grid)){
    for (p in 1:length(positions)){
      score <- rep(NA, k)
      for ( i in 1:k){
        idx <- ((i-1)*l_folds + 1): (i*l_folds)
        cv_test <- train_set[idx,]
        cv_train <- train_set[-idx,]
        
        
        knots <- seq(0.1, positions[p] , length.out = q_grid[q])
        
        M_cv_train <- power_functions(d = d_grid[d], q = q_grid[q] , knots = knots, x = cv_train$x)
        M_cv_train <- data.frame(M_cv_train , target = cv_train$y)
        
        M_cv_test <-  data.frame(power_functions(d = d_grid[d] , q = q_grid[q] , knots = knots, x = cv_test$x))
        
        cv_model <- lm(target ~ . , data = M_cv_train )
        
        cv_predictions <- predict(cv_model , M_cv_test)
        
        score[i] <- sqrt(mean(( cv_test$y- cv_predictions)^2))
      
      }
      rmse_score[d,q,p] <- mean(score)
    }
}}



best_conf_ind <- which(rmse_score==min(rmse_score), arr.ind=T)


d_best <- d_grid[best_conf_ind[1]]
q_best <- q_grid[best_conf_ind[2]]
p_best <- positions[best_conf_ind[3]]


### TODO , fare cv con enumerate neatStats
#a = c(3,4,5)
#for (el in enum(a)){
#  print(el)
#}
knots <- seq(0.1, positions[p_best] , length.out = q_best)
M <- power_functions(d = d_best , q = q_best , knots = knots, x = train_set$x  )

M_train <- data.frame(target =train_set$y , M )

M_test <- power_functions( d = d_best , q = q_best , knots = knots, x = test_set_vero$x )
M_test <- data.frame(M_test)


final_model <- lm(target ~ . , data = M_train)


predictions <- predict(final_model,M_test)
plot(train_set$x,M_train$target)
points(test_set_vero$x,predictions, col = "red")



dataset <- data.frame(id = test_set_vero$id , target = predictions )



write.csv(dataset, "predictions.csv", row.names=FALSE)

