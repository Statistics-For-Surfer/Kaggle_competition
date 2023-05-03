##### HW1  STATISTICAL LEARNING######
##### Author: Barba Paolo, Candi Matteo , Costantini Silvia , Maria Vittoria Vestini

rm(list = ls())
test_set_vero <- read.csv("test.csv")
train_set <- read.csv("train.csv")


d <- 3
q <- 3
knots <- seq(0.1,0.9, length.out = 3)
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
M <- power_functions(d = d , q = q , knots = knots, x = xs)

plot(M[,7])


plot(train_set$x, train_set$y)






write.csv(dataset, "predictions.csv", row.names=FALSE)



