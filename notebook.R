##### HW1  STATISTICAL LEARNING######
##### Author: Barba Paolo, Candi Matteo , Costantini Silvia , Maria Vittoria Vestini

rm(list = ls())
test_set_vero <- read.csv("test.csv")
train_set <- read.csv("train.csv")

train_set$x_2 <- (train_set$x )^ 2
train_set$x_3 <- (train_set$x) ^ 3
train_set$x_4 <- (train_set$x )^ 4
train_set$x_5 <- (train_set$x) ^ 5
train_set$x_6 <- (train_set$x )^ 6
train_set$x_7 <- (train_set$x )^ 7
train_set$x_8 <- (train_set$x )^ 8
train_set$x_9 <- (train_set$x )^ 9
test_set_vero$x_2 <- (test_set_vero$x )^ 2
test_set_vero$x_3 <- (test_set_vero$x) ^ 3
test_set_vero$x_4 <- (test_set_vero$x )^ 4
test_set_vero$x_5 <- (test_set_vero$x) ^ 5
test_set_vero$x_6 <- (test_set_vero$x )^ 6
test_set_vero$x_7 <- (test_set_vero$x) ^ 7
test_set_vero$x_8 <- (test_set_vero$x) ^ 8
test_set_vero$x_9 <- (test_set_vero$x) ^ 9



plot(train_set$x,train_set$y) 
length(train_set$x)
model <- lm( y ~  . , data = train_set)


pred <-predict(model, test_set_vero )
points(test_set_vero$x , pred , col = 'red')



dataset <- data.frame(id = test_set_vero$id , target = pred)

write.csv(dataset, "predictions.csv", row.names=FALSE)



