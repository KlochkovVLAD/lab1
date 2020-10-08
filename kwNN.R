distance <- function(u, v){
  return(sqrt(sum((u - v)^2)))
}

sort <- function(set, point){
  n <- dim(set)[1]
  dist <- matrix(NA,n,2)
  for(i in 1:n){
    dist[i,1] <- i
    dist[i,2] <- distance(set[i,1:2],point)
  }
  orderedSet <- set[order(dist[, 2]), ]
  return(orderedSet)
}

kwNN <- function(orderedSet,q){
  n <- dim(orderedSet)[1]
  weights <- matrix(0,n,1)
  for (p in seq(1,n,1)) {
    weights[p] <- (q/100)^p
  }
  
  order_and_weight <- cbind(orderedSet, weights)
  classes <- order_and_weight[1:6, 3:4]
  
  w1 <- sum(classes[classes$Species == "setosa", 2])
  w2 <- sum(classes[classes$Species == "versicolor", 2])
  w3 <- sum(classes[classes$Species == "virginica", 2])
  
  answer <- matrix(c(w1, w2, w3), nrow = 1, ncol = 3, byrow = TRUE, list(c(1), c(1, 2, 3)))
  class <- c("setosa", "versicolor", "virginica")
  
  return(class[which.max(answer)])
}


loo <- function(){
  arrayOfErrors <- matrix(0,99,1)
  for (i in seq(1,150,1)) {
    set <- iris[ ,3:5]
    point <- set[i,1:2]
    set <- set[-i,]
    set <- sort(set,point)
      for (q in seq(1,99,1)) {
        class <- kwNN(set,q)
        if (class != iris[i,5]) {
          arrayOfErrors[q] <- arrayOfErrors[q] + 1
        }
      }
      
  }
  print(arrayOfErrors)
  res <- which.min(arrayOfErrors)
  return(res)
}

colors <- c("setosa" = "red", "versicolor" = "green4", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, ylim = c(0,3))
##Вычисляем оптимальное q, при k=6
q <- loo()/100
for (x in seq(1,7,0.1)){
  for (y in seq(0,3,0.1)){
  point <- c(x,y)
  print(point)
  set <- sort(iris[, 3:5],point)
  ##Определяем класс нашей точки, методом k ближайших взвещенных соседей
  class <- kwNN(set,q)
  print(class)
  points(point[1], point[2], pch = 1, col = colors[class], asp = 1)
  }
}