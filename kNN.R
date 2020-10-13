distance <- function(u, v){
  return(sqrt(sum((u - v)^2)))
}

sort <- function(set, point){
  n <- dim(iris)[1]
  dist <- matrix(NA,n,2)
  for(i in 1:n){
    dist[i,1] <- i
    dist[i,2] <- distance(set[i,1:2],point)
  }
  orderedSet <- set[order(dist[, 2]), ]
  return(orderedSet)
}

kNN <- function(orderedSet,k){
    classes <- orderedSet[1:k,3]
    cnt <- table(classes)
    class <- names(which.max(cnt))
    return(class)
}
  

loo <- function(){
  n <- dim(iris)[1]
  arrayOfErrors <- matrix(0,n-1,1)
  for(i in seq (1,n,1)){
    
    set <- iris[ ,3:5]
    point <- set[i,1:2]
    set <- set[-i,]
    set <- sort(set,point)
    
    for(k in seq(1,n-1,1)){
      class <-kNN(set,k)
      if(class != iris[i,5]){
        arrayOfErrors[k] <- arrayOfErrors[k] + 1
      }
      
    }
  }
  res <- which.min(arrayOfErrors[1:n-1])
  return(res)
}


colors <- c("setosa" = "red", "versicolor" = "green4", "virginica" = "blue")
plot(iris[, 3:4], pch = 25, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
point <- cbind(runif(1, 0, 7), runif(1, 0, 2.5))
train_set <- sort(iris[, 3:5],point)
k <-loo()
print(k)
class <- kNN(train_set,k)
points(point[1], point[2], pch = 24, bg = colors[class], asp = 1)
