distance <- function(u, v){
  return(sqrt(sum((u - v)^2)))
}

oneNN <- function(set, point){
  min_range <- 1000000000000
  ion <- 1
  
  for(i in 1:150){
    if (distance(set[i, 1:2], point) < min_range){
      min_range <- distance(set[i, 1:2], point)
      ion <- i
    }
  }
  
  return(ion)
}

colors <- c("setosa" = "red", "versicolor" = "green4", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)

point <- cbind(runif(1, 0, 7), runif(1, 0, 2.5))
train_set <- iris[, 3:5]
ind <- oneNN(train_set,point)
points(point[1], point[2], pch = 25, bg = colors[train_set[ind,3]], asp = 1)
