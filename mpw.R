distance <- function(u, v){
  return(sqrt(sum((u - v)^2)))
}
mpw <- function(set,h,point){
  n <- dim(set)[1]
  weights <- matrix(0,n,1)
  #ok <- 0
  for (i in seq(1, n, 1)) {
    dist <- distance(set[i,1:2],point)
    weights[i] <- ((2*pi)^(-0.5))*exp(-(dist/h)*(dist/h)/2)
    #if (dist <= h) {
      #ok <- 1
      #weights[i] <- 0.5
      #weights[i] <- 3*(1 - (dist/h)*(dist/h))/4
      #weights[i] <- 1 - abs(dist/h)
      #weights[i] <- 15*(1 - (dist/h)*(dist/h))/16weights[i] <- 3*(1-(dist/h)*(dist/h))/4
    #}
  }
  #if (ok == 0) {
    #return("white")
  #}
    
  order_and_weight <- cbind(set, weights)
  classes <- order_and_weight[ ,3:4]
  
  w1 <- sum(classes[classes$Species == "setosa", 2])
  w2 <- sum(classes[classes$Species == "versicolor", 2])
  w3 <- sum(classes[classes$Species == "virginica", 2])
  #print("weights")
  #print(w1)
  #print(w2)
  #print(w3)
  #if ( w1 == 0 && w2 == 0 && w3 == 0 ) {
    #return("white")
  #}
  
  answer <- matrix(c(w1, w2, w3), nrow = 1, ncol = 3, byrow = TRUE, list(c(1), c(1, 2, 3)))
  class <- c("setosa", "versicolor", "virginica")
  
  return(class[which.max(answer)])
}

loo <- function(){
  arrayOfErrors <- matrix(0,20,1)
  for (i in seq(1,150,1)) {
    set <- iris[ ,3:5]
    point <- set[i,1:2]
    set <- set[-i,]
    for (h in seq(1,20,1)) {
      class <- mpw(set,h/10,point)
      #print(class)
      #print(point)
      if (class != iris[i,5] || class == "white") {
        arrayOfErrors[h] <- arrayOfErrors[h] + 1
      }
      
    }
    
  }
  res <- which.min(arrayOfErrors)
  print(arrayOfErrors)
  print(res)
  return(res)
}

colors <- c("setosa" = "red", "versicolor" = "green4", "virginica" = "blue","white" = "white")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, ylim = c(0,3))
h <- loo()/10
set <- iris[, 3:5]
for (x in seq(1,7,0.1)) {
  for (y in seq(0,3,0.1)) {
    point <- c(x,y)
   # print(point)
    class <- mpw(set,h,point)
   # print(class)
    points(point[1], point[2], pch = 1, col = colors[class], asp = 1)
  }
}