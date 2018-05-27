library(MASS)
library(jpeg)

setwd("~/ws-r/pic/")
img.small <- readJPEG("mount-small.jpg")
img.small

img.fake <- array(sample(50, replace = TRUE), dim = c(12, 8, 3))


to_bw <- function(img){
  apply(img, c(1, 2), mean)
}

img.fake.bw <- to_bw(img.fake)
img.small.bw <- to_bw(img.small)

test.data <- matrix(sample(50, replace = TRUE, size = 10000), ncol = 2, nrow = 200)

test.data2 <- matrix(c(0, 0), ncol = 2)
for(i in 1:50){
  test.data2 <- rbind(test.data2, mvrnorm(n = 2, mu = c(3, 5), Sigma = cbind(c(10, 1), c(1, 10))))
}
for(i in 1:50){
  test.data2 <- rbind(test.data2, mvrnorm(n = 2, mu = c(3, 30), Sigma = cbind(c(25, 1), c(1, 4))))
}
for(i in 1:50){
  test.data2 <- rbind(test.data2, mvrnorm(n = 2, mu = c(45, 30), Sigma = cbind(c(12, 4), c(2, 15))))
}
for(i in 1:50){
  test.data2 <- rbind(test.data2, mvrnorm(n = 2, mu = c(25, 25), Sigma = cbind(c(35, 4), c(2, 3))))
}
for(i in 1:50){
  test.data2 <- rbind(test.data2, mvrnorm(n = 2, mu = c(15, 35), Sigma = cbind(c(22, 4), c(2, 30))))
}
for(i in 1:50){
  test.data2 <- rbind(test.data2, mvrnorm(n = 2, mu = c(30, 10), Sigma = cbind(c(42, 4), c(2, 50))))
}



kmeans <- function(data, k, trace = FALSE, maxiter = 10, change = 0.001, 
                   dimensionality = length(dim(data)), method = "uniform",
                   pic.reduce = NULL){

  reduce_dimensionality <- function(data, dims = 3){
    a <- array()
    for(i in dims:3){
      a <- apply(data, 3:i, c)
    }
    return(a)
  }
  
  init.centroids <- function(method){
    if(method == "uniform"){
      samp <- sample(nrow(pack$data), size = k)
      return(rbind(pack$data[samp, , drop = FALSE]))
    } else if(method == "kmeans++"){
      pass
    }
  }
  
  closest_point <- function(x){
    m <- matrix(x, byrow = TRUE, ncol = pack$num.vars, nrow = k)
    # print("m")
    # print(m)
    which.min(sqrt(rowSums((pack$centroids - m)**2)))
  }
  
  
  pack <- new.env()
  pack$data <- data
  
  if(!is.null(pic.reduce)){
    if(pic.reduce == "bw"){
      pack$data <- cbind(as.vector(pack$data))
    }
  }
  
  if(dimensionality > 2){
    pack$data <- reduce_dimensionality(data, dimensionality)
  }
  pack$num.elem <- nrow(pack$data)
  pack$num.vars <- ncol(pack$data)
  # print(pack$num.vars)
  # print(pack$data)
  # print("num vars")
  # print(pack$num.vars)

  pack$centroids <- init.centroids(method)
  # print("pack data")
  # print(pack$data)
  # print("num elem")
  # print(pack$num.elem)
  # print("centroids")
  # print(pack$centroids)
  
  for(i in 1:maxiter){
    
    print(paste(i * 100/maxiter, "%"))
    
    
    closests <- apply(pack$data, MARGIN = 1, FUN = closest_point)
    # print("closests")
    # print(closests)
    aggs <- aggregate(pack$data, by = list(closests), FUN = mean)
    # pack$centroids <- cbind(aggs[, 2], aggs[ , 3])
    pack$centroids <- aggs[ , -1]


    if(trace && pack$num.vars <= 2){
      plot(pack$data, col = closests, pch = 16)
      points(pack$centroids, pch = "X", col = 1:k, cex = 3)
      points(pack$centroids, pch = "X", col = "white", cex = 1)
    }
    
  }
  
}

kmeans(data = img.small, k = 4, maxiter = 10, trace = TRUE)#, pic.reduce = "bw")



# 
# 
# 
# a <- array()
# for(i in dims:3){
#   a <- apply(a, 3:i, c)
# }












# aggregate(test.data2, by = list(sample(5, replace = TRUE, size = ncol(test.data2))), FUN = mean)
# 




# 
# 
# euclidean_distance <- function(x){
#   sqrt(sum((x[1, ] - x[2, ])**2))
# }
# 
# 
# 
# 
# 
# sums <- by(pack$data, INDICES = list(closests), sum)
# print(sums)
# g <- as.vector(sums) / table(closests)
# print(g)


