distance <- function(x, y) {
    sqrt(sum((x - y)^2))
}

# Following the algorithm of Hartigan and Wong
# https://doi.org/10.2307/2346830

k_means <- function(X, C) {
    distance.matrix <- apply(X, 2, function(x) {
        apply(C, 2, function(c) distance(x, c))
    })
    IC1 <- apply(distance.matrix, 2, function(x) which.min(x))
    IC2 <- apply(distance.matrix, 2, function(x) order(x)[2])
    ncl <- sapply(1:ncol(C), function(x) sum(IC1 == x))
    live.set <- c(TRUE, ncol(C))
    distances <- apply(distance.matrix, 2, function(m) min(m))
    curr.val <- ncl * distances / (ncl - 1)
    for(i in 1:ncol(X)) {
        if(IC1[i] %in% live.set) {
            N <- ncl[IC1[i]]
            distances <- apply(C[, -IC1[i]], 2, function(c) distance(X[, i], c))
            R2 <- ncl * distances^2 / (ncl + 1)
            # need to keep track of inserting / deleting the number of the
            # cluster the observation is currently assigned to 
            # from the vector 
            IC2 <- which.min(R2)
            if(IC2 < curr.val[i]) 
                IC1[i] <- 
        }
        
    }
    
    list(distance.matrix, IC1, IC2, ncl)
    
}
X <- matrix(1, nrow = 3, ncol = 4)
C <- matrix(c(1, 2), nrow = 3, ncol = 2, byrow = T)
k_means(X, C)


