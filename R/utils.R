library(gips)
library(permutations)

goal_function_maker <- function(p, n, sigma_matrix=NULL){
  mu <- numeric(p)
  if(is.null(sigma_matrix)){
    sigma_matrix <- matrix(numeric(p*p), nrow=p)
    for(i in 1:p){
      for(j in 1:p){
        sigma_matrix[i,j] <- 1 - min(abs(i-j), p-abs(i-j)) / p
      }
      sigma_matrix[i,i] <- 1 + 1/p
    }
  }
  
  Z <- MASS::mvrnorm(n, mu = mu, Sigma = sigma_matrix)
  U <- t(Z) %*% Z
  
  my_goal_function <- function(perm){
    goal_function(perm, n, U)
  }
  
  attr(my_goal_function, "U") <- U
  
  my_goal_function
}


# Losowanie transpozycji
runif_transposition <- function(perm_size){
  permutations::as.cycle(sample(perm_size, 2,
                                replace=FALSE))
}

#' Draw a random permutation
runif_perm <- function(perm_size){
  permutations::as.cycle(permutations::as.word(sample(perm_size, perm_size,
                                                      replace=FALSE)))
}




