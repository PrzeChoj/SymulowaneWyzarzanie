source("R/Wyzarzanie_algorytm.R")

library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(1234)

p <- 10
n <- 20

sigma_matrix <- diag(p)

eigen_U <- numeric(0)
eigen_U_mh <- numeric(0)
eigen_U_bg <- numeric(0)

M <- 20
progressBar <- utils::txtProgressBar(min = 0, max = 2*M, initial = 1)
for(i in 1:M){
  utils::setTxtProgressBar(progressBar, i*2-1)
  
  example_goal_function <- goal_function_maker(p, n, sigma_matrix)
  U <- attr(example_goal_function, "U")
  
  eigen_U <- c(eigen_U, eigen(U/n)$values)
  
  bg <- best_growth(U, n, show_progress_bar = FALSE)
  U_bg <- gips::project_matrix(U, bg$found_perm, perm_size = p)
  
  eigen_U_bg <- c(eigen_U_bg, eigen(U_bg/n)$values)
  
  utils::setTxtProgressBar(progressBar, i*2)
  
  mh <- gips::MH(U, n, bg$iterations_performed * choose(p, 2), show_progress_bar = FALSE)
  U_mh <- gips::project_matrix(U, mh$found_point, perm_size = p)
  
  eigen_U_mh <- c(eigen_U_mh, eigen(U_mh/n)$values)
}
close(progressBar)



p <- data.frame("eigen_U" = eigen_U,
                "eigen_U_bg" = eigen_U_bg,
                "eigen_U_mh" = eigen_U_mh) %>% 
  pivot_longer(cols = c("eigen_U", "eigen_U_bg", "eigen_U_mh"),
               names_to = "matrix_type", values_to = "eigen_value") %>% 
  ggplot(aes(x=matrix_type, y=eigen_value)) + 
    geom_violin()
p



# sprawdz norme frobieniusa

# sprawdz dla innych macierzy kowariancji