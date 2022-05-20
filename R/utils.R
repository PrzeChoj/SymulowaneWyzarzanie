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


#' Trim values to 0-1 range
#' 
trim_values <- function(values, min_val=NULL, max_val=NULL){
  if(is.null(min_val))
    min_val = min(values)
  if(is.null(max_val))
    max_val = max(values)
  
  pmin(1, pmax(0, (cummax(values) - min_val) / max_val))
}

#' Plot EPDF for multiple sets of values
#' 
#' @param values_list list of lists of values of goal_function that were found in the iteration; for a optimization method has to be the same length
#' @param min_val value that will be considered 0
#' @param max_val value that will be considered 1
plot_epdf <- function(values_list, min_val, max_val, xlog = TRUE,
                      line_colours = "rainbow", max_y_scale = 1,
                      show_legend = TRUE, legend_text = NULL){
  stopifnot(max_y_scale > 0, max_y_scale <= 1)
  
  num_of_algorithms <- length(values_list)
  num_of_tries <- sapply(values_list, length)
  num_of_iters <- sapply(1:num_of_algorithms, function(i){
    length(values_list[[i]][[1]])
  })
  
  if((length(line_colours) == 1) && (line_colours == "rainbow")){
    line_colours <- rainbow(num_of_algorithms)
  }
  stopifnot(length(line_colours) == num_of_algorithms)
  
  xlim <- c(1, max(num_of_iters))
  if(xlog){
    xlim <- c(0, log10(xlim[2]))
  }
  ylim <- c(0, max_y_scale)
  
  graphics::plot.new()
  graphics::plot.window(xlim, ylim)
  
  for(i in 1:num_of_algorithms){
    avrage_for_ith_algorithm <- numeric(num_of_iters[i])
    for(j in 1:num_of_tries[i]){
      trimed_values <- trim_values(values_list[[i]][[j]], min_val, max_val)
      avrage_for_ith_algorithm <- avrage_for_ith_algorithm + cummax(trimed_values)
    }
    avrage_for_ith_algorithm <- avrage_for_ith_algorithm / num_of_tries[i]
    
    x_cords <- 1:length(avrage_for_ith_algorithm)
    if(xlog){
      x_cords <- log10(x_cords)
    }
    graphics::lines.default(x_cords, avrage_for_ith_algorithm,
                            type = "l", col = line_colours[i])
  }
  
  if(xlog){
    xlab <- "log10 of number of function calls"
  }else{
    xlab <- "number of function calls"
  }
  graphics::title(main = "EPDF plot", sub = "for different algorithms",
                  xlab = xlab, ylab = "optimization goals reached")
  graphics::axis(1)
  graphics::axis(2)
  graphics::box()
  
  if(show_legend){
    if(is.null(legend_text))
      legend_text <- paste0("algorytm ", 1:num_of_algorithms)
    
    graphics::legend("topleft", inset=.002,
                     legend = legend_text,
                     col = line_colours, cex = 0.7,
                     lwd = 1)
  }
  
  invisible(NULL)
}

#' 1. beta jest lista, wtedy testujesz algorytmy z roznymi betami
#' 2. number_of_iterations jest wektorem, wtedy testujesz algorytmy z roznymi długościami kroków pojedynczych iteracji `single_symulated_anneling`
get_list_of_lists_of_log_values <- function(goal_function, p, beta, number_of_iterations, M){
  list_of_lists_of_log_values <- list()
  
  number_of_loops <- ifelse(is.list(beta),
                            length(beta), # testujemy różne strategie dla wzrastania beta
                            length(number_of_iterations)) # testujemy różne strategie liczby krokow w iteracji
  
  stopifnot(number_of_loops > 1)
  
  progressBar_iterations <- number_of_loops * M
  
  progressBar <- utils::txtProgressBar(initial = 1, min = 0,
                                       max = progressBar_iterations)
  
  for(i in 1:number_of_loops){
    list_of_log_values <- list()
    for(j in 1:M){
      utils::setTxtProgressBar(progressBar, (i-1)*M + j)
      
      if(is.list(beta)){
        beta_i <- beta[[i]]
        number_of_iterations_i <- number_of_iterations
      }else{
        beta_i <- beta
        number_of_iterations_i <- number_of_iterations[i]
      }
      
      sa <- symulated_anneling(goal_function, p=p, beta=beta_i,
                               number_of_iterations = number_of_iterations_i,
                               show_progress_bar=FALSE,
                               stopping_criteria=FALSE)
      list_of_log_values[[j]] <- sa[["goal_function_logvalues"]]
    }
    list_of_lists_of_log_values[[i]] <- list_of_log_values
  }
  close(progressBar)
  
  list_of_lists_of_log_values
}





