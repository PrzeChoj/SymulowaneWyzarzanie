source("R/utils.R")

#' Szuka maximum dla ciagu bet
#' 
#' @param funkcja funkcja, ktora chcemy maksymalizowac. Musi przyjmowac pojedyncza permutacje jako argument
#' @param start w jakiej permutacji zaczynamy
#' @param beta wektor kolejnych odwrotnosci temperatur
#' @param number_of_iterations liczba lub wektor długości takiej samej jak beta; liczba iteracji dla kazdej bety
#' @param p rozmiar rozwazanych permutacji
#'
#' @return permutacja ze znalezionym max
symulated_anneling <- function(funkcja, start=permutations::id,
                               beta=c(1,2,3), number_of_iterations=100, p=6,
                               eps_hard_end=0.01,
                               eps_soft_end=0.05, n_soft_end=5,
                               n_perm_end=10,
                               show_progress_bar=TRUE){
  if(show_progress_bar)
    progressBar <- utils::txtProgressBar(min = 0, max = length(beta),
                                        initial = 1)
  
  # number_of_iterations bedzie dla kazdej bety oddzielnie
  if(length(number_of_iterations) == 1){
    number_of_iterations_for_every_beta <- number_of_iterations * (numeric(length(beta)) + 1)
  }else{
    number_of_iterations_for_every_beta <- number_of_iterations
  }
  
  punkt <- start
  soft_end_number <- 0
  perm_end_number <- 0
  called_function_values <- 0
  log_values <- numeric(0)
  
  all_acceptance_rates <- numeric(0)
  points <- list()
  number_of_iterations_cumsum <- cumsum(number_of_iterations_for_every_beta)
    
  for(i in 1:length(beta)){
    b <- beta[i]
    if(show_progress_bar)
      utils::setTxtProgressBar(progressBar, i)
    
    punkt_prev <- punkt
    lista_wynik <- single_symulated_anneling(punkt_prev, b, funkcja,
                                             number_of_iterations_for_every_beta[i],
                                             p)
  
      
    punkt <- lista_wynik[["permutation_found"]]
    found_point_function_logvalue <- lista_wynik[["permutation_found_function_logvalue"]]
    acceptance_rate <- lista_wynik[["acceptance_rate"]]
    points[[i]] <- lista_wynik[["points"]]
    all_acceptance_rates[i] <- acceptance_rate
    called_function_values <- called_function_values + lista_wynik[["called_function_values"]]
    log_values <- c(log_values, lista_wynik[["log_values"]])
    
    # warunki stopu:
    stop_condition <- FALSE
    
    if(acceptance_rate < eps_hard_end){
      stop_condition <- TRUE
    }
    
    if(acceptance_rate < eps_soft_end){
      soft_end_number <- soft_end_number + 1
      if(soft_end_number > n_soft_end){
        stop_condition <- TRUE
      }
    }else{
      soft_end_number <- 0
    }
    
    if(punkt_prev == punkt){
      perm_end_number <- perm_end_number + 1
      if(perm_end_number > n_perm_end){
        stop_condition <- TRUE
      }
    }else{
      perm_end_number <- 0
    }
    
    if(stop_condition){
      break
    }
  }
  
  if(show_progress_bar)
    close(progressBar)
  
  if(!stop_condition){
    warning("Nie osiagnieto warunku stopu. Nie utknelismy jeszcze w minimum lokalnym. Sprobuj z wieksza beta.")
  }
  
  acceptance_rate <- mean(all_acceptance_rates * number_of_iterations_for_every_beta[1:i]) / sum(number_of_iterations_for_every_beta[1:i])
  
  out <- list("acceptance_rate"=acceptance_rate,
              "goal_function_logvalues"=log_values,
              "points"=points,
              "found_point"=punkt,
              "found_point_function_logvalue"=found_point_function_logvalue,
              "last_point"=points[[i]][[number_of_iterations_for_every_beta[i]]],
              "last_point_function_logvalue"=log_values[number_of_iterations_cumsum[i]])
  
  class(out) <- c("gips", "list")
  
  out
}


#' Szuka maximum dla pojedynczej bety; To jest alg Metropolisa (NIE Metropolisa Hastingsa, bo zadanie jest symetryczne)
#' 
#' @param x Te co wyzej
#' @return lista z 2 rzeczami: `permutation_found`, `acceptance_rate`
#' 
single_symulated_anneling <- function(punkt_startowy, b, funkcja,
                                      number_of_iterations, p){
  l_akcept <- 0 # liczba zaakceptowanych zmian permutacji

  X <- list() # lista wybranych permutacji w kolejnych iteracjach
  X[[1]] <- punkt_startowy
  funkcja_punkt_startowy <- funkcja(punkt_startowy)
  funkcja_aktualny <- funkcja_punkt_startowy
  log_values <- numeric(0)
  log_values[1] <- funkcja_aktualny
  
  U_wylosowane <- runif(number_of_iterations)

  for(i in 2:number_of_iterations){
    transp <- runif_transposition(p) # Wyznaczamy losowa transpozycje
    
    prop_X <- as.cycle(X[[i-1]] * transp)  # proponowana permutacja
    funkcja_propozycja <- funkcja(prop_X)
    
    A <- min(exp(b*(funkcja_propozycja - funkcja_aktualny)),1) # prawdopodobienstwo akceptacji
    
    if(U_wylosowane[i] < A){
      
      X[[i]] <- prop_X       # akceptujemy
      l_akcept <- l_akcept + 1
      funkcja_aktualny <- funkcja_propozycja
      log_values[i] <- funkcja_propozycja
      
    } else {
      
      X[[i]] <- X[[i-1]]   # nie akceptujemy
      log_values[i] <- funkcja_aktualny
      
    }
  }
    
  # Sprawdzamy czy otrzymana permutacja daje większą wartość niż poprzednia, tylko jeżeli tak jest to akceptujemy nową.
  
  if(funkcja_aktualny > funkcja_punkt_startowy) {
    wynik <- X[[number_of_iterations]]
    wynik_log_value <- funkcja_aktualny
  }
  else {
    wynik <- punkt_startowy
    wynik_log_value <- funkcja_punkt_startowy
  }
  
  acceptance_rate <- l_akcept / (number_of_iterations-1)
  lista_wynik <- list("acceptance_rate" = acceptance_rate,
                      "permutation_found" = wynik,
                      "permutation_found_function_logvalue" = wynik_log_value,
                      "log_values" = log_values,
                      "points" = X)
  
  return(lista_wynik)
}
