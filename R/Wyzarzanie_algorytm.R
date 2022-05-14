source("utils.R")

#' Szuka maximum dla ciagu bet
#' 
#' @param start w jakiej permutacji zaczynamy
#' @param beta wektor kolejnych odwrotnosci temperatur
#' @param funkcja funkcja, ktora chcemy maksymalizowac. Musi przyjmowac pojedyncza permutacje jako argument
#' @param number_of_iterations liczba iteracji dla kazdej bety
#' @param p rozmiar rozwazanych permutacji
#'
#' @return permutacja ze znalezionym max
symulated_anneling <- function(funkcja, start=permutations::id,
                               beta=c(1,2,3), number_of_iterations=100, p=6,
                               eps_hard_end=0.01,
                               eps_soft_end=0.05, n_soft_end=5,
                               n_perm_end=10,
                               show_progress_bar=TRUE){
  punkt <- start
  soft_end_number <- 0
  perm_end_number <- 0

  if(show_progress_bar)
    progressBar = utils::txtProgressBar(min = 0, max = length(beta),
                                        initial = 1)
    
  for(i in 1:length(beta)){
    b <- beta[i]
    if(show_progress_bar)
      utils::setTxtProgressBar(progressBar, i)
    
    punkt_prev <- punkt
    lista_wynik <- single_symulated_anneling(punkt_prev, b, funkcja,
                                             number_of_iterations, p)
  
      
    punkt <- lista_wynik[["permutation_found"]]
    acceptance_rate <- lista_wynik[["acceptance_rate"]]
    
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
  return(punkt)
}


#' Szuka maximum dla pojedynczej bety
#' 
#' @param x Te co wyzej
#' @return lista z 2 rzeczami: `permutation_found`, `acceptance_rate`
#' 
single_symulated_anneling <- function(punkt, b, funkcja,
                                      number_of_iterations, p){
    l_akcept <- 0 # liczba zaakceptowanych zmian permutacji
  
    X <- list() # lista wybranych permutacji w kolejnych iteracjach
    X[[1]] <- punkt
    funkcja_aktualny <- funkcja(punkt)
    
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
        
      } else {
        
        X[[i]] <- X[[i-1]]   # nie akceptujemy
        
      }
    }
    
    acceptance_rate <- l_akcept / (number_of_iterations-1)
    lista_wynik <- list("acceptance_rate" = acceptance_rate,
                        "permutation_found" = X[[number_of_iterations]])
    
    return(lista_wynik)

}


# jeszcze do poprawienia cos z argumentami goal_function - na ten moment chce wszystkie przy wywolywaniu
# symulated_anneling, a przeciez perm_proposal ma bys ruchome
#symulated_anneling(log(goal_function(, 2, 100, U1)))
