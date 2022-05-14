source("utils.R")

#' Szuka maximum dla ciagu bet
#' 
#' @param start w jakiej permutacji zaczynamy
#' @param beta wektor kolejnych odwrotnosci temperatur
#' @param funkcja funkcja, ktora chcemy maksymalizowac. Musi przyjmowac pojedyncza permutacje jako argument
#' @param n liczba iteracji dla kazdej bety
#' @param p rozmiar rozwazanych permutacji
#'
#' @return permutacja ze znalezionym max
#' 
symulated_anneling <- function(funkcja, start=permutations::id,
                               beta=c(1,2,3), n=1000, p=6, eps=0.01){
  punkt <- start
  
  for(b in beta){
    lista_wynik <- single_symulated_anneling(punkt, b, funkcja, n, p)
    
    punkt <- lista_wynik[["permutation_found"]]
    acceptance_rate <- lista_wynik[["acceptance_rate"]]
    if(acceptance_rate < eps * n){
      return(punkt)
    }
  }
  
  warning("Nie osiagnieto warunku stopu. Nie utknelismy jeszcze w minimum lokalnym. Sprobuj z wieksza beta.")
  return(punkt)
}


#' Szuka maximum dla pojedynczej bety
#' 
#' @param x Te co wyzej
#' @return lista z 2 rzeczami: `permutation_found`, `acceptance_rate`
#' 
single_symulated_anneling <- function(punkt, b, funkcja, n, p){
  
    l_akcept <- 0 # liczba zaakceptowanych zmian permutacji
  
    X <- list() # lista wybranych permutacji w kolejnych iteracjach
    X[[1]] <- punkt
    funkcja_aktualny <- funkcja(punkt)

    for(i in 2:n){
      
      transp <- runif_transposition(p) # Wyznaczamy losowa transpozycje
      
      prop_X <- X[[i-1]] * transp  # proponowana permutacja
      funkcja_propozycja <- funkcja(prop_X)
      
      A = min(exp(b*(funkcja_propozycja - funkcja_aktualny)),1) # prawdopodobienstwo akceptacji
      
      if(runif(1)<A){
        
        X[[i]] = prop_X       # akceptujemy
        l_akcept <- l_akcept + 1
        funkcja_aktualny <- funkcja_propozucja
        
      } else {
        
        X[[i]] <- aktual_X   # nie akceptujemy
        
      }
    }
    
    acceptance_rate <- l_akcept / (n-1)
    lista_wynik <- list("acceptance_rate" = acceptance_rate, "permutation_found" = X[[n]])
    
    return(lista_wynik)

}

# jeszcze do poprawienia cos z argumentami goal_function - na ten moment chce wszystkie przy wywolywaniu
# symulated_anneling, a przeciez perm_proposal ma bys ruchome
#symulated_anneling(log(goal_function(, 2, 100, U1)))
