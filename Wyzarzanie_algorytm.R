library(gips)
library(permutations)

# Wyszukiwanie losowej transpozycji
runif_transposition <- function(perm_size){
  permutations::as.cycle(sample(perm_size, 2, replace=FALSE))
}


#' Szuka maximum dla ci¹gu bet
#' 
#' @param start w jakiej permutacji zaczynamy
#' @param beta wektor kolejnych odwrotnosci temperatur
#' @param funkcja funkcja, któr¹ chcemy maksymalizowaæ. Musi przyjmowaæ pojedyncz¹ permutacjê jako argument
#' @param n max liczba iteracji dla ka¿dej bety
#' @param p rozmiar rozwa¿anych permutacji
#'
#' @return permutacja ze znalezionym max
#' 
symulated_anneling <- function(funkcja, start=permutations::id,
                               beta=c(1,2,3), n=1000, p=6, eps=0.01){
  punkt <- start
  
  for(b in beta){
    lista_wynik <- single_symulated_anneling(punkt, b, funkcja, n, p, dziedzina)
    
    punkt <- lista_wynik[["permutation_found"]]
    acceptance_rate <- lista_wynik[["acceptance_rate"]]
    if(acceptance_rate < eps * n){
      return(punkt)
    }
  }
  
  warning("Nie osi¹gniêto warunku stopu. Nie utknêliœmy jeszcze w minimum lokalnym. Spróbuj z wiêksz¹ bet¹.")
  return(punkt)
}


#' Szuka maximum dla pojedynczej bety
#' 
#' @param x Te co wy¿ej
#' @param dziedzina zbiór wszystkich permutacji dla wybranego rozmiaru p
#' @return lista z 2 rzeczami: `permutation_found`, `acceptance_rate`
#' 
single_symulated_anneling <- function(punkt, b, funkcja, n, p, dziedzina){
  
    l_akcept <- 0 # liczba zaakceptowanych zmian permutacji w trakcie iteracji
  
    X = list() # lista wybranych permutacji w kolejnych iteracjach
    X[[1]] = punkt

    for(i in 2:n){
      
      transp <- runif_transposition(p) # Wyznaczamy losow¹ transpozycjê
      
      prop_X = X[[i-1]] * transp  # proponowana permutacja
      
      A = min(exp(b*(funkcja(prop_X) - funkcja(aktual_X))),1) # prawdopodobieñstwo akceptacji
      
      if(runif(1)<A){
        
        X[[i]] = prop_X       # akceptujemy
        l_akcept <- l_akcept + 1
        
      } else {
        
        X[[i]] = aktual_X   # nie akceptujemy
        
      }
    }
    
    acceptance_rate <- l_akcept / (n-1)
    lista_wynik <- list("acceptance_rate" = acceptance_rate, "permutation_found" = X[[n]])
    
    return(lista_wynik)

}

# jeszcze do poprawienia coœ z argumentami goal_function - na ten moment chce wszystkie przy wywo³ywaniu
# symulated_anneling, a przecie¿ perm_proposal ma byæ ruchome
symulated_anneling(log(goal_function(, 2, 100, U1)))
