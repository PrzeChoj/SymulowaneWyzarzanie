library(gips)

#' Szuka maximum dla ciągu bet
#' 
#' @param start w jakiej permutacji zaczynamy
#' @param beta wektor kolejnych odwrotnosci temperatur
#' @param funkcja funkcja, którą chcemy maksymalizować. Musi przyjmować pojedyńczą permutację jako argument
#' @param n max liczba iteracji dla każdej bety
#' @param p rozmiar rozwarzanych permutacji
#' 
#' @return permutacja ze znalezionym max
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
  
  warning("Nie osiągnięto warunku stopu. Nie utnkęliśmy jeszcze w minimum lokalnym. Spróbuj z większą betą.")
  return(punkt)
}


#' Szuka maximum dla pojedyńczej bety
#' 
#' @param x Te co wyżej
#' 
#' @return lista z 2 rzeczami: `permutation_found`, `acceptance_rate`
single_symulated_anneling <- function(punkt, b, funkcja, n, p){
  
}