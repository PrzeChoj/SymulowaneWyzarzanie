install.packages("permutations")
library(permutations)

install.packages("devtools")

devtools::install_github("PrzeChoj/gips")
library(gips)


# przykladowe wywolanie funkcji celu z `gips`
c <- as.cycle(as.word(c(2,1)))
U1 <- matrix(c(1,0.5,0.5,2), nrow=2,byrow = TRUE)
goal_function(c, 2, 100, U1)
goal_function(permutations::id, 2, 100, U1)  # `id` i tak trzeba wołać przez `permutations::id`

# trzeba trzymac wielkosc tablicy
perm <- as.cycle(as.word(c(1,4,5,2,3,6))) # (1)(3,5)(2,4)(6)
permutations::fixed(perm)  # pamięta o 1. Nie pamięta o 6
length(permutations::fixed(perm))  # 5


# zadanie posortowania tablicy
pomieszane <- c(5,6,7,8) # c(5,2,6.4,3)
wielkosc_problemu <- length(pomieszane)

permutacja_sortujaca <- as.cycle(as.word(order(pomieszane)))
permutacja_mieszajaca <- inverse(permutacja_sortujaca)

is.id(permutacja_mieszajaca * permutacja_sortujaca)  # TRUE



goal_function_sorting <- function(wektor, permutacja){
  wielkosc_problemu <- length(wektor)
  wielkosc_problemu_mniejszy <- length(permutations::fixed(permutacja))
  
  funkcja_permutacji_org <- as.function.permutation(permutacja)
  funkcja_permutacji <- function(i){
    if(i<=wielkosc_problemu_mniejszy)
      return(funkcja_permutacji_org(i))
    return(i)
  }
  
  liczba_zlych <- 0
  for(i in 1:(wielkosc_problemu-1)){
    for(j in (i+1):wielkosc_problemu){
      if(wektor[funkcja_permutacji(i)] > wektor[funkcja_permutacji(j)]){
        liczba_zlych <- liczba_zlych + 1
      }
    }
  }
  
  liczba_zlych
}

goal_function_sorting(pomieszane, permutacja_sortujaca)
goal_function_sorting(pomieszane, permutacja_mieszajaca)
(losowa_permutacja <- as.cycle(as.word(sample(wielkosc_problemu))))
goal_function_sorting(pomieszane, losowa_permutacja)


# przeszukanie calej przestrzeni
posortowane <- c(4,5,6,7)
for(i in 1:length(permutations::allperms(4))){
  permutacja <- as.cycle(permutations::allperms(4)[i])
  wartosc <- goal_function_sorting(posortowane,
                                   as.cycle(permutations::allperms(4)[i]))
  print(paste0(permutacja, ", ", wartosc))
}


# przyklad dla wymiaru 100; tak duzej nieda sie przeszukac
M <- 100
losowa_permutacja1 <- sample(M)
losowa_permutacja2 <- as.cycle(as.word(sample(M)))
goal_function_sorting(losowa_permutacja1, losowa_permutacja2)














