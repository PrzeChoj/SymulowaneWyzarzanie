#install.packages("permutations")
#install.packages("devtools")
#devtools::install_github("PrzeChoj/gips")

source("R/Wyzarzanie_algorytm.R")

# trzeba trzymac wielkosc tablicy
perm <- as.cycle(as.word(c(1,4,5,2,3,6))) # (1)(3,5)(2,4)(6)
permutations::fixed(perm)  # pamięta o 1. Nie pamięta o 6
length(permutations::fixed(perm))  # 5


# przyklad z `goal_function_maker`:
p <- 15
n <- 20

example_goal_function <- goal_function_maker(p, n)
example_log_goal_function <- log_goal_function_maker(p, n)

actual_permutation <- as.cycle(as.word(c(2:p, 1)))

example_goal_function(permutations::id)   # to jest malo
example_goal_function(actual_permutation) # tego szukamy. To jest max funkcji celu
example_goal_function(runif_transposition(p))

example_log_goal_function(permutations::id)   # to jest malo
example_log_goal_function(actual_permutation) # tego szukamy. To jest max funkcji celu

example_log_goal_function(runif_transposition(p))

# porownanie podstawowego MH i symulowanego wyzarzania:
number_of_iterations <- 100
beta <- c(1,2,3,4)
# beta <- c(1:100)

perm_found <- symulated_anneling(example_log_goal_function, p=p, beta=beta)
print(paste0("symulated_anneling found value ",
             example_log_goal_function(perm_found), " with ",
             attr(perm_found, "called_function_values"),
             " calls of goal function"))

mh <- MH(U = attr(example_log_goal_function, "U"), n_number = n,
                    max_iter = attr(perm_found, "called_function_values"),
                    start = permutations::id)
perm_found_MH <- mh$found_point

print(paste0("Metrop-Hastings found value ",
             example_log_goal_function(perm_found_MH), " with ",
             length(mh$points),
             " calls of goal function"))







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














