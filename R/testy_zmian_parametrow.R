source("R/Wyzarzanie_algorytm.R")

p <- 15
n <- 20

example_goal_function <- goal_function_maker(p, n)
example_log_goal_function <- log_goal_function_maker(p, n)

actual_permutation <- as.cycle(as.word(c(2:p, 1))) # permutacja dla której osi¹gamy maksimum

example_goal_function(permutations::id)   # na id
example_goal_function(runif_transposition(p)) # na losowej

example_log_goal_function(permutations::id)
example_log_goal_function(actual_permutation) # tego szukamy. To jest podejrzewany max funkcji celu

# Testy wy¿arzania dla ró¿nych metod sch³adzania.

perm_found <- symulated_anneling(example_log_goal_function, p=p, beta=1:100, number_of_iterations = 100)
print(paste0("W wyniku symulowanego wy¿arzania otrzymano permutacjê ",
             perm_found, " dla której wartoœæ logarytmu funkcji celu wynosi ",
             example_log_goal_function(perm_found), "."))


# Wyznaczymy œredni¹ wartoœæ przy 10 próbach wy¿arzania dla ka¿dego typu sch³adzania.



wartosc <- numeric()
permutacje_wynik <- list()
# Pierwszy typ bet: ci¹g b_n = 1/10 + 2n (zaczynamy z temperatury T = 10) 

for(i in 1:10) {
  
  permutacje_wynik[[i]] <- symulated_anneling(example_log_goal_function, p=p, 
                     beta=seq(1/10, 100, 2), number_of_iterations = 100)
  
  wartosc[i] <- example_log_goal_function(permutacje_wynik[[i]])

}

mean(wartosc) # -42.90793

# (1,9)(2,4,11,14,7,15,6,13,8,3,12,5,10) daje wartoœæ -33.40569

max_perm <- as.cycle(as.word(c(9,4,12,11,10,13,15,3,1,2,14,5,8,7,6)))
example_log_goal_function(max_perm)
# Drugi typ bet: ci¹g b_n = suma(1:n) - zaczynamy z niskiej temperatury

wartosc2 <- numeric()

for(i in 1:10) {
  
  wartosc2[i] <- example_log_goal_function(symulated_anneling(example_log_goal_function, p=p, 
                                                             beta=cumsum(1:100), number_of_iterations = 100))
  
}

mean(wartosc2) # -50.28323 - gorzej


# Trzeci typ bet: ci¹g b_n = 

wartosc3 <- numeric()

for(i in 1:10) {
  
  wartosc3[i] <- example_log_goal_function(permutacje_wynik[[i]])
  
}

mean(wartosc) # 

