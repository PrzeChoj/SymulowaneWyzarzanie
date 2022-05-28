source("R/Wyzarzanie_algorytm.R")


# Dobrane parametry
p <- 10
n <- 20

# Od razu goal function jest juz zlogarytmowana

example_goal_function <- goal_function_maker(p, n)

actual_permutation <- as.cycle(as.word(c(2:p, 1))) # permutacja dla ktorej osiagamy maksimum

example_goal_function(permutations::id)   # na id
example_goal_function(runif_transposition(p)) # na losowej
example_goal_function(actual_permutation) # na kandydacie na max

# Testy wyzarzania dla roznych metod schladzania.

perm_found <- symulated_anneling(example_goal_function, p=p, beta=1:100, number_of_iterations = 100)
print(paste0("W wyniku symulowanego wyżarzania otrzymano permutację ",
             perm_found, " dla której wartość logarytmu funkcji celu wynosi ",
             example_goal_function(perm_found), "."))


#####################################
#####################################
#####################################
#####################################


# Wyznaczymy srednia wartosc przy 10 probach wyzarzania dla kazdego typu schladzania.

# Pierwszy typ bet: ciąg b_n = 1/10 + 2n (zaczynamy z temperatury T = 10) .
# Drugi typ bet: ciąg b_n = cumsum(1:n).
# Trzeci typ bet: ciag b_n = 1/100, 1/90, 1/80, ..., 200 - zeby zaczac z wysokiej temperatury.
# Czwarty typ bet: ciag b_n = 1/10, 1/9, 1/8, ..., 10.
# Piąty typ bet: ciag b_n = log(log(n)).
# Szósty typ bet: ciag b_n = log(n) (na starcie kilka innych wartości).

set.seed(1234)

wartosc1 <- numeric()
permutacje_wynik1 <- list()

wartosc2 <- numeric()
permutacje_wynik2 <- list()

wartosc3 <- numeric()
permutacje_wynik3 <- list()

wartosc4 <- numeric()
permutacje_wynik4 <- list()

wartosc5 <- numeric()
permutacje_wynik5 <- list()

wartosc6 <- numeric()
permutacje_wynik6 <- list()


perm_start <- permutations::id # Wybrana permutacja startowa
n_iter <- 1000 # Wybrana liczba iteracji dla pojedynczj bety

for(i in 1:10) {
  
  print(paste0("Symulowane wyżarzanie: próba nr ", i))
  
  permutacje_wynik1[[i]] <- symulated_anneling(example_goal_function, start = perm_start, p=p, 
                                               beta=seq(1/10, 100, 2), number_of_iterations = n_iter)[["found_point"]]
  wartosc1[i] <- example_goal_function(permutacje_wynik1[[i]])
  
  permutacje_wynik2[[i]] <- symulated_anneling(example_goal_function, start = perm_start, p=p, 
                                               beta=cumsum(1:100), number_of_iterations = n_iter)[["found_point"]]
  wartosc2[i] <- example_goal_function(permutacje_wynik2[[i]])
  
  permutacje_wynik3[[i]] <- symulated_anneling(example_goal_function, start = perm_start, p=p, 
                                               beta=c(1/ (10 * (10:1)), 1, 10 * (1:20)),
                                               number_of_iterations = n_iter)[["found_point"]]
  wartosc3[i] <- example_goal_function(permutacje_wynik3[[i]])
  
  permutacje_wynik4[[i]] <- symulated_anneling(example_goal_function, start= perm_start, p=p, 
                                               beta=c(1/ (10:1), 2:20),
                                               number_of_iterations = n_iter)[["found_point"]]
  wartosc4[i] <- example_goal_function(permutacje_wynik4[[i]])
  
  permutacje_wynik5[[i]] <- symulated_anneling(example_goal_function, start= perm_start, p=p, 
                                               beta=log(log(3:100)), 
                                               number_of_iterations = n_iter)[["found_point"]]
  wartosc5[i] <- example_goal_function(permutacje_wynik5[[i]])
  
  permutacje_wynik6[[i]] <- symulated_anneling(example_goal_function,start= perm_start, p=p, 
                                               beta=c(1/10,1/8,1/6,1/4,1/3,1/2,log(2:100)), 
                                               number_of_iterations = n_iter)[["found_point"]]
  wartosc6[i] <- example_goal_function(permutacje_wynik6[[i]])
  
}

# Wykresy obrazujace proby wyzarzania dla roznych bet

plot(wartosc1, main = "10 prób symulowanego wyżarzania dla różnych ciągów temperatur" , xlab = "Numer próby", ylab = "Osiągnięta wartość funkcji celu",
     ylim = c(example_goal_function(perm_start) - 10,example_goal_function(actual_permutation) + 10), font.lab=2, font = 2, type = "b", lwd=2)
axis(side=1, at=1:10, labels = TRUE, font = 2)
axis(side=2, at=c(-10,0,10,20,30,40,50,60), labels = TRUE, font = 2)

lines(wartosc2, col = "red", lwd=2, type="b")
lines(wartosc3, col = "blue", lwd=2, type="b")
lines(wartosc4, col = "burlywood", lwd = 2, type="b")
lines(wartosc5, col = "chartreuse1", lwd = 2, type="b")
lines(wartosc6, col = "coral", lwd = 2, type="b")
lines(replicate(10,example_goal_function(perm_start)), col = "green",lwd = 2, type="b") # startowa
lines(replicate(10,example_goal_function(actual_permutation)), col = "aquamarine3",lwd = 2, type="b") # max
text(x = 5.5, y = example_goal_function(perm_start) - 5, "Permutacja startowa", font = 2)
text(x = 5.5, y = example_goal_function(actual_permutation) + 5, "Kandydat na max", lwd = 3, font = 2)
legend("topright", legend=c("Bety 1", "Bety 2", "Bety 3", "Bety 4", "Bety 5", "Bety 6"),
       col=c("black", "red", "blue", "burlywood", "chartreuse1", "coral"), 
       lty = 1, lwd = 2, cex=0.8, text.font = 2, horiz=TRUE)


#####################################
#####################################
#####################################
#####################################

# Wykresy porownujace na inny sposob zbieznosc do max dla roznych bet (EPDF).
# Wartosc 0 na wykresie odpowiada wartosci logarytmu goal_function w permutacji startowej (id),
# a wartosc 1 na wykresie odpowiada maksymalnej wartosci logarytmu goal_function.

# Kryteria stopu sa wylaczone przy tych testach.

# Parametry goal_function
p <- 10 # bedziemy badac dla p = 10
n <- 100

example_goal_function <- goal_function_maker(p, n)

# Typy ciagow bet
beta <- list(1:20, log(2:21), log(log(3:22)), sqrt(log(2:21)))

# Liczba iteracji
number_of_iterations <- 1000

# Liczba wywolan wyzarzania dla danego ciagu bet
M <- 10

# Wywolanie wyzarzania M razy dla kazdego z ciagow bet
list_of_lists_of_log_values <- get_list_of_lists_of_log_values(example_goal_function,
                                                               p, beta,
                                                               number_of_iterations, M)


# Na wykresie usredniamy uzyskane wartosci w wywolaniach wyzarzania (patrzymy na poszczegolne iteracje).
# Rozne krzywe odpowiadaja roznym betom.

plot_epdf(values_list = list_of_lists_of_log_values,
          min_val = example_goal_function(permutations::id),
          max_val = example_goal_function(actual_permutation),
          max_y_scale = 1,
          legend_text = c("b_n = n", "b_n = log(n+1)", "b_n = log(log(n+2))", "b_n = sqrt(log(n+1))"))

# log(log(n)) radzi sobie zdecydowanie najlepiej. Porownam jeszcze z log(log(log(n))).

beta2 <- list(log(log(3:22)), log(log(log(16:35))))
list_of_lists_of_log_values <- get_list_of_lists_of_log_values(example_goal_function, p, beta2, number_of_iterations, M)

plot_epdf(values_list = list_of_lists_of_log_values,
          min_val = example_goal_function(permutations::id),
          max_val = example_goal_function(actual_permutation),
          max_y_scale = 1,
          legend_text = c("b_n = log(log(n+2))", "b_n = log(log(log(n+15)))"))

# Wychodza podobnie do siebie.
# Sprawdze teraz dla b_n = log(log(n+2)) rozne liczby iteracji i jak algorytm dla nich dziala.

beta3 <- log(log(3:22))
number_of_iterations <- 10 * 2^(0:7)
M <- 30

list_of_lists_of_log_values <- get_list_of_lists_of_log_values(example_goal_function,
                                                               p, beta3,
                                                               number_of_iterations, M)

plot_epdf(values_list = list_of_lists_of_log_values,
          min_val = example_goal_function(permutations::id),
          max_val = 100,
          max_y_scale = 1)

# 1280 iteracji przynosi już niewielki zysk ponad 640.

#####################################
#####################################
#####################################
#####################################

# Sprawdzę teraz jak dla 700 iteracji poradzi sobie 100 wywołań wyżarzania (dla bety log(log(n))) )

# Dobrane parametry - najpierw p = 10, potem p = 15 (z wlaczonymi warunkami stopu)
p <- 10
n <- 20

example_goal_function <- goal_function_maker(p, n)
actual_permutation <- as.cycle(as.word(c(2:p, 1)))

perm_start <- permutations::id # Wybrana permutacja startowa
n_iter <- 700 # Wybrana liczba iteracji dla pojedynczj bety

for(i in 1:100) {
  
  print(paste0("Symulowane wyżarzanie: próba nr ", i))
  
  permutacje_wynik5[[i]] <- symulated_anneling(example_goal_function, start= perm_start, p=p, 
                                               beta=log(log(3:100)), 
                                               number_of_iterations = n_iter)[["found_point"]]
  wartosc5[i] <- example_goal_function(permutacje_wynik5[[i]])
  
  
}


plot(wartosc5, main = "100 prób symulowanego wyżarzania dla ciągu log(log(n))" , xlab = "Numer próby", ylab = "Osiągnięta wartość funkcji celu",
     ylim = c(example_goal_function(perm_start) - 10,example_goal_function(actual_permutation) + 10), font.lab=2, font = 2, type = "b", lwd=2)
axis(side=1, at=1:100, labels = FALSE, font = 2)
axis(side=2, at=c(-10,0,10,20,30,40,50,60), labels = TRUE, font = 2)
lines(replicate(100,example_goal_function(perm_start)), col = "green",lwd = 2, type="l") # startowa
lines(replicate(100,example_goal_function(actual_permutation)), col = "aquamarine3",lwd = 2, type="l") # max
text(x = 50, y = example_goal_function(perm_start) - 5, "Permutacja startowa", font = 2)
text(x = 50, y = example_goal_function(actual_permutation) + 5, "Kandydat na max", lwd = 3, font = 2)

mean(wartosc5)


# Jeszcze na koniec porownanie najlepszego znalezionego ciagu bet ze stalym ciagiem jedynek.

p <- 10
n <- 100

example_goal_function <- goal_function_maker(p, n)
actual_permutation <- as.cycle(as.word(c(2:p, 1)))

beta <- list(log(log(3:22)), rep(1,20))
number_of_iterations <- 700
M <- 50

list_of_lists_of_log_values <- get_list_of_lists_of_log_values(example_goal_function,
                                                               p, beta,
                                                               number_of_iterations, M)

plot_epdf(values_list = list_of_lists_of_log_values,
          min_val = example_goal_function(permutations::id),
          max_val = example_goal_function(actual_permutation),
          max_y_scale = 1,
          legend_text = c("b_n = log(log(n+2))", "b_n = 1"))

# log(log(n)) osiaga lepsze rezultaty niz 1
