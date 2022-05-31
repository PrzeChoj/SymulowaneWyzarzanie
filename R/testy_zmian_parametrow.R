source("R/Wyzarzanie_algorytm.R")

set.seed(1234)

# Dobrane parametry
p <- 10
n <- 20

# Od razu goal function jest juz zlogarytmowana

example_goal_function1 <- goal_function_maker(p, n)

actual_permutation <- as.cycle(as.word(c(2:p, 1))) # permutacja dla ktorej osiagamy maksimum

example_goal_function1(permutations::id)   # na id
example_goal_function1(runif_transposition(p)) # na losowej
example_goal_function1(actual_permutation) # na kandydacie na max

# Testy wyzarzania dla roznych metod schladzania.

sa <- symulated_anneling(example_goal_function1, p=p, beta=1:100, number_of_iterations = 100)
print(sa, log_value = TRUE)


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

# to jest bez sensu, bo rożne bety będą powodować różne momenty zatrzymania się algorytmu.
  # Zawsze tak jest, że im dłużej algorytm działa, tym lepszą wartość znajdzie, bo można
  # sie tylko z czasem poprawiać.
for(i in 1:10) {
  
  print(paste0("Symulowane wyżarzanie: próba nr ", i))
  
  permutacje_wynik1[[i]] <- symulated_anneling(example_goal_function1, start = perm_start, p=p, 
                                               beta=seq(1/10, 100, 2), number_of_iterations = n_iter)[["found_point"]]
  wartosc1[i] <- example_goal_function1(permutacje_wynik1[[i]])
  
  permutacje_wynik2[[i]] <- symulated_anneling(example_goal_function1, start = perm_start, p=p, 
                                               beta=cumsum(1:100), number_of_iterations = n_iter)[["found_point"]]
  wartosc2[i] <- example_goal_function1(permutacje_wynik2[[i]])
  
  permutacje_wynik3[[i]] <- symulated_anneling(example_goal_function1, start = perm_start, p=p, 
                                               beta=c(1/ (10 * (10:1)), 1, 10 * (1:20)),
                                               number_of_iterations = n_iter)[["found_point"]]
  wartosc3[i] <- example_goal_function1(permutacje_wynik3[[i]])
  
  permutacje_wynik4[[i]] <- symulated_anneling(example_goal_function1, start= perm_start, p=p, 
                                               beta=c(1/ (10:1), 2:20),
                                               number_of_iterations = n_iter)[["found_point"]]
  wartosc4[i] <- example_goal_function1(permutacje_wynik4[[i]])
  
  permutacje_wynik5[[i]] <- symulated_anneling(example_goal_function1, start= perm_start, p=p, 
                                               beta=log(log(3:100)), 
                                               number_of_iterations = n_iter)[["found_point"]]
  wartosc5[i] <- example_goal_function1(permutacje_wynik5[[i]])
  
  permutacje_wynik6[[i]] <- symulated_anneling(example_goal_function1,start= perm_start, p=p, 
                                               beta=c(1/10,1/8,1/6,1/4,1/3,1/2,log(2:100)), 
                                               number_of_iterations = n_iter)[["found_point"]]
  wartosc6[i] <- example_goal_function1(permutacje_wynik6[[i]])
  
}

# Wykresy obrazujace proby wyzarzania dla roznych bet

plot(wartosc1, main = "10 prób symulowanego wyżarzania dla różnych ciągów temperatur" , xlab = "Numer próby", ylab = "Osiągnięta wartość funkcji celu",
     ylim = c(example_goal_function1(perm_start) - 10,example_goal_function1(actual_permutation) + 10), font.lab=2, font = 2, type = "b", lwd=2)
axis(side=1, at=1:10, labels = TRUE, font = 2)
axis(side=2, at=c(-10,0,10,20,30,40,50,60), labels = TRUE, font = 2)

lines(wartosc2, col = "red", lwd=2, type="b")
lines(wartosc3, col = "blue", lwd=2, type="b")
lines(wartosc4, col = "burlywood", lwd = 2, type="b")
lines(wartosc5, col = "chartreuse1", lwd = 2, type="b")
lines(wartosc6, col = "coral", lwd = 2, type="b")
lines(replicate(10,example_goal_function1(perm_start)), col = "green",lwd = 2, type="b") # startowa
lines(replicate(10,example_goal_function1(actual_permutation)), col = "aquamarine3",lwd = 2, type="b") # max
text(x = 5.5, y = example_goal_function1(perm_start) - 5, "Permutacja startowa", font = 2)
text(x = 5.5, y = example_goal_function1(actual_permutation) + 5, "Kandydat na max", lwd = 3, font = 2)
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

set.seed(1234)

example_goal_function2 <- goal_function_maker(p, n)

# Typy ciagow bet
beta <- list(rep(1, 20), 1:20, log(2:21), sqrt(log(2:21)), log(log(1:20 + 2)),
             log(log(log(1:20 + 15))), log(log(log(1:20 + 30))))

# Liczba iteracji
number_of_iterations <- 1000

# Liczba wywolan wyzarzania dla danego ciagu bet
M <- 10

# Wywolanie wyzarzania M razy dla kazdego z ciagow bet
list_of_lists_of_log_values_betas <- get_list_of_lists_of_log_values(example_goal_function2,
                                                                     p, beta,
                                                                     number_of_iterations, M)


# Na wykresie usredniamy uzyskane wartosci w wywolaniach wyzarzania (patrzymy na poszczegolne iteracje).
# Rozne krzywe odpowiadaja roznym betom.

#save(list_of_lists_of_log_values_betas, file="data/list_of_lists_of_log_values_betas.RData")
#load("data/list_of_lists_of_log_values_betas.RData")

plot_epdf(values_list = list_of_lists_of_log_values_betas,
          min_val = example_goal_function2(permutations::id),
          max_val = example_goal_function2(actual_permutation),
          max_y_scale = 1,
          legend_text = c("b_n = 1", "b_n = n", "b_n = log(n+1)",
                          "b_n = sqrt(log(n+1))", "b_n = log(log(n+2))",
                          "b_n = log(log(log(n+15)))"),
          my_title = paste0("EPDF plot - mean of ", M, " runs"))

# log(log(n)) oraz log(log(log(n))) radzą sobie podobnie i zdecydowanie lepiej od innych.


# Sprawdze teraz dla b_n = log(log(n+2)) rozne liczby iteracji i jak algorytm dla nich dziala.

set.seed(1234)

beta3 <- log(log(3:22))
number_of_iterations <- 10 * 2^(0:7)
M <- 30

# MiNI, 2 godziny:
list_of_lists_of_log_values_num_iters <- get_list_of_lists_of_log_values(example_goal_function2,
                                                                         p, beta3,
                                                                         number_of_iterations, M)
#save(list_of_lists_of_log_values_num_iters, file="data/list_of_lists_of_log_values_num_iters.RData")
#load("data/list_of_lists_of_log_values_num_iters.RData")

plot_epdf(values_list = list_of_lists_of_log_values_num_iters,
          min_val = example_goal_function2(permutations::id),
          max_val = example_goal_function2(actual_permutation), # TODO dlaczego tu bylo wpisane 100?
          max_y_scale = 1,
          legend_text = paste0("number_of_iterations = ", number_of_iterations))

# 1280 iteracji przynosi już niewielki zysk ponad 640.

#####################################
#####################################
#####################################
#####################################

# Sprawdzę teraz jak dla 700 iteracji poradzi sobie 100 wywołań wyżarzania (dla bety log(log(n))) )
# i porównam z bla bety 1, czyli standardowym algorytmem metropolisa

# Dobrane parametry - najpierw p = 10, potem p = 15 (z wlaczonymi warunkami stopu)
p <- 10
n <- 20

set.seed(1234)

example_goal_function3 <- goal_function_maker(p, n)
actual_permutation <- as.cycle(as.word(c(2:p, 1)))

perm_start <- permutations::id # Wybrana permutacja startowa
n_iter <- 700 # Wybrana liczba iteracji dla pojedynczj bety

SA_log_log_perm = list()
SA_log_log_results = numeric(0)
ma_perm = list()
ma_results = numeric(0)

liczba_powtorzen <- 100
sredni_czas_iteracji <- NULL

for(i in 1:liczba_powtorzen) { # MiNI 54 minuty
  start_ta_iteracja <- Sys.time()
  
  print(paste0("Symulowane wyżarzanie: próba nr ", i))
  
  if(i > 1){
    print(paste0("Estymowany pozostaly czas to ",
                 (liczba_powtorzen - i + 1) * sredni_czas_iteracji,
                 " ", attr(sredni_czas_iteracji, "units")))
  }
  
  sa <- symulated_anneling(example_goal_function3, start= perm_start, p=p, 
                           beta=log(log(3:100)), 
                           number_of_iterations = n_iter)
  
  SA_log_log_perm[[i]] <- sa[["found_point"]]
  SA_log_log_results[i] <- example_goal_function3(SA_log_log_perm[[i]])
  
  mh <- MH(attr(example_goal_function3, "U"), n_number = n,
           max_iter = length(sa[["goal_function_logvalues"]]))
  ma_perm[[i]] <- mh[["found_point"]]
  ma_results[i] <- mh[["found_point_function_logvalue"]]
  
  end_ta_iteracja <- Sys.time()
  if(i == 1){
    sredni_czas_iteracji <- end_ta_iteracja - start_ta_iteracja
  }else{
    sredni_czas_iteracji <- (sredni_czas_iteracji * (i-1) + end_ta_iteracja - start_ta_iteracja) / i
  }
}

#save(ma_results, SA_log_log_results, file="data/porownanie_log_log_MH_p_10_n_100.RData")
#load("data/porownanie_log_log_MH_p_10_n_100.RData")

P_mh <- ecdf(ma_results)
plot(P_mh, col="red")

P_SA <- ecdf(SA_log_log_results)
lines(P_SA, col="green")
lines(x=c(example_goal_function3(actual_permutation),
          example_goal_function3(actual_permutation)),
      y=c(0,1), type = "l", lty=3)

legend("topleft", col=c("red", "green"), lty = c(1,1), cex = 1.2, inset=0.002,
       legend = c("results of MH optimization", "results of SA optimization"))

wilcox.test(ma_results, SA_log_log_results) # p-val = 1.6 * 10^(-9)





# Stary kod generujący obrazek:
plot(SA_log_log_results, main = "100 prób symulowanego wyżarzania dla ciągu log(log(n))" , xlab = "Numer próby", ylab = "Osiągnięta wartość funkcji celu",
     ylim = c(example_goal_function3(perm_start) - 10,example_goal_function3(actual_permutation) + 10), font.lab=2, font = 2, type = "b", lwd=2)
axis(side=1, at=1:100, labels = FALSE, font = 2)
axis(side=2, at=c(-10,0,10,20,30,40,50,60), labels = TRUE, font = 2)
lines(replicate(100,example_goal_function3(perm_start)), col = "green",lwd = 2, type="l") # startowa
lines(replicate(100,example_goal_function3(actual_permutation)), col = "aquamarine3",lwd = 2, type="l") # max
text(x = 50, y = example_goal_function3(perm_start) - 5, "Permutacja startowa", font = 2)
text(x = 50, y = example_goal_function3(actual_permutation) + 5, "Kandydat na max", lwd = 3, font = 2)

mean(wartosc5)


# log(log(n)) osiaga lepsze rezultaty niz 1
