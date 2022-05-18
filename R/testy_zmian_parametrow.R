source("R/Wyzarzanie_algorytm.R")


# Dobrane parametry
p <- 15
n <- 20

# Od razu goal function jest juz zlogarytmowana

example_goal_function <- goal_function_maker(p, n)

actual_permutation <- as.cycle(as.word(c(2:p, 1))) # permutacja dla ktorej osiagamy maksimum

example_goal_function(permutations::id)   # na id
example_goal_function(runif_transposition(p)) # na losowej
example_goal_function(actual_permutation) # na kandydacie na max

# Testy wyzarzania dla roznych metod schladzania.

perm_found <- symulated_anneling(example_log_goal_function, p=p, beta=1:100, number_of_iterations = 100)
print(paste0("W wyniku symulowanego wyżarzania otrzymano permutację ",
             perm_found, " dla której wartość logarytmu funkcji celu wynosi ",
             example_log_goal_function(perm_found), "."))


# Wyznaczymy srednia wartosc przy 10 probach wyzarzania dla kazdego typu schladzania.


wartosc1 <- numeric()
permutacje_wynik1 <- list()
# Pierwszy typ bet: ciąg b_n = 1/10 + 2n (zaczynamy z temperatury T = 10) 

for(i in 1:10) {
  
  permutacje_wynik1[[i]] <- symulated_anneling(example_goal_function, p=p, 
                                               beta=seq(1/10, 100, 2), number_of_iterations = 100)
  
  wartosc1[i] <- example_goal_function(permutacje_wynik1[[i]])
  
}

mean(wartosc1) # 2.302884
max(wartosc1) # 8.335847




wartosc2 <- numeric()
permutacje_wynik2 <- list()

for(i in 1:10) {
  
  permutacje_wynik2[[i]] <- symulated_anneling(example_goal_function, p=p, 
                                               beta=cumsum(1:100), number_of_iterations = 100)
  
  wartosc2[i] <- example_goal_function(permutacje_wynik2[[i]])
  
}

mean(wartosc2) # 3.342569
max(wartosc2) # 11.63841

# Trzeci typ bet: ciag b_n = 1/100, 1/90, 1/80, ..., 200 - zeby zacząac z wysokiej temperatury

wartosc3 <- numeric()
permutacje_wynik3 <- list()

for(i in 1:10) {
  
  permutacje_wynik3[[i]] <- symulated_anneling(example_goal_function, p=p, 
                                               beta=c(1/100,1/90,1/80,1/70,1/60,1/50,1/40,1/30,1/20,1/10,1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200), 
                                               number_of_iterations = 100)
  
  wartosc3[i] <- example_goal_function(permutacje_wynik3[[i]])
  
}

mean(wartosc3) # 3.220668
max(wartosc3) # 19.20786


# Wykresy obrazujace proby wyzarzania dla roznych bet

plot(wartosc1, xlab = "10 prób symulowanego wyżarzania", ylab = "Osiągnięta wartość funkcji celu",
     ylim = c(-30,40), font.lab=2, font = 2, lwd = 2)
axis(side=1, at=1:10, labels = TRUE, font = 2)

points(wartosc2, col = "red", pch = 2, lwd = 2)
points(wartosc3, col = "blue", pch = 3, lwd = 2)
points(replicate(10,example_goal_function(permutations::id)), col = "green", pch = 9) # startowa

# Wersja liniowa

plot(wartosc1, xlab = "10 prób symulowanego wyżarzania", ylab = "Osiągnięta wartość funkcji celu",
     ylim = c(-30,40), font.lab=2, font = 2, type = "l", lwd=2)
axis(side=1, at=1:10, labels = TRUE, font = 2)

lines(wartosc2, col = "red", lwd=2)
lines(wartosc3, col = "blue", pch = 3, lwd=2)
lines(replicate(10,example_goal_function(permutations::id)), col = "green",lwd = 2) # startowa
# Nie wydaje sie, zeby ktoras ze sprawdzonych dotychczas metod schladzania
# byla istotnie lepsza od pozostalych

# do dodania legenda
# do przetestowania inne dobory bet
# do przetestowania inny punkt startowy
