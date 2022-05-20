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

# Pierwszy typ bet: ciąg b_n = 1/10 + 2n (zaczynamy z temperatury T = 10) 

wartosc1 <- numeric()
permutacje_wynik1 <- list()

for(i in 1:10) {
  
  permutacje_wynik1[[i]] <- symulated_anneling(example_goal_function, p=p, 
                                               beta=seq(1/10, 100, 2), number_of_iterations = 100)
  
  wartosc1[i] <- example_goal_function(permutacje_wynik1[[i]])
  
}

mean(wartosc1)
max(wartosc1)




wartosc2 <- numeric()
permutacje_wynik2 <- list()

for(i in 1:10) {
  
  permutacje_wynik2[[i]] <- symulated_anneling(example_goal_function, p=p, 
                                               beta=cumsum(1:100), number_of_iterations = 100)
  
  wartosc2[i] <- example_goal_function(permutacje_wynik2[[i]])
  
}

mean(wartosc2)
max(wartosc2)

# Trzeci typ bet: ciag b_n = 1/100, 1/90, 1/80, ..., 200 - zeby zaczac z wysokiej temperatury

wartosc3 <- numeric()
permutacje_wynik3 <- list()

for(i in 1:10) {
  
  permutacje_wynik3[[i]] <- symulated_anneling(example_goal_function, p=p, 
                                               beta=c(1/100,1/90,1/80,1/70,1/60,1/50,1/40,1/30,1/20,1/10,1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200), 
                                               number_of_iterations = 100)
  
  wartosc3[i] <- example_goal_function(permutacje_wynik3[[i]])
  
}

mean(wartosc3)
max(wartosc3)


# Czwarty typ bet: ciag b_n = 1/10, 1/9, 1/8, ..., 10

wartosc4 <- numeric()
permutacje_wynik4 <- list()

for(i in 1:10) {
  
  permutacje_wynik4[[i]] <- symulated_anneling(example_goal_function, p=p, 
                                               beta=c(1/10,1/9,1/8,1/7,1/6,1/5,1/4,1/3,1/2,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), 
                                               number_of_iterations = 100)
  
  wartosc4[i] <- example_goal_function(permutacje_wynik4[[i]])
  
}

mean(wartosc4)
max(wartosc4)



# Piąty typ bet: ciag b_n = 1/512, 1/256, 1/128, 1/64, 1/32, 1/16, 1/8, 1/4, 1/2, 1, 2 , 4, 8, 16, 32, 64, 128, 256, 512

wartosc5 <- numeric()
permutacje_wynik5 <- list()

for(i in 1:10) {
  
  permutacje_wynik5[[i]] <- symulated_anneling(example_goal_function, p=p, 
                                               beta=c(1/512, 1/256, 1/128, 1/64, 1/32, 1/16, 1/8, 1/4, 1/2, 1, 2 , 4, 8, 16, 32, 64, 128, 256, 512), 
                                               number_of_iterations = 100)
  
  wartosc5[i] <- example_goal_function(permutacje_wynik5[[i]])
  
}

mean(wartosc5)
max(wartosc5)



# Szósty typ bet: ciag b_n = log(n)

wartosc6 <- numeric()
permutacje_wynik6 <- list()

for(i in 1:10) {
  
  permutacje_wynik6[[i]] <- symulated_anneling(example_goal_function, p=p, 
                                               beta=c(1/10,1/8,1/6,1/4,1/3,1/2,log(2:100)), 
                                               number_of_iterations = 100)
  
  wartosc6[i] <- example_goal_function(permutacje_wynik6[[i]])
  
}

mean(wartosc6)
max(wartosc6)

# Wykresy obrazujace proby wyzarzania dla roznych bet

plot(wartosc1, main = "10 prób symulowanego wyżarzania dla różnych ciągów temperatur" , xlab = "Numer próby", ylab = "Osiągnięta wartość funkcji celu",
     ylim = c(-30,60), font.lab=2, font = 2, type = "b", lwd=2)
axis(side=1, at=1:10, labels = TRUE, font = 2)
axis(side=2, at=c(-10,0,10,20,30,40,50,60), labels = TRUE, font = 2)

lines(wartosc2, col = "red", lwd=2, type="b")
lines(wartosc3, col = "blue", lwd=2, type="b")
lines(wartosc4, col = "burlywood", lwd = 2, type="b")
lines(wartosc5, col = "chartreuse1", lwd = 2, type="b")
lines(wartosc6, col = "coral", lwd = 2, type="b")
lines(replicate(10,example_goal_function(permutations::id)), col = "green",lwd = 2, type="b") # startowa
lines(replicate(10,example_goal_function(actual_permutation)), col = "aquamarine3",lwd = 2, type="b") # max
text(x = 5.5, y = example_goal_function(permutations::id) - 5, "Permutacja startowa", font = 2)
text(x = 5.5, y = example_goal_function(actual_permutation) + 5, "Kandydat na max", lwd = 3, font = 2)
legend("topright", legend=c("Bety 1", "Bety 2", "Bety 3", "Bety 4", "Bety 5", "Bety 6"),
       col=c("black", "red", "blue", "burlywood", "chartreuse1", "coral"), 
       lty = 1, lwd = 2, cex=0.8, text.font = 2)

# Nie wydaje sie, zeby ktoras ze sprawdzonych metod schladzania
# byla istotnie lepsza od pozostalych


# Sprawdźmy czy coś zmieni inny dobór punktu startowego.

# Niech to teraz będzie (2,6,4,9,3)(7,12)

perm_start <- as.cycle(as.word(c(1,6,2,9,5,4,12,8,3,10,11,7,13,14,15,16)))
example_goal_function(actual_permutation)

for(i in 1:10) {
  
  permutacje_wynik1[[i]] <- symulated_anneling(example_goal_function, start = perm_start, p=p, 
                                               beta=seq(1/10, 100, 2), number_of_iterations = 100)
  wartosc1[i] <- example_goal_function(permutacje_wynik1[[i]])
  
  permutacje_wynik2[[i]] <- symulated_anneling(example_goal_function, start = perm_start, p=p, 
                                               beta=cumsum(1:100), number_of_iterations = 100)
  wartosc2[i] <- example_goal_function(permutacje_wynik2[[i]])
  
  permutacje_wynik3[[i]] <- symulated_anneling(example_goal_function, start = perm_start, p=p, 
                                               beta=c(1/100,1/90,1/80,1/70,1/60,1/50,1/40,1/30,1/20,1/10,1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200), 
                                               number_of_iterations = 100)
  wartosc3[i] <- example_goal_function(permutacje_wynik3[[i]])
  
  permutacje_wynik4[[i]] <- symulated_anneling(example_goal_function, start= perm_start, p=p, 
                                               beta=c(1/10,1/9,1/8,1/7,1/6,1/5,1/4,1/3,1/2,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), 
                                               number_of_iterations = 100)
  wartosc4[i] <- example_goal_function(permutacje_wynik4[[i]])
  
  permutacje_wynik5[[i]] <- symulated_anneling(example_goal_function, start= perm_start, p=p, 
                                               beta=c(1/512, 1/256, 1/128, 1/64, 1/32, 1/16, 1/8, 1/4, 1/2, 1, 2 , 4, 8, 16, 32, 64, 128, 256, 512), 
                                               number_of_iterations = 100)
  wartosc5[i] <- example_goal_function(permutacje_wynik5[[i]])
  
  permutacje_wynik6[[i]] <- symulated_anneling(example_goal_function,start= perm_start, p=p, 
                                               beta=c(1/10,1/8,1/6,1/4,1/3,1/2,log(2:100)), 
                                               number_of_iterations = 100)
  wartosc6[i] <- example_goal_function(permutacje_wynik6[[i]])
  
}

# Wykresy raz jeszcze

plot(wartosc1, main = "10 prób symulowanego wyżarzania dla różnych ciągów temperatur" , xlab = "Numer próby", ylab = "Osiągnięta wartość funkcji celu",
     ylim = c(-30,60), font.lab=2, font = 2, type = "b", lwd=2)
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
       lty = 1, lwd = 2, cex=0.8, horiz=TRUE,text.font = 2)

# Wciaz ladujemy dosyc daleko od maksimum
