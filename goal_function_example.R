install.packages("permutations")
library(permutations)

install.packages("devtools")
library(devtools)

devtools::install_github("PrzeChoj/gips")
library(gips)

c <- as.cycle(as.word(c(2,1)))
U1 <- matrix(c(1,0.5,0.5,2), nrow=2,byrow = TRUE)
goal_function(c, 2, 100, U1)
goal_function(permutations::id, 2, 100, U1)  # `id` i tak trzeba wołać przez `permutations::id`


perm <- as.cycle(as.word(c(1,4,5,2,3,6))) # (1)(3,5)(2,4)(6)
str(perm) # pamięta o 1, ale jej nie wyświetla. Nie pamięta o 6
