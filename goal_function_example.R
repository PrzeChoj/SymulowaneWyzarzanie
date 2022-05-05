install.packages("devtools")
library(devtools)

devtools::install_github("PrzeChoj/gips")
library(gips)

c <- permutations::as.cycle(permutations::as.word(c(2,1)))
U1 <- matrix(c(1,0.5,0.5,2), nrow=2,byrow = TRUE)
goal_function(c, 2, 100, U1)
goal_function(permutations::id, 2, 100, U1)
