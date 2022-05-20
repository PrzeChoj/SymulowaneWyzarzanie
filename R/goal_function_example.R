#install.packages("permutations")
#install.packages("devtools")
#devtools::install_github("PrzeChoj/gips")  # update 2022.05.20 16:30

source("R/Wyzarzanie_algorytm.R")

# trzeba trzymac wielkosc tablicy
perm <- as.cycle(as.word(c(1,4,5,2,3,6))) # (1)(3,5)(2,4)(6)
permutations::fixed(perm)  # pamięta o 1. Nie pamięta o 6
length(permutations::fixed(perm))  # 5


# przyklad z `goal_function_maker`:
p <- 10
n <- 100

example_goal_function <- goal_function_maker(p, n)

actual_permutation <- as.cycle(as.word(c(2:p, 1)))

example_goal_function(permutations::id)   # to jest malo
example_goal_function(actual_permutation) # tego szukamy. To jest max funkcji celu
example_goal_function(runif_transposition(p))


# porownanie podstawowego MH i symulowanego wyzarzania:
number_of_iterations <- 100
beta <- log(c(2:4))

sa <- symulated_anneling(example_goal_function, p=p, beta=beta,
                         number_of_iterations = number_of_iterations)
print(sa)
plot(sa)
perm_found_SA <- sa[["found_point"]]

mh <- MH(U = attr(example_goal_function, "U"), n_number = n,
                    max_iter = length(sa[["goal_function_logvalues"]]),
                    start = permutations::id)
print(mh)
plot(mh)
perm_found_MH <- mh[["found_point"]]




beta <- log(2:5)
number_of_iterations <- 10 * 2^(1:4)
M <- 30

list_of_lists_of_log_values <- get_list_of_lists_of_log_values_num_of_it(example_goal_function, p, beta, number_of_iterations, M)

plot_epdf(values_list = list_of_lists_of_log_values,
          min_val = example_goal_function(permutations::id),
          max_val = example_goal_function(actual_permutation))








