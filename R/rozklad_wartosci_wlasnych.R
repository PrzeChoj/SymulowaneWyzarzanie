source("R/Wyzarzanie_algorytm.R")

library(ggplot2)
library(dplyr)
library(tidyr)


get_values <- function(p, n, sigma_matrix, M){
  start_time <- Sys.time()
  
  # czy wartosci wlasne beda podobniejsze do oryginalnych?
  eigen_U <- numeric(0)
  eigen_U_bg <- numeric(0)
  eigen_U_mh <- numeric(0)
  
  # czy norma frobeniusa bedzie podobniejsza od oryginalnej?
  frob_norm_U <- numeric(0)
  frob_norm_U_bg <- numeric(0)
  frob_norm_U_mh <- numeric(0)
  
  # czy norma frobeniusa bledu bedzie mniejsza dla poprawionych?
  frob_norm_est <- numeric(0)
  frob_norm_est_bg <- numeric(0)
  frob_norm_est_mh <- numeric(0)
  
  f_val_bg <- numeric(0)
  f_val_mh <- numeric(0)
  
  progressBar <- utils::txtProgressBar(min = 0, max = 2*M, initial = 1)
  for(i in 1:M){
    utils::setTxtProgressBar(progressBar, i*2-1)
    
    example_goal_function <- goal_function_maker(p, n, sigma_matrix)
    U <- attr(example_goal_function, "U")
    
    frob_norm_est[i] <- norm(U/n - sigma_matrix, "F")
    frob_norm_U[i] <- norm(U/n, "F")
    eigen_U <- c(eigen_U, eigen(U/n)$values)
    
    bg <- gips::best_growth(U, n, show_progress_bar = FALSE, max_iter = Inf)
    U_bg <- gips::project_matrix(U, bg$found_point, perm_size = p)
    f_val_bg[i] <- example_goal_function(bg$found_point)
    
    frob_norm_est_bg[i] <- norm(U_bg/n - sigma_matrix, "F")
    frob_norm_U_bg[i] <- norm(U_bg/n, "F")
    eigen_U_bg <- c(eigen_U_bg, eigen(U_bg/n)$values)
    
    utils::setTxtProgressBar(progressBar, i*2)
    
    mh <- gips::MH(U, n, bg$iterations_performed * choose(p, 2), show_progress_bar = FALSE)
    U_mh <- gips::project_matrix(U, mh$found_point, perm_size = p)
    f_val_mh[i] <- example_goal_function(mh$found_point)
    
    frob_norm_est_mh[i] <- norm(U_mh/n - sigma_matrix, "F")
    frob_norm_U_mh[i] <- norm(U_mh/n, "F")
    eigen_U_mh <- c(eigen_U_mh, eigen(U_mh/n)$values)
  }
  close(progressBar)
  
  end_time <- Sys.time()
  print(end_time - start_time)
  
  list("p" = p,
       "n" = n,
       "sigma_matrix" = sigma_matrix,
       "eigen_U" = eigen_U,
       "eigen_U_bg" = eigen_U_bg,
       "eigen_U_mh" = eigen_U_mh,
       "frob_norm_U" = frob_norm_U,
       "frob_norm_U_bg" = frob_norm_U_bg,
       "frob_norm_U_mh" = frob_norm_U_mh,
       "frob_norm_est" = frob_norm_est,
       "frob_norm_est_bg" = frob_norm_est_bg,
       "frob_norm_est_mh" = frob_norm_est_mh,
       "f_val_bg" = f_val_bg,
       "f_val_mh" = f_val_mh
  )
}

# TODO list:
# 1. Rozklady w ostatnim plocie porownaj tez testem na rownosc rozkladu, a nie tylko median
# 2. Dodac tytuly i opisy osi wykresow
# 3. Zapisac wykres ecdf (to nie ggplot)
# 4. Jak bedzie czas: v3: inna macierz sigma_matrix

set.seed(1234)

#v1 <- get_values(p=10, n=20, sigma_matrix=diag(10), M=100) # MiNI 10 minut
#save(v1, file="data/eigen_v1.RData") # UWAGA! nie nadpisac!
#load("data/eigen_v1.RData")

#v2 <- get_values(p=25, n=20, sigma_matrix=diag(25), M=50) # MiNI 90
#save(v2, file="data/eigen_v2.RData") # UWAGA! nie nadpisac!
#load("data/eigen_v2.RData")



# NOTE: W wilcox.test: H_0 = (mediany rozkladow sa takie same); H_1 = (mediany rozkladow sa inne)
# NOTE: dla sigma_matrix = diag(p), plot_frob_est bedzie przesunietym plot_frob, bo od kazdego odejmujemy troche.Interpretacja jest jednak troche inna


v <- v1


# plots:

plot_eigen <- data.frame("eigen_U" = v$eigen_U,
                         "eigen_U_bg" = v$eigen_U_bg,
                         "eigen_U_mh" = v$eigen_U_mh) %>% 
  pivot_longer(cols = c("eigen_U", "eigen_U_bg", "eigen_U_mh"),
               names_to = "matrix_type", values_to = "eigen_value") %>% 
  ggplot(aes(x=matrix_type, y=eigen_value)) + 
  geom_violin() +
  geom_hline(yintercept = 1, linetype="dashed", color = "red")
plot_eigen
#ggsave("./plots/plot_eigen_v?.png", plot_eigen)

wilcox.test(v$eigen_U, v$eigen_U_bg)    # p_val: v1 0.0001;        v2 2.2*10^(-16)
wilcox.test(v$eigen_U, v$eigen_U_mh)    # p_val: v1 1.5 * 10^(-5); v2 2.2*10^(-16)
wilcox.test(v$eigen_U_bg, v$eigen_U_mh) # p_val: v1 0.9763;        v2 0.7018       <<<--- takie same mediany


plot_frob <- data.frame("frob_norm_U" = v$frob_norm_U,
                        "frob_norm_U_bg" = v$frob_norm_U_bg,
                        "frob_norm_U_mh" = v$frob_norm_U_mh) %>% 
  pivot_longer(cols = c("frob_norm_U", "frob_norm_U_bg", "frob_norm_U_mh"),
               names_to = "matrix_type", values_to = "eigen_value") %>% 
  ggplot(aes(x=matrix_type, y=eigen_value)) + 
  geom_violin() +
  geom_hline(yintercept = norm(v$sigma_matrix, "F"),
             linetype = "dashed", color = "red")
plot_frob
#ggsave("./plots/plot_frob_v?.png", plot_frob)

wilcox.test(v$frob_norm_U, v$frob_norm_U_bg)    # p_val: v1 5*10^(-16);   v2 2.2*10^(-16)
wilcox.test(v$frob_norm_U, v$frob_norm_U_mh)    # p_val: v1 2.2*10^(-16); v2 2.2*10^(-16)
wilcox.test(v$frob_norm_U_bg, v$frob_norm_U_mh) # p_val: v1 0.3088;       v2 0.04521      <<<--- v1: byc moze takie same mediany; v2: odrzucam hipoteze o takich samych medianach, czyli MH lepszy niż BG


plot_frob_est <- data.frame("frob_norm_est" = v$frob_norm_est,
                            "frob_norm_est_bg" = v$frob_norm_est_bg,
                            "frob_norm_est_mh" = v$frob_norm_est_mh) %>% 
  pivot_longer(cols = c("frob_norm_est", "frob_norm_est_bg", "frob_norm_est_mh"),
               names_to = "matrix_type", values_to = "eigen_value") %>% 
  ggplot(aes(x=matrix_type, y=eigen_value)) + 
  geom_violin() +
  geom_hline(yintercept = 0,
             linetype = "dashed", color = "red")
plot_frob_est
#ggsave("./plots/plot_frob_est_v?.png", plot_frob_est)

wilcox.test(v$frob_norm_est, v$frob_norm_est_bg)    # p_val: v1 2.2*10^(-16); v2 2.2*10^(-16)
wilcox.test(v$frob_norm_est, v$frob_norm_est_mh)    # p_val: v1 2.2*10^(-16); v2 2.2*10^(-16)
wilcox.test(v$frob_norm_est_bg, v$frob_norm_est_mh) # p_val: v1 0.01101;      v2 2.5*10^(-6) <<<--- inne mediany; MH leprzy


P_bg <- ecdf(v$f_val_bg)
plot(P_bg, col="red")

P_mh <- ecdf(v$f_val_mh)
lines(P_mh, col="green")

legend("topleft", col=c("red", "green"), lty = c(1,1), cex = 1.2, inset=0.002,
       legend = c("results of BG optimization", "results of MH optimization"))

wilcox.test(v$f_val_bg, v$f_val_mh) # p_val: v1 0.97; v2 0.49 <<<--- takie same mediany funkcji wiarogodnosci






