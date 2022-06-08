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

set.seed(1234)

#v1 <- get_values(p=10, n=20, sigma_matrix=diag(10), M=100) # MiNI 10 minut
#save(v1, file="data/eigen_v1.RData") # UWAGA! nie nadpisac!

#v2 <- get_values(p=25, n=20, sigma_matrix=diag(25), M=50) # MiNI 90 minut
#save(v2, file="data/eigen_v2.RData") # UWAGA! nie nadpisac!


#p_v3 <- 10
#n_v3 <- 20
#sigma_matrix_v3 <- matrix(numeric(p_v3*p_v3), nrow=p_v3)
#for(i in 1:p_v3){
#  for(j in 1:p_v3){
#    sigma_matrix_v3[i,j] <- 1 - min(abs(i-j), p_v3-abs(i-j)) / p_v3
#  }
#  sigma_matrix_v3[i,i] <- 1 + 1/p_v3
#}
#heatmap(sigma_matrix_v3, Colv = NA, Rowv = NA)
#v3 <- get_values(p=p_v3, n=n_v3, sigma_matrix=sigma_matrix_v3, M=200) # PC 10.5 minut ?
#save(v3, file="data/eigen_v3.RData") # UWAGA! nie nadpisac!


#p_half_v4 <- 5
#p_v4 <- p_half_v4 * 2
#n_v4 <- 20
#sigma_matrix_v4 <- matrix(numeric(p_v4*p_v4), nrow=p_v4)
#for(i in 1:p_half_v4){
#  for(j in 1:p_half_v4){
#    sigma_matrix_v4[i,j] <- 1 - min(abs(i-j), p_half_v4-abs(i-j)) / p_v4
#    sigma_matrix_v4[i+p_half_v4,j+p_half_v4] <- 1 - min(abs(i-j), p_half_v4-abs(i-j)) / p_v4
#    
#    sigma_matrix_v4[i,j+p_half_v4] <- 1 - min(abs(i-j), p_half_v4-abs(i-j)) / p_v4 / 1.5
#    sigma_matrix_v4[i+p_half_v4,j] <- 1 - min(abs(i-j), p_half_v4-abs(i-j)) / p_v4 / 1.5
#  }
#  sigma_matrix_v4[i,i] <- 1 + 4/p_v4
#  sigma_matrix_v4[i+p_half_v4,i+p_half_v4] <- 1 + 2/p_v4
#}
#heatmap(sigma_matrix_v4, Colv = NA, Rowv = NA) # invariant względem (1,2,3,4,5)(6,7,8,9,10)
#v4 <- get_values(p=p_v4, n=n_v4, sigma_matrix=sigma_matrix_v4, M=200) # PC 12 minut
#save(v4, file="data/eigen_v4.RData") # UWAGA! nie nadpisac!



# TODO:
  # v5 - ciekawe; prawie zawsze znajduje podzial {1,2,3,4,5}, {6,7,8,9,10}

#p_half_v5 <- 5
#p_v5 <- p_half_v5 * 2
#n_v5 <- 20
#sigma_matrix_v5 <- matrix(numeric(p_v5*p_v5), nrow=p_v5)
#for(i in 1:p_half_v5){
#  for(j in 1:p_half_v5){
#    sigma_matrix_v5[i,j] <- 1 - min(abs(i-j), p_half_v5-abs(i-j)) / p_v5
#    sigma_matrix_v5[i+p_half_v5,j+p_half_v5] <- 1 - min(abs(i-j), p_half_v5-abs(i-j)) / p_v5
#    
#    sigma_matrix_v5[i,j+p_half_v5] <- - (1 - min(abs(i-j), p_half_v5-abs(i-j)) / p_v5 / 1.5)
#    sigma_matrix_v5[i+p_half_v5,j] <- - (1 - min(abs(i-j), p_half_v5-abs(i-j)) / p_v5 / 1.5)
#  }
#  sigma_matrix_v5[i,i] <- 1 + 4/p_v5
#  sigma_matrix_v5[i+p_half_v5,i+p_half_v5] <- 1 + 2/p_v5
#}
#heatmap(sigma_matrix_v5, Colv = NA, Rowv = NA) # invariant względem (1,2,3,4,5)(6,7,8,9,10)


load("data/eigen_v1.RData")
load("data/eigen_v2.RData")
load("data/eigen_v3.RData")
load("data/eigen_v4.RData")

# NOTE: W wilcox.test: H_0 = (mediany rozkladow sa takie same); H_1 = (mediany rozkladow sa inne)
# NOTE: dla sigma_matrix = diag(p), plot_frob_est bedzie przesunietym plot_frob, bo od kazdego odejmujemy troche.Interpretacja jest jednak troche inna

plot_experiment <- "v3" # narazie jest v1 i v2 i raczej tak zostanie xd

if(plot_experiment == "v1"){
  v <- v1
  p <- 10
  n <- 20
}else if(plot_experiment == "v2"){
  v <- v2
  p <- 25
  n <- 20
}else if(plot_experiment == "v3"){
  v <- v3
  p <- 10
  n <- 20
}else if(plot_experiment == "v4"){
  v <- v4
  p <- 10
  n <- 20
}else{
  print(paste0("Zla wartosc plot_experiment = ", plot_experiment))
}


# plots:

plot_eigen <- data.frame("eigen_U" = v$eigen_U,
                         "eigen_U_bg" = v$eigen_U_bg,
                         "eigen_U_mh" = v$eigen_U_mh) %>% 
  pivot_longer(cols = c("eigen_U", "eigen_U_bg", "eigen_U_mh"),
               names_to = "matrix_type", values_to = "eigen_value") %>% 
  ggplot(aes(x=matrix_type, y=eigen_value, fill=matrix_type)) + 
  geom_violin() +
  geom_hline(yintercept = 1, linetype="dashed", color = "red") +
  labs(title="Rozkład wartości własnych estymatora macierzy kowariancji",
       subtitle = paste0("na podstawie próbki wielkosci n=", n,
                         " z rozkładu normalnego N(0, I) wymiaru p=", p),
       x = "Metoda poprawiania estymatora", y = "Wartości własne") +
  scale_x_discrete(labels=c("Zwykly estymator U",
                            "U poprawione algorytmem BG",
                            "U poprawione algorytmem MH")) +
  theme(
    plot.title = element_text(size=20, face="bold"),
    plot.subtitle = element_text(size=16, face="bold"),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size=16, face="bold"),
    axis.text.x = element_text(face="bold", size=14),
    axis.text.y = element_text(face="bold", size=14)
  ) +
  scale_y_continuous(breaks = 0:5) +
  geom_boxplot(width=0.1, fill='#FFFFFF') +
  theme(legend.position='none')
plot_eigen
#ggsave(paste0("./plots/rozklad_wartosci_wlasnych/plot_eigen_", plot_experiment, ".png"), plot_eigen, width = 10, height = 6)

wilcox.test(v$eigen_U, v$eigen_U_bg)    # p_val: v1 0.0001;        v2 2.2*10^(-16)
wilcox.test(v$eigen_U, v$eigen_U_mh)    # p_val: v1 1.5 * 10^(-5); v2 2.2*10^(-16)
wilcox.test(v$eigen_U_bg, v$eigen_U_mh) # p_val: v1 0.9763;        v2 0.7018       <<<--- takie same mediany


plot_frob_est <- data.frame("frob_norm_est" = v$frob_norm_est,
                            "frob_norm_est_bg" = v$frob_norm_est_bg,
                            "frob_norm_est_mh" = v$frob_norm_est_mh) %>% 
  pivot_longer(cols = c("frob_norm_est", "frob_norm_est_bg", "frob_norm_est_mh"),
               names_to = "matrix_type", values_to = "eigen_value") %>% 
  ggplot(aes(x=matrix_type, y=eigen_value, fill=matrix_type)) + 
  geom_violin() +
  geom_hline(yintercept = 0,
             linetype = "dashed", color = "red") +
  geom_boxplot(width=0.1, fill='#FFFFFF') +
  labs(title="Norma Frobeniusa błędu estymatora macierzy kowariancji",
       subtitle = paste0("na podstawie próbki wielkosci n=", n,
                         " z rozkładu normalnego N(0, I) wymiaru p=", p),
       x = "Metoda poprawiania estymatora",
       y = "Norma Frobeniusa błędu estymacji, czyli (U - I)") +
  theme(
    plot.title = element_text(size=20, face="bold"),
    plot.subtitle = element_text(size=16, face="bold"),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size=16, face="bold"),
    axis.text.x = element_text(face="bold", size=14),
    axis.text.y = element_text(face="bold", size=14)
  ) +
  scale_x_discrete(labels=c("Zwykly estymator U",
                            "U poprawione algorytmem BG",
                            "U poprawione algorytmem MH")) +
  theme(legend.position="none")
plot_frob_est
#ggsave(paste0("./plots/rozklad_wartosci_wlasnych/plot_frob_est_", plot_experiment, ".png"), plot_frob_est, width = 10, height = 6)

wilcox.test(v$frob_norm_est, v$frob_norm_est_bg)    # p_val: v1 2.2*10^(-16); v2 2.2*10^(-16)
t.test(v$frob_norm_est, v$frob_norm_est_bg)
wilcox.test(v$frob_norm_est, v$frob_norm_est_mh)    # p_val: v1 2.2*10^(-16); v2 2.2*10^(-16)
t.test(v$frob_norm_est, v$frob_norm_est_mh)
wilcox.test(v$frob_norm_est_bg, v$frob_norm_est_mh) # p_val: v1 0.01101;      v2 2.5*10^(-6) <<<--- inne mediany; MH leprzy
t.test(v$frob_norm_est_bg, v$frob_norm_est_mh)      # p_val: v1 0.00839;      v2 2*10^(-6)   <<<--- inne średnie; MH leprzy


plot_ECDF_opt <- data.frame(BG = v$f_val_bg,
                            MH = v$f_val_mh) %>% 
  pivot_longer(cols = c("BG", "MH"),
               names_to = "alg_type",
               values_to = "found_max") %>% 
  ggplot(aes(found_max, col=alg_type)) +
  stat_ecdf(geom = "step", size=2) +
  labs(title="ECDF rokładu znalezionej największej wartości",
       subtitle = paste0("na podstawie próbki wielkosci n=", n,
                         " z rozkładu normalnego N(0, I) wymiaru p=", p),
       x = "Logarytm znalezionej wartości funkcji wiarogodności",
       y = "Skumulowane estymowane prawdopodobieństwo",
       col = "Użyty algorytm") +
  theme(
    plot.title = element_text(size=20, face="bold"),
    plot.subtitle = element_text(size=16, face="bold"),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size=16, face="bold"),
    axis.text.x = element_text(face="bold", size=14),
    axis.text.y = element_text(face="bold", size=14),
    legend.text = element_text(face="bold", size=14)
  )
plot_ECDF_opt
#ggsave(paste0("./plots/rozklad_wartosci_wlasnych/plot_ECDF_opt_", plot_experiment, ".png"), plot_ECDF_opt, width = 10, height = 6)

wilcox.test(v$f_val_bg, v$f_val_mh) # p_val: v1 0.97; v2 0.49 <<<--- niema powodu podejrzewac, ze mediany log funkcji wiarogodnosci sa inne
t.test(v$f_val_bg, v$f_val_mh)      # p_val: v1 0.89; v2 0.49 <<<--- niema powodu podejrzewac, ze średnie log funkcji wiarogodnosci sa inne
ks.test(v$f_val_bg, v$f_val_mh)     # p_val: v1 1   ; v2 0.55 <<<--- niema powodu podejrzewac, ze rozklady log funkcji wiarogodnosci sa inne

