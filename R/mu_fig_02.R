library(tibble)
library(tidyr)
library(furrr)
library(readr)

plan(multisession, workers = 6)

source("R/mu_model.R")

p_0 <- 0.4

N <- 1000

F_val_m_1 <- 1.021
F_val_m_2 <- 1.05

sh_range <- seq(0.0,0.5,by=0.05)
reps <- 1:25

rep_df <- expand_grid(x_sh=sh_range,rep_ind=reps)

f_1 <- function(x_sh,rep_ind,...){
  
  t_f <- mu_simulation(p_0=p_0,F_val_m=F_val_m_1,
                            sh_val=x_sh,N=N)
  
  return(tibble(F_val_m=F_val_m_1,sh_val=x_sh,repeat_num=rep_ind,t_f=t_f))
  
}


res_1 <- rep_df |>
  future_pmap_dfr(f_1,
                  .options = furrr_options(seed=TRUE))

write_csv(res_1,"model_results/mu_fig_02_data_1.csv")

f_2 <- function(x_sh,rep_ind,...){
  
  t_f <- mu_simulation(p_0=p_0,F_val_m=F_val_m_2,
                            sh_val=x_sh,N=N)
  
  return(tibble(F_val_m=F_val_m_2,sh_val=x_sh,repeat_num=rep_ind,t_f=t_f))
  
}


res_2 <- rep_df |>
  future_pmap_dfr(f_2,
                  .options = furrr_options(seed=TRUE))

write_csv(res_2,"model_results/mu_fig_02_data_2.csv")


