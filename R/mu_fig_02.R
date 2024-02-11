library(tibble)
library(tidyr)
library(furrr)
library(readr)

plan(multisession, workers = 6)

source("R/mu_model.R")

p_0 <- 0.4

N <- 1000

sh_range <- seq(0.0,0.25,by=0.05)
F_range <- seq(0.8,1.2,by=0.05)
mu_range <- seq(0.01,0.1,by=0.01)
reps <- 1:25

rep_df <- expand_grid(x_sh=sh_range,x_F=F_range,x_mu=mu_range,rep_ind=reps)

f_1 <- function(x_sh,x_F,x_mu,rep_ind,...){
  
  t_f <- mu_simulation(p_0=p_0,F_val_m=x_F,mu_m=x_mu,
                       sh_val=x_sh,N=N)
  
  return(tibble(F_val_m=x_F,mu_m=x_mu,sh_val=x_sh,repeat_num=rep_ind,t_f=t_f))
  
}


res_1 <- rep_df |>
  future_pmap_dfr(f_1,
                  .options = furrr_options(seed=TRUE))

write_csv(res_1,"model_results/mu_fig_02_data.csv")




