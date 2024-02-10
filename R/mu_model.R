library(gamlss.inf)

infection_frequency_model <- function(p_t,F_val,mu_val,sh_val){
  
  num_exp <- p_t*F_val*(1-mu_val)
  den_exp <- 1 + p_t*(F_val - 1 - sh_val) + (p_t^2)*sh_val*(1 - mu_val*F_val)
  
  p_t1 <- num_exp/den_exp
  
  return(max(0.0,min(p_t1,1.0)))
  
}


mu_iteration <- function(p_val,F_val_m,
                              sh_val,N){
  
  mu_val_r = rBEINF0(1, mu = 0.5, sigma = 0.75, nu = 0.95)
  p_star <- infection_frequency_model(p_val, F_val_m, mu_val_r, sh_val)
  
  return(rbinom(1, N, p_star) / N )
  
}

mu_simulation <- function(p_0,F_val_m,
                               sh_val,N,
                               max_iter=10^10, thresh = 10^(-8)){
  
  gen_i <- 0
  
  while (gen_i < max_iter & p_0 > thresh){
    p_0 <- mu_iteration(p_0,F_val_m=F_val_m,
                             sh_val=sh_val,N=N)
    gen_i <- gen_i + 1
  }
  
  return(gen_i)
  
}

mu_simulation_p <- function(p_0,F_val_m,
                                 sh_val,N,
                                 max_iter=10^10, thresh = 10^(-8)){
  
  gen_i <- 1
  p <- c(p_0)
  
  while (gen_i < max_iter & p_0 > thresh){
    p_0 <- mu_iteration(p_0,F_val_m=F_val_m,
                             sh_val=sh_val,N=N)
    gen_i <- gen_i + 1
    p[gen_i] <- p_0 
  }
  
  return(p)
  
}