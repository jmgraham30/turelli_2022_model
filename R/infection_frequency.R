infection_frequency_step <- function(p_t,F_val,mu_val,sh_val){
  
  num_exp <- p_t*F_val*(1-mu_val)
  den_exp <- 1 + p_t*(F_val - 1 - sh_val) + (p_t^2)*sh_val*(1 - mu_val*F_val)
  
  p_t1 <- num_exp/den_exp
  
  return(max(0.0,min(p_t1,1.0)))
  
}

turelli_iteration <- function(p_val,F_val_m,F_val_CV,
                              sh_val,mu_val,N){
  
  F_val_r <- rnorm(1, mean=log(F_val_m), sd=sqrt(log(F_val_CV^2 + 1))) |>
    exp()
  p_star <- infection_frequency_step(p_val, F_val_r, mu_val, sh_val)
  
  return(rbinom(1, N, p_star) / N )
  
}


turelli_simulation <- function(p_0,F_val_m,F_val_CV,
                               sh_val,mu_val,N,
                               max_iter=10^10, thresh = 10^(-8)){
  
  gen_i <- 0
  
  while (gen_i < max_iter & p_0 > thresh){
    p_0 <- turelli_iteration(p_0,F_val_m=F_val_m,F_val_CV=F_val_CV,
                             sh_val=sh_val,mu_val=mu_val,N=N)
    gen_i <- gen_i + 1
  }
  
  return(gen_i)
  
}

turelli_simulation_p <- function(p_0,F_val_m,F_val_CV,
                                 sh_val,mu_val,N,
                                 max_iter=10^10, thresh = 10^(-8)){
  
  gen_i <- 1
  p <- c(p_0)
  
  while (gen_i < max_iter & p_0 > thresh){
    p_0 <- turelli_iteration(p_0,F_val_m=F_val_m,F_val_CV=F_val_CV,
                             sh_val=sh_val,mu_val=mu_val,N=N)
    gen_i <- gen_i + 1
    p[gen_i] <- p_0 
  }
  
  return(p)
  
}

