library(ggplot2)
library(tibble)

theme_set(theme_minimal(base_size = 13))
update_geom_defaults("point", list(color = "midnightblue", alpha = 0.8))

source("R/infection_frequency.R")

p_0 <- 0.4
F_val_m <- 1.021
F_val_CV <- 0.4

mu_val <- 0.02
sh_val <- 0.0

N <- 1000


p_val <- p_0
p_val <- turelli_iteration(p_val,F_val_m,F_val_CV,
                           sh_val,mu_val,N)
p_val

turelli_simulation(p_0,F_val_m,F_val_CV,
                   sh_val,mu_val,N)

p <- turelli_simulation_p(p_0,F_val_m,F_val_CV,
                          sh_val,mu_val,N)

tibble(p=p) |>
  ggplot(aes(x=1:length(p),y=p)) +
  geom_point() +
  ylim(c(0,1)) + 
  labs(x = "Generation", y = "Frequency of infection")

