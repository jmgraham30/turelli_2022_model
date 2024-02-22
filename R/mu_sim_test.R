library(ggplot2)
library(tibble)

theme_set(theme_minimal(base_size = 13))
update_geom_defaults("point", list(color = "midnightblue", alpha = 0.8))

source("R/mu_model.R")

p_0 <- 0.4
F_val_m <- 1.2

sh_val <- 0.2

N <- 1000


p_val <- p_0
p_val <- mu_iteration(p_val,F_val_m,
                      sh_val,N)
p_val

mu_simulation(p_0,F_val_m,
              sh_val,N)

p <- mu_simulation_p(p_0,F_val_m,
                     sh_val,N)

tibble(p=p) |>
  ggplot(aes(x=1:length(p),y=p)) +
  geom_point() +
  ylim(c(0,1)) + 
  labs(x = "Generation", y = "Frequency of infection")

