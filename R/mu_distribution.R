library(tidyverse)
library(gamlss.inf)

?BEINF

mu_sample <- tibble(x = rBEINF(1000, mu = 0.2, sigma = 0.9, nu = 0.9)) |>
  mutate(x = round(x,2))

mu_sample |> 
  ggplot(aes(x)) +
  geom_histogram(binwidth=0.01,fill = "black", color = "white") +
  theme_bw() + 
  xlim(c(-0.2,1.1))

summary(mu_sample)

mu_sample_beta <- tibble(x = rbeta(1000,shape1 = 0.05,shape2 = 0.9)) |>
  mutate(x = round(x,2))

mu_sample_beta |> 
  ggplot(aes(x)) +
  geom_histogram(binwidth=0.01,fill = "black", color = "white") +
  theme_bw() + 
  xlim(c(-0.2,1.1))

summary(mu_sample_beta)


