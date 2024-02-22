library(tidyverse)
library(gamlss.inf)

?BEINF

mu_sample <- tibble(x = rBEINF(1000, mu = 0.2, sigma = 0.9, nu = 0.9)) |>
  mutate(x = round(x,2))

mu_sample |> 
  ggplot(aes(x)) +
  geom_histogram(binwidth=0.05,fill = "black", color = "white") +
  theme_bw() + 
  xlim(c(-0.1,1.1))

summary(mu_sample)

