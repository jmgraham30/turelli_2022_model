library(tidyverse)
library(gamlss.inf)

?BEINF0

mu_sample <- tibble(x = rBEINF0(1000, mu = 0.05, sigma = 0.1, nu = 0.95)) 

mu_sample |> 
  ggplot(aes(x)) +
  geom_histogram(binwidth=0.01,fill = "black", color = "white") +
  theme_bw() + 
  xlim(c(-0.05,1.05))

summary(mu_sample)
