library(tidyverse)
library(ggthemes)
library(scales)
library(latex2exp)

theme_set(theme_bw(base_size = 13))

mu_model_data <- read_csv("model_results/mu_fig_02_data.csv")

glimpse(mu_model_data)

mu_model_data |>
  ggplot(aes(x = t_f)) + 
  geom_histogram(color="white",fill="steelblue") 

#  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                labels = trans_format("log10", math_format(10^.x)))

mu_model_data_avgs <- mu_model_data |>
  group_by(sh_val,F_val_m) |>
  summarise(t_f_avg = mean(t_f), t_f_sd = sd(t_f), n = n()) 

mu_model_data_avgs |>
  ggplot(aes(x = t_f_avg)) + 
  geom_histogram(color="white",fill="steelblue") 

#  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#             labels = trans_format("log10", math_format(10^.x)))

mu_model_data_avgs |>
  ggplot(aes(x = t_f_sd)) + 
  geom_histogram(color="white",fill="steelblue") 

#  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                labels = trans_format("log10", math_format(10^.x)))

mu_model_data_avgs |>
  ggplot(aes(x = F_val_m, y = sh_val, fill = t_f_avg)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
 # scale_fill_gradient(trans = "log10",low = "lightblue", high = "darkblue",
 #                  breaks = trans_breaks("log10", function(x) 10^x),
 #                  labels = trans_format("log10", math_format(10^.x))) +
  labs(x = TeX("$F$"), y = TeX("$s_h$"), fill = TeX("$\\hat{E}(T_{Loss})$")) 


mu_model_data_avgs |>
  ggplot(aes(x = F_val_m, y = sh_val, fill = t_f_sd)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
#  scale_fill_gradient(trans = "log10",low = "lightblue", high = "darkblue",
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x))) +
  labs(x = TeX("$F$"), y = TeX("$s_h$"), fill = TeX("$\\hat{\\sigma}(T_{Loss})$")) 
