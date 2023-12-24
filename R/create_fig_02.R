library(tidyverse)
library(ggthemes)
library(scales)
library(latex2exp)

theme_set(theme_minimal(base_size=13))

# Read in data
df_1 <- read_csv("model_results/fig_02_data_1.csv")
df_2 <- read_csv("model_results/fig_02_data_2.csv")

glimpse(df_1)
glimpse(df_2)


df_1_s <- df_1 |>
  group_by(sh_val) |>
  summarise(mean = mean(t_f), n = n()) |>
  mutate(F_val = "1.021")

df_2_s <- df_2 |>
  group_by(sh_val) |>
  summarise(mean = mean(t_f), n = n())  |>
  mutate(F_val = "1.05")

df_12 <- rbind(df_1_s,df_2_s)

glimpse(df_12)


df_12 %>% 
  ggplot(aes(x=sh_val, y = mean,color=F_val)) +
  geom_point() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_color_colorblind() +
  labs(x = TeX("$s_{h}$"), y = TeX("$\\hat{E}(T_{Loss})$"), color=TeX("$Median_{F}$"))
