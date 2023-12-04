# Dependencies ----
library(dplyr)
library(tidyr)
library(ggplot2)

# Set seed ----
set.seed(1337)

# Generate data ----
df <- tibble(
  x = sample(0:100, 1000, replace = TRUE),
  y = sample(0:100, 1000, replace = TRUE),
)

# Describe data ----
df |> 
  summarise(
    x_M = mean(x),
    x_SD = sd(x),
    y_M = sd(y),
    y_SD = sd(y),
  )

# Jitter plot ----
df |> 
  ggplot(aes(x = x, y = y)) +
  geom_jitter(color = "blue") +
  labs(
    x = "Perceived Score",
    y = "Actual Score"
  )

# Dunning-Kruger-Plot ----
df |> 
  mutate(x_quantile = percent_rank(x) %/% .25) |> 
  group_by(x_quantile) |> 
  summarise(
    x = mean(x),
    y = mean(y)
  ) |>
  pivot_longer(-x_quantile) |> 
  mutate(name = factor(name, levels = c("x", "y"), labels = c("actual performance", "perceived performance"))) |> 
  ggplot(aes(x = x_quantile, y = value, shape = name, color = name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Dunning-Kruger-Effect",
    subtitle = "The data was generated, so there is no connection between objective\nand perceived performance score",
    x = "Objective performance quartile",
    y = "Percentile",
    color = "Category",
    shape = "Category"
  )
  