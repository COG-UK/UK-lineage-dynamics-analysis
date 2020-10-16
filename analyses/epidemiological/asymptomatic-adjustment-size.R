library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)

total_intros_18_df <- read.csv("results/estimated-introduction-index-18.csv") %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(total_intros_18 = sum(num_intros))

total_intros_31_df <- read.csv("results/estimated-introduction-index-31.csv") %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(total_intros_31 = sum(num_intros))

total_intros_78_df <- read.csv("results/estimated-introduction-index-78.csv") %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(total_intros_78 = sum(num_intros))

plot_df <- full_join(total_intros_18_df, total_intros_31_df, by = "date")
plot_df <- full_join(plot_df, total_intros_78_df, by = "date")

g <- ggplot(data = plot_df,
            mapping = aes(x = date,
                          ymin = total_intros_18,
                          y = total_intros_31,
                          ymax = total_intros_78)) +
  geom_ribbon(alpha = 0.3) +
  geom_line() +
  theme_classic() +
  theme() +
  labs(x = NULL,
       y = "EII")

ggsave("results/asymptomatic-uncertainty.png", g,
       height = 10, width = 20, units = "cm")
