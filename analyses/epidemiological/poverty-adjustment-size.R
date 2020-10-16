library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)

unadjusted_val_df <- read.csv("results/estimated-introduction-index.csv") %>%
  group_by(primary_location) %>%
  summarise(total_intros = sum(num_intros))

adjusted_val_df <- read.csv("results/estimated-introduction-index-poverty-adjusted.csv") %>%
  group_by(primary_location)%>%
  summarise(total_intros = sum(num_intros)) %>%
  rename(total_intros_adjusted = total_intros)


poverty_df <- read.csv("results/clean-worldbankpoverty.csv") %>%
  rename(primary_location = location) %>%
  mutate(poverty_string = ifelse(latest_poverty_percentage > 10,
                                 sprintf("%.1f %%", latest_poverty_percentage),
                                 ""))

plot_df <- full_join(unadjusted_val_df, adjusted_val_df, by = "primary_location")
plot_df <- full_join(plot_df, poverty_df, by = "primary_location")

g <- ggplot(data = plot_df,
            mapping = aes(x = primary_location,
                          y = total_intros_adjusted - total_intros,
                          colour = round(latest_poverty_percentage))) +
  geom_segment(mapping = aes(xend = primary_location,
                             yend = 0)) +
  geom_point() +
  geom_text(mapping = aes(label = poverty_string),
            vjust = -2,
            angle = 0) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -80)) +
  labs(x = NULL,
       y = "Adjustment size",
       colour = "Percentage in\nextreme poverty")


ggsave("results/poverty-adjustment-sizes.png", g, 
       height = 20, width = 40, units = "cm")
