library(dplyr)
library(purrr)
library(magrittr)

threshold_date <- as.Date("2020-05-01")
threshold_level <- 0.99

jhu_deaths_df <- read.csv("results/clean-jhu-deaths.csv") %>%
    mutate(date = as.Date(date)) %>%
    filter(location != "United Kingdom")

final_count_df <- jhu_deaths_df %>%
    filter(date == threshold_date) %>%
    rename(final_count = cumulative_deaths)

sorted_final_counts <- sort(final_count_df$final_count, decreasing = TRUE)
cumulative_proportions <- cumsum(sorted_final_counts) / sum(sorted_final_counts)
mask <- cumulative_proportions <= threshold_level
threshold <- min(sorted_final_counts[mask])

primary_source_locations <- final_count_df %>%
    filter(final_count >= threshold) %>%
    use_series("location")

rm(threshold_date)
rm(threshold_level)
rm(final_count_df)
rm(sorted_final_counts)
rm(cumulative_proportions)
rm(mask)
rm(threshold)

## @@ mutate-location-name-defn @@

mutate_location_name <- function(df, old_name, new_name) {
  mask <- df$location == old_name
  df[mask,"location"] <- new_name
  return(df)
}

poverty_df <- read.csv("../../data/epidemiological/elvidge2009global.csv") %>%
  rename(location = country) %>%
  select(location,
         estimated_percentage_in_poverty) %>%
  mutate_location_name("Czech Republic", "Czechia") %>%
  mutate_location_name("South Korea", "Korea, South") %>%
  mutate_location_name("United States", "US") %>%
  mutate_location_name("UAE", "United Arab Emirates")


write.table(x = poverty_df,
            file = "results/clean-elvidge2009global.csv",
            sep = ",",
            row.names = FALSE)
