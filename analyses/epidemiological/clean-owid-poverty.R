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

poverty_df <- read.csv("../../data/epidemiological/share-of-the-population-living-in-extreme-poverty.csv") %>%
  rename(location = Entity,
         year = Year,
         poverty_percentage = Share.of.the.population.living.in.extreme.poverty....) %>%
  select(location,year,poverty_percentage) %>%
  mutate_location_name("Czech Republic", "Czechia") %>%
  mutate_location_name("South Korea", "Korea, South") %>%
  mutate_location_name("United States", "US")


missing_locs <- setdiff(primary_source_locations, poverty_df$location)

poverty_elvidge_df <- read.csv("results/clean-elvidge2009global.csv") %>%
  rename(poverty_percentage = estimated_percentage_in_poverty) %>%
  filter(location %in% missing_locs) %>%
  mutate(year = 2009)

poverty_df <- bind_rows(poverty_df, poverty_elvidge_df) %>%
  group_by(location) %>%
  summarise(latest_poverty_percentage = poverty_percentage[which.max(year)])

other_mean <- poverty_df %>%
  filter(!(location %in% primary_source_locations)) %>%
  use_series("latest_poverty_percentage") %>%
  mean
poverty_other_record <- data.frame(location = "other",
                                   latest_poverty_percentage = other_mean)


poverty_df <- poverty_df %>%
  filter(location %in% primary_source_locations) %>%
  bind_rows(poverty_other_record) %>%
  as.data.frame

cat("clean-owid-poverty.R")
setdiff(primary_source_locations, poverty_df$location)
stopifnot(length(setdiff(primary_source_locations, poverty_df$location))==0)


write.table(x = poverty_df,
            file = "results/clean-worldbankpoverty.csv",
            sep = ",",
            row.names = FALSE)
