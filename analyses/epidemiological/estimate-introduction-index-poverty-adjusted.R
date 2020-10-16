library(dplyr)
library(magrittr)
library(purrr)

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

prop_potential_seeders <- read.csv("results/estimated-proportion-seeders-poverty-adjusted.csv",
                                   stringsAsFactors = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    dplyr::select(date, location, seeder_proportion)

stopifnot(!any(is.na(prop_potential_seeders$seeder_proportion)))
stopifnot(all(intersect(primary_source_locations, prop_potential_seeders$location) == primary_source_locations))

estimated_arrivals <- read.csv("results/estimated-arrivals.csv") %>%
    mutate(date = as.Date(date)) %>%
    rename(num_arrivals = estimate) %>%
    dplyr::select(date, location, num_arrivals)

## @@ mutate-location-name-defn @@

mutate_location_name <- function(df, old_name, new_name) {
  mask <- df$location == old_name
  df[mask,"location"] <- new_name
  return(df)
}

estimated_arrivals <- mutate_location_name(estimated_arrivals, "Czech Republic", "Czechia")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "United States", "US")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Dominican Rep", "Dominican Republic")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Korea (South)", "Korea, South")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Antigua-Barbuda", "Antigua and Barbuda")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Bosnia-Herzegovina", "Bosnia and Herzegovina")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Brunei Darussalam", "Brunei")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Cape Verde", "Cabo Verde")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Central African Rep", "Central African Republic")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Cote D'Ivoire", "Cote d'Ivoire")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "St Kitts-Nevis", "Saint Kitts and Nevis")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "St Lucia", "Saint Lucia")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "St Vincent-Grenad", "Saint Vincent and the Grenadines")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Taiwan", "Taiwan*")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Trinidad-Tobago", "Trinidad and Tobago")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Viet Nam", "Vietnam")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Timor-leste", "Timor-Leste")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Sao Tome-Principe", "Sao Tome and Principe")
estimated_arrivals <- mutate_location_name(estimated_arrivals, "Macedonia", "North Macedonia")

estimated_arrivals <- filter(estimated_arrivals, location != "Aruba")
estimated_arrivals <- filter(estimated_arrivals, location != "Bermuda")
estimated_arrivals <- filter(estimated_arrivals, location != "Bonaire, Saint Eustatius and Saba")
estimated_arrivals <- filter(estimated_arrivals, location != "Cayman Islands")
estimated_arrivals <- filter(estimated_arrivals, location != "Cook Islands")
estimated_arrivals <- filter(estimated_arrivals, location != "Curacao")
estimated_arrivals <- filter(estimated_arrivals, location != "Falkland Islands")
estimated_arrivals <- filter(estimated_arrivals, location != "Faroe Islands")
estimated_arrivals <- filter(estimated_arrivals, location != "French Polynesia")
estimated_arrivals <- filter(estimated_arrivals, location != "Gibraltar")
estimated_arrivals <- filter(estimated_arrivals, location != "Greenland")
estimated_arrivals <- filter(estimated_arrivals, location != "Guadeloupe")
estimated_arrivals <- filter(estimated_arrivals, location != "Guam")
estimated_arrivals <- filter(estimated_arrivals, location != "Guernsey")
estimated_arrivals <- filter(estimated_arrivals, location != "Hong Kong (SAR)")
estimated_arrivals <- filter(estimated_arrivals, location != "Isle of Man")
estimated_arrivals <- filter(estimated_arrivals, location != "Jersey")
estimated_arrivals <- filter(estimated_arrivals, location != "Macao (SAR)")
estimated_arrivals <- filter(estimated_arrivals, location != "Martinique")
estimated_arrivals <- filter(estimated_arrivals, location != "Mayotte")
estimated_arrivals <- filter(estimated_arrivals, location != "Myanmar")
estimated_arrivals <- filter(estimated_arrivals, location != "New Caledonia")
estimated_arrivals <- filter(estimated_arrivals, location != "North Mariana Isl")
estimated_arrivals <- filter(estimated_arrivals, location != "Palau")
estimated_arrivals <- filter(estimated_arrivals, location != "Puerto Rico")
estimated_arrivals <- filter(estimated_arrivals, location != "Reunion")
estimated_arrivals <- filter(estimated_arrivals, location != "Samoa")
estimated_arrivals <- filter(estimated_arrivals, location != "Solomon Islands")
estimated_arrivals <- filter(estimated_arrivals, location != "St Barthelemy")
estimated_arrivals <- filter(estimated_arrivals, location != "St Helena")
estimated_arrivals <- filter(estimated_arrivals, location != "St Maarten (Dutch Part)")
estimated_arrivals <- filter(estimated_arrivals, location != "Svalbard")
estimated_arrivals <- filter(estimated_arrivals, location != "Swaziland")
estimated_arrivals <- filter(estimated_arrivals, location != "Tonga")
estimated_arrivals <- filter(estimated_arrivals, location != "Turkmenistan")
estimated_arrivals <- filter(estimated_arrivals, location != "Turks-Caicos")
estimated_arrivals <- filter(estimated_arrivals, location != "Vanuatu")
estimated_arrivals <- filter(estimated_arrivals, location != "Virgin Islands (GB)")
estimated_arrivals <- filter(estimated_arrivals, location != "Virgin Islands (US)")
estimated_arrivals <- filter(estimated_arrivals, location != "French Guiana")

stopifnot(all(intersect(primary_source_locations, estimated_arrivals$location) == primary_source_locations))

min_date_intersection <- max(min(estimated_arrivals$date), min(prop_potential_seeders$date))
max_date_intersection <- min(max(estimated_arrivals$date), max(prop_potential_seeders$date))

seeder_numbers <- left_join(prop_potential_seeders,
                            estimated_arrivals) %>%
  mutate(num_seeders = seeder_proportion * num_arrivals) %>%
  filter(date <= max_date_intersection, date >= min_date_intersection)

.mask <- is.na(seeder_numbers$num_seeders)
seeder_numbers[.mask, "num_arrivals"] <- 0
seeder_numbers[.mask, "num_seeders"] <- 0

eii_data <- seeder_numbers %>%
    mutate(primary_location = ifelse(test = location %in% primary_source_locations,
                                     yes = location, no = "other")) %>%
    group_by(date,primary_location) %>%
    summarise(num_intros = sum(num_seeders)) %>%
    filter(!is.na(num_intros))

stopifnot(is.element(el = "Italy", set = unique(eii_data$primary_location)))
stopifnot(is.element(el = "other", set = unique(eii_data$primary_location)))

write.table(x = eii_data,
            file = "results/estimated-introduction-index-poverty-adjusted.csv",
            sep = ",",
            row.names = FALSE)
