library(dplyr)

x <- read.csv("../../data/epidemiological/un-population.csv",
              header = TRUE,
              stringsAsFactors = FALSE) %>%
    filter(Variant == "Medium", Time == 2020) %>%
    select(Location, PopTotal) %>%
    rename(location = Location, population_size = PopTotal)

jhu_deaths <- read.csv("results/clean-jhu-deaths.csv") %>%
    mutate(date = as.Date(date),
           deaths = daily_deaths) %>%
  filter(location != "United Kingdom",
         location != "West Bank and Gaza")


mutate_location_name <- function(df, old_name, new_name) {
  mask <- df$location == old_name
  df[mask,"location"] <- new_name
  return(df)
}

x <- mutate_location_name(x, "Russian Federation", "Russia")
x <- mutate_location_name(x, "Bolivia (Plurinational State of)", "Bolivia")
x <- mutate_location_name(x, "Republic of Korea", "Korea, South")
x <- mutate_location_name(x, "United States of America", "US")
x <- mutate_location_name(x, "Iran (Islamic Republic of)", "Iran")
x <- mutate_location_name(x, "Brunei Darussalam", "Brunei")
x <- mutate_location_name(x, "United Republic of Tanzania", "Tanzania")
x <- mutate_location_name(x, "Syrian Arab Republic", "Syria")
x <- mutate_location_name(x, "China, Taiwan Province of China", "Taiwan*")
x <- mutate_location_name(x, "Venezuela (Bolivarian Republic of)", "Venezuela")
x <- mutate_location_name(x, "Republic of Moldova", "Moldova")
x <- mutate_location_name(x, "Viet Nam", "Vietnam")
x <- mutate_location_name(x, "Myanmar", "Burma")
x <- mutate_location_name(x, "Congo", "Congo (Brazzaville)")
x <- mutate_location_name(x, "Democratic Republic of the Congo", "Congo (Kinshasa)")
x <- mutate_location_name(x, "Côte d'Ivoire", "Cote d'Ivoire")
x <- mutate_location_name(x, "Lao People's Democratic Republic", "Laos")

stopifnot(length(setdiff(jhu_deaths$location, x$location)) == 0)

write.table(x = x,
            file = "results/clean-un-population.csv",
            sep = ",",
            row.names = FALSE)
