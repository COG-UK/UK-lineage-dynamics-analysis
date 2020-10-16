## @@ age-of-infection-matrix-defn @@

age_of_infection_matrix <- function(inf_to_death_days, deaths_vector) {
    num_days_in_matrix <- length(deaths_vector) + inf_to_death_days
    result <- matrix(data = NA,
                     nrow = num_days_in_matrix,
                     ncol = inf_to_death_days + 1)

    padded_deaths <- c(rep(0, inf_to_death_days),
                       deaths_vector,
                       rep(NA, inf_to_death_days))

    for (i in 1:num_days_in_matrix) {
        result[i,] <- rev(padded_deaths[i + (0:inf_to_death_days)])
    }

    return(result)
}

## @@ padded-potential-seeders-defn @@

padded_potential_seeders <- function(age_matrix,
                                     days_latent,
                                     days_incubating,
                                     days_infectious,
                                     prop_asymptomatic) {

    presymptomatic_cases <- (age_matrix[,0:days_incubating + 1])
    asymptomatic_cases <- prop_asymptomatic * age_matrix[,(days_incubating + 1):(days_latent + days_infectious) + 1]

    padded_total_seeders <- rowSums(cbind(presymptomatic_cases, asymptomatic_cases))

    return(padded_total_seeders)
}

## @@ seeder-proportion-defn @@

seeder_proportion <- function(deaths_df,
                              location_population,
                              days_latent,
                              days_incubating,
                              days_infectious,
                              prop_asymptomatic,
                              days_infection_to_death,
                              infection_fatality_ratio) {
    if (!setequal(names(deaths_df), c("deaths", "date"))) {
        stop("Bad dataframe names: ", names(deaths_df))
    }


    age_matrix <- age_of_infection_matrix(days_infection_to_death, deaths_df$deaths)

    potential_seeders <- padded_potential_seeders(age_matrix,
                                                  days_latent,
                                                  days_incubating,
                                                  days_infectious,
                                                  prop_asymptomatic)

    start_date <- min(deaths_df$date)
    padding_dates <- seq(from = start_date - days_infection_to_death,
                         to = start_date - 1,
                         by = 1)

    total_dates <- c(padding_dates, deaths_df$date)

    data.frame(date = total_dates,
               seeder_proportion = infection_fatality_ratio * potential_seeders / location_population)
}

## @@ jhu-un-location-unification @@

library(dplyr)
library(purrr)

jhu_deaths <- read.csv("results/clean-jhu-deaths.csv") %>%
    mutate(date = as.Date(date),
           deaths = daily_deaths) %>%
  filter(location != "United Kingdom",
         location != "West Bank and Gaza")

un_populations <- read.csv("results/clean-un-population.csv")

stopifnot(length(setdiff(jhu_deaths$location, un_populations$location)) == 0)

## @@ model-parameters @@
days_latent <- 3
days_incubating <- 5
days_infectious <- 7
prop_asymptomatic <- 0.31
days_infection_to_death <- 23
infection_fatality_ratio <- 100

location_seeder_props <- function(location_str) {
    deaths_df <- filter(jhu_deaths, location == location_str) %>%
        select(date,deaths)

    if (is.element(location_str, un_populations$location)) {
        pop_size <- 1e3 * un_populations[un_populations$location == location_str, "population_size"]
    } else {
        stop("Cannot find a population size for ", location_str)
    }

    seeder_props <- seeder_proportion(deaths_df,
                                      pop_size,
                                      days_latent,
                                      days_incubating,
                                      days_infectious,
                                      prop_asymptomatic,
                                      days_infection_to_death,
                                      infection_fatality_ratio)

    seeder_props$location <- location_str
    return(seeder_props)
}


result <- map(.x = unique(jhu_deaths$location),
              .f = location_seeder_props) %>%
  bind_rows %>%
  filter(date < "2020-07-01")

stopifnot(!any(is.na(result$seeder_proportion)))

write.table(x = result,
            file = "results/estimated-proportion-seeders.csv",
            sep = ",",
            row.names = FALSE)

location_num_infections <- function(location_str) {
    deaths_df <- filter(jhu_deaths, location == location_str) %>%
        select(date,deaths)

    age_matrix <- age_of_infection_matrix(days_infection_to_death, deaths_df$deaths)

    num_deaths_infs <- age_matrix[,1]

    start_date <- min(deaths_df$date)
    padding_dates <- seq(from = start_date - days_infection_to_death,
                         to = start_date - 1,
                         by = 1)

    total_dates <- c(padding_dates, deaths_df$date)

    data.frame(date = total_dates,
               num_infs = infection_fatality_ratio * num_deaths_infs,
               location = location_str)
}

result <- map(.x = unique(jhu_deaths$location),
              .f = location_num_infections) %>%
    bind_rows

write.table(x = result,
            file = "results/estimated-daily-infections.csv",
            sep = ",",
            row.names = FALSE)
