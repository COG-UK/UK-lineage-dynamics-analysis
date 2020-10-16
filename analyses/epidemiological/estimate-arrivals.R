library(magrittr)
library(dplyr)


iata_df <- read.csv("results/clean-iata.csv") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d"))

home_office_df <- read.csv("results/clean-home-office.csv") %>%
    mutate(date = as.Date(date, format = "%d-%b-%y"))


iata_and_ho_df <- left_join(iata_df, home_office_df, by = "date") 

non_air_df <- read.csv("results/clean-non-air-travel.csv") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    rename(non_air_num = daily_average)

all_arrivals_df <- left_join(iata_and_ho_df, non_air_df) %>%
    mutate(non_air_num = ifelse(test = is.na(non_air_num), yes = 0, no = non_air_num),
           estimate = daily_average * total_air_travels + non_air_num) %>%
    filter(!is.na(estimate))


write.table(x = all_arrivals_df,
            file = "results/estimated-arrivals.csv",
            sep = ",",
            row.names = FALSE)
