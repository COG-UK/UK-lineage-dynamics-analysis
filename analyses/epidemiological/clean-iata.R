library(magrittr)
library(dplyr)
library(purrr)
library(memoise)

x <- read.csv("../../data/epidemiological/IATA_CountryLevel_Dec_May.csv",
              header = TRUE,
              stringsAsFactors = FALSE) %>%
    filter(month != "", country_origin != "United Kingdom") %>%
    select(country_origin, month, total_volume) %>%
    rename(location = country_origin, num_travellers = total_volume)


month_total_factory <- function(x) {
    function(country_str, month_str) {
        maybe_count <- x %>%
            filter(location == country_str,
                   grepl(pattern = month_str, x = month)) %>%
            use_series("num_travellers")
        switch(length(maybe_count)+1,
               0,
               maybe_count,
               stop("Bad country and month: ", country_str, month_str))
    }
}

month_total <- month_total_factory(x)
m_month_total <- memoise(month_total)

location_names <- unique(x$location)
num_locations <- length(location_names)

dates <- seq(from = as.Date("2019-12-01"),
             to = as.Date("2020-04-30"),
             by = 1)


month_global_total_factory <- function(x) {
    month_totals_df <- x %>%
        group_by(month) %>%
        summarise(total_travellers = sum(num_travellers))

    function(month_str) {
        maybe_count <- month_totals_df %>%
            filter(grepl(pattern = month_str, x = month)) %>%
            use_series("total_travellers")
        if (length(maybe_count) == 1) {
            return(maybe_count)
        } else {
            stop("Bad month: ", month_str)
        }
    }
}

month_global_total <- month_global_total_factory(x)
m_month_global_total <- memoise(month_global_total)



record_list <- function(country_str, date_obj) {
    month_str <- format(date_obj, format = "%b")

    month_passenger_count <- m_month_total(country_str, month_str)
    month_total_count <- m_month_global_total(month_str)

    daily_average <- month_passenger_count / month_total_count

    data.frame(location = country_str,
               date = date_obj,
               daily_average = daily_average)
}

result <- cross2(location_names, dates) %>%
    map(lift_dl(record_list)) %>% 
    bind_rows

write.table(x = result,
            file = "results/clean-iata.csv",
            sep = ",",
            row.names = FALSE)
