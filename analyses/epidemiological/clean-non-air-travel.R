library(reshape2)
library(dplyr)
library(magrittr)
library(dplyr)
library(purrr)
library(memoise)

x <- read.csv("../../data/epidemiological/extra-uk-arrivals.csv", 
              header = TRUE,
              stringsAsFactors = FALSE) %>%
    mutate(country = gsub(pattern = " total", replacement = "", x = X)) %>%
    select(country, matches("*daily")) %>%
    melt(id.vars = "country", variable.name = "month_var", value.name = "daily_count")

daily_number_factory <- function(x) {
    function(country_str, month_str) {
        maybe_count <- x %>%
            filter(country == country_str,
                   grepl(pattern = month_str, x = month_var)) %>%
            use_series("daily_count")
        switch(length(maybe_count)+1,
               NA,
               maybe_count,
               stop("Bad country and month: ", country_str, month_str))
    }
}

daily_number <- daily_number_factory(x)
m_daily_number <- memoise(daily_number)

record_list <- function(country_str, date_obj) {
    month_str <- format(date_obj, format = "%b")

    daily_passenger_count <- m_daily_number(country_str, month_str)


    data.frame(location = country_str,
               date = date_obj,
               daily_average = daily_passenger_count)
}


location_names <- unique(x$country)

dates <- seq(from = as.Date("2019-12-01"),
             to = as.Date("2020-04-30"),
             by = 1)

result <- cross2(location_names, dates) %>% map(lift_dl(record_list)) %>% bind_rows

write.table(x = result,
            file = "results/clean-non-air-travel.csv",
            sep = ",",
            row.names = FALSE)
