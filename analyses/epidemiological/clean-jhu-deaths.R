library(dplyr)
library(magrittr)
library(purrr)
library(reshape2)

x <- read.csv("../../data/epidemiological/jhu-deaths.csv",
              header = TRUE,
              stringsAsFactors = FALSE) %>%
    select(Province.State,
           Country.Region,
           starts_with("X")) %>%
    filter(Country.Region != "Diamond Princess",
           Country.Region != "MS Zaandam") %>%
    melt(id.vars = c("Province.State","Country.Region"),
         value.name = "cumulative_deaths",
         variable.name = "date_string") %>%
    mutate(date = as.Date(date_string, format = "X%m.%d.%y"))

countries_needing_cleaning <- x %>%
    filter(Province.State != "") %>%
    use_series("Country.Region") %>%
    unique

subset_aus <- x %>%
    filter(Country.Region == "Australia") %>%
    group_by(Country.Region, date) %>%
    summarise(cumulative_deaths = sum(cumulative_deaths))

subset_can <- x %>%
    filter(Country.Region == "Canada",
           Province.State != "Grand Princess",
           Province.State != "Diamond Princess") %>%
    group_by(Country.Region, date) %>%
    summarise(cumulative_deaths = sum(cumulative_deaths))

subset_chn <- x %>%
    filter(Country.Region == "China") %>%
    group_by(Country.Region, date) %>%
    summarise(cumulative_deaths = sum(cumulative_deaths))

subset_dnk <- x %>%
    filter(Country.Region == "Denmark", Province.State == "") %>%
    group_by(Country.Region, date) %>%
    summarise(cumulative_deaths = sum(cumulative_deaths))

subset_fra <- x %>%
    filter(Country.Region == "France", Province.State == "") %>%
    group_by(Country.Region, date) %>%
    summarise(cumulative_deaths = sum(cumulative_deaths))

subset_nld <- x %>%
    filter(Country.Region == "Netherlands", Province.State == "") %>%
    group_by(Country.Region, date) %>%
    summarise(cumulative_deaths = sum(cumulative_deaths))

subset_gbr <- x %>%
    filter(Country.Region == "United Kingdom", Province.State == "") %>%
    group_by(Country.Region, date) %>%
    summarise(cumulative_deaths = sum(cumulative_deaths))

result_ <- x %>%
    filter(not(is.element(el = Country.Region, set = countries_needing_cleaning))) %>%
    select(Country.Region, cumulative_deaths, date)

result <- rbind(result_,
                subset_aus,
                subset_can,
                subset_chn,
                subset_dnk,
                subset_fra,
                subset_nld,
                subset_gbr) %>%
    rename(location = Country.Region)

first_date <- min(result$date)

diffed_deaths <- function(location_str) {
    tmp <- filter(result, location == location_str)
    death_diff <- diff(tmp$cumulative_deaths)
    tmp <- filter(tmp, date > first_date)
    tmp$daily_deaths <- pmax(0, death_diff)
    return(tmp)
}

location_names <- unique(result$location)

diffed_result <- location_names %>%
    map(diffed_deaths) %>%
    bind_rows

result <- diffed_result

result[result$location == "China" & result$date == "2020-04-17", "daily_deaths"] <- 0
.mask <- result$location == "China" & result$date < "2020-04-17"
death_adjustment <- 1290 / nrow(result[.mask,])
result[.mask, "daily_deaths"] <- result[.mask, "daily_deaths"] + death_adjustment

.kosovo_mask <- result$location == "Kosovo"
.kosovo_cumulative_deaths <- result[.kosovo_mask, "cumulative_deaths"]
.kosovo_daily_deaths <- result[.kosovo_mask, "daily_deaths"]
result_rm_kosovo <- filter(result, location != "Kosovo")
.serbia_mask <- result_rm_kosovo$location == "Serbia"
result_rm_kosovo[.serbia_mask, "cumulative_deaths"] <- result_rm_kosovo[.serbia_mask, "cumulative_deaths"] + .kosovo_cumulative_deaths
result_rm_kosovo[.serbia_mask, "daily_deaths"] <- result_rm_kosovo[.serbia_mask, "daily_deaths"] + .kosovo_daily_deaths

write.table(x = result_rm_kosovo,
            file = "results/clean-jhu-deaths.csv",
            sep = ",",
            row.names = FALSE)
