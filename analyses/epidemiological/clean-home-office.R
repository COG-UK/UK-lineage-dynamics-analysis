library(dplyr)

result <- read.csv("../../data/epidemiological/home-office.csv") %>%
    mutate(total_air_travels = as.numeric(gsub(pattern = ",",
                                               replacement = "",
                                               x = Total.air.arrivals))) %>%
    select(Date,total_air_travels) %>%
    rename(date = Date)

write.table(x = result,
            file = "results/clean-home-office.csv",
            sep = ",",
            row.names = FALSE)
