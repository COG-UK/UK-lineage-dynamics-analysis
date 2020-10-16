model_1_llhd_factory <- function(daily_introduction_prob, daily_tmrca_count) {
    stopifnot(length(daily_introduction_prob) == length(daily_tmrca_count))
    num_days <- length(daily_introduction_prob)
    function(mean_lag) {
        if (mean_lag > 0) {
            lag_pmf <- dgeom(x = 0:(num_days - 1), prob = 1 / (1 + mean_lag))

            tmrca_pmf <- numeric(num_days)

            for (ix in 1:num_days) {
                tmrca_pmf[ix] <- daily_introduction_prob[1:ix] %*% rev(lag_pmf[1:ix])
            }

            tmrca_pmf <- tmrca_pmf / sum(tmrca_pmf)

            # independence assumption
            as.numeric(daily_tmrca_count %*% log(tmrca_pmf))
        } else {
            -1e10
        }
    }
}

library(dplyr)
library(ggplot2)

set.seed(1)

eii_csv <- "results/estimated-introduction-index.csv"

x <- read.csv(eii_csv, stringsAsFactors = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>%
    summarise(total_intros = sum(num_intros))

num_intros <- 1000
intro_dates <- as.integer(x$date - min(x$date))
intro_dist <- x$total_intros / sum(x$total_intros)

rand_intro_dates <- sample(intro_dates,
                           size =  num_intros,
                           replace = TRUE,
                           prob = intro_dist)

mean_delay <- 10 # days
rand_delays <- rgeom(n = num_intros, prob = 1 - mean_delay / (1 + mean_delay))

rand_tmrcas <- rand_delays + rand_intro_dates

sim_data_fig <- ggplot() +
    geom_line(data = x,
              mapping = aes(x = date, y = total_intros)) +
    geom_histogram(data = data.frame(tmrca = min(x$date) + rand_tmrcas),
                   mapping = aes(x = tmrca)) +
    labs(y = "Frequency", x = "Date") +
    theme_classic()

ggsave("results/sim-data-fig.png", sim_data_fig)

tmrca_table <- table(rand_tmrcas)
tmrca_df <- data.frame(day_num = as.integer(names(tmrca_table)),
                       num_tmrca = as.integer(tmrca_table))

intro_probs_df <- data.frame(day_num = intro_dates,
                             intro_prob = intro_dist)

sim_data <- dplyr::full_join(intro_probs_df, tmrca_df)

na_mask <- is.na(sim_data$num_tmrca)
sim_data[na_mask,"num_tmrca"] <- 0
rm(na_mask)

na_mask <- is.na(sim_data$intro_prob)
sim_data[na_mask,"intro_prob"] <- 0
rm(na_mask)


model_1_llhd <- model_1_llhd_factory(sim_data$intro_prob, sim_data$num_tmrca)

mean_lag_mesh <- seq(from = 3, to = 20, length = 100)
plot_df <- data.frame(mean_lag = mean_lag_mesh,
                      llhd = purrr::as_vector(purrr::map(.x = mean_lag_mesh, .f = model_1_llhd)))

sim_llhd_fig <- ggplot(plot_df, aes(x = mean_lag, y = llhd)) +
    geom_line() +
    geom_vline(xintercept = mean_delay, linetype = "dashed") +
    labs(x = "Expected lag duration", y = "Log-likelihood") +
    theme_classic()

ggsave("results/sim-llhd-fig.png", sim_llhd_fig)
