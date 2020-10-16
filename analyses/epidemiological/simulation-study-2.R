library(dplyr)
library(ggplot2)
library(magrittr)
library(purrr)
library(stats)

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

mean_size <- 8
rand_sizes <- rgeom(n = num_intros,
                    prob = 1 - mean_size / (1 + mean_size)) + 2

mean_delay <- 8 + 20 / rand_sizes
rand_delays <- rgeom(n = num_intros,
                     prob = 1 - mean_delay / (1 + mean_delay))
rand_tmrcas <- rand_delays + rand_intro_dates

tmrca_df <- data.frame(date = rand_tmrcas,
                       delay = rand_delays,
                       size = rand_sizes)
intro_prob_df <- data.frame(date = intro_dates,
                            prob = intro_dist)

sim_data <- dplyr::full_join(tmrca_df,
                             intro_prob_df,
                             by = "date")

time_series_fig <- ggplot() +
    geom_line(data = intro_prob_df,
              mapping = aes(x = date, y = prob)) +
    geom_histogram(data = tmrca_df,
                   mapping = aes(x = date, y = ..density..),
                   alpha = 0.2) +
    labs(x = "Date", y = "Density") +
    theme_classic()

scatter_plot_fig <- ggplot(data = tmrca_df,
       mapping = aes(x = size,
                     y = delay)) +
    geom_jitter() +
    scale_y_log10() +
    geom_smooth(method = "lm", colour = "black") +
    labs(x = "Cluster size", y = "Lag") +
    theme_classic()

simulation_fig <- cowplot::plot_grid(time_series_fig, 
                                     scatter_plot_fig,
                                     nrow = 2)

ggsave("results/sim-study-2-fig.png", simulation_fig)

log_sum_exp <- function(x) {
  m <- max(x)
  log(sum(exp(x - m))) + m
}

model_2_llhd_factory <- function(daily_intro_log_probs, tmrca_date, cluster_size) {
    stopifnot(length(tmrca_date) == length(cluster_size))
    stopifnot(max(tmrca_date) <= length(daily_intro_log_probs))
    num_intros <- length(tmrca_date)
    max_possible_lag <- max(tmrca_date)

    function(params) {
        a <- params[1]
        b <- params[2]
        if (min(params) >= 0) {
            llhd <- 0
            mean_lags <- a + b / cluster_size
            for (ix in 1:num_intros) {
                tmrca <- tmrca_date[ix]
                mean_lag <- mean_lags[ix]
                lag_lpmf <- dgeom(x = 0:max_possible_lag,
                                  prob = 1 / (1 + mean_lag),
                                  log = TRUE)
                llhd <- llhd +
                    log_sum_exp(daily_intro_log_probs[1:tmrca] +
                                rev(lag_lpmf[1:tmrca]))
            }
        } else {
            llhd <- - .Machine$double.xmax
        }
        return(llhd)
    }
}

tmrca_dates <- sim_data %>%
    filter(not(is.na(size))) %>%
    use_series("date")
cluster_sizes <- sim_data %>%
    filter(not(is.na(size))) %>%
    use_series("size")

daily_intro_log_probs <- dplyr::left_join(data.frame(date = 0:229,
                                                     dummy = NA),
                                          intro_prob_df,
                                          by = "date") %>%
    mutate(safe_prob = ifelse(test = is.na(prob),
                              yes = .Machine$double.eps,
                              no = prob)) %>%
    use_series("safe_prob") %>%
    log

model_2_llhd <- model_2_llhd_factory(daily_intro_log_probs,
                                     tmrca_dates,
                                     cluster_sizes)

a_vals <- seq(from = 2, to = 20, by = 2)
b_vals <- seq(from = 5, to = 80, by = 5)
z_vals <- cross2(a_vals, b_vals) %>%
    map(compose(model_2_llhd,as_vector)) %>%
    as_vector
ab_vals <- cross2(a_vals, b_vals) %>%
    transpose %>%
    map(as_vector) %>%
    set_names(c("a", "b"))

result <- data.frame(alpha = sprintf("%02d", ab_vals$a),
                     beta = sprintf("%02d", ab_vals$b),
                     llhd = z_vals)

write.table(result,
            file = "results/sim-study-2-llhds.csv",
            quote = FALSE,
            sep = ",",
            row.names = FALSE)

estimate_as_df <- function(est) {
  est %>%
    use_series("estimate") %>%
    exp %>%
    set_names(c("alpha", "beta")) %>%
    as.list %>%
    as.data.frame
}

numeric_mle <- 10 %>%
  rerun(log(rexp(n = 2,
                 rate = 1 / c(10, 10)))) %>%
  map(~ nlm(f = function(p) { - model_2_llhd(exp(p)) },
            p = .x)) %>%
  keep(~ .x$code == 1) %>%
  map(estimate_as_df) %>%
  bind_rows

write.table(x = numeric_mle,
            file = "results/sim-study-2-mle.csv",
            quote = FALSE,
            sep = ",",
            row.names = FALSE)

null_obj <- function(p) { - model_2_llhd(c(exp(p), 0)) }
null_est <- nlm(f = null_obj, p = rnorm(1))
if (null_est$code == 1) {
  null_llhd <- - null_est$minimum
} else {
  stop("Optimisation non-unit code.")
}

altr_obj <- function(p) { - model_2_llhd(exp(p)) }
altr_est <- nlm(f = altr_obj, p = rnorm(2))
if (altr_est$code == 1) {
  altr_llhd <- - altr_est$minimum
} else {
  stop("Optimisation non-unit code.")
}


llhd_ratio_stat <- 2 * (altr_llhd - null_llhd)
p_val <- pchisq(q = llhd_ratio_stat,
                df = 1, lower.tail = FALSE)

cat("The null LLHD is ", null_llhd, " at ", exp(null_est$estimate), "\n",
    "The alternative LLHD is ", altr_llhd, " at ", exp(altr_est$estimate), "\n",
    "The likelihood ratio statistic is ", llhd_ratio_stat, "with 1 degree of freedom\n",
    "the p-value is ", p_val, "\n")
