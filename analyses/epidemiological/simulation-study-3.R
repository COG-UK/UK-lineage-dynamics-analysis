library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)

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

#' Return a vector of TMRCA values
#'
#' @param n integer number of TMRCA values
#' @param intro_dates integer vector of dates
#' @param intro_dist numeric vector of date weights
#' @param r_size function which takes an integer and returns that many sizes
#' @param mean_from_size function from size to the mean lag
#'
r_tmrcas <- function(n, intro_dates, intro_dist, r_size, mean_from_size) {
  r_intro_dates <- sample(intro_dates,
                          size = n,
                          replace = TRUE,
                          prob = intro_dist)
  r_sizes <- r_size(n)
  mean_delay <- mean_from_size(r_sizes)
  r_delay <- rgeom(n = n,
                   prob = 1 / (1 + mean_delay))
  return(r_intro_dates + r_delay)
}

date_as_day_of_year <- function(date_objs) {
  as.integer(format.Date(date_objs, "%j"))
}

eii_csv <- "results/estimated-introduction-index.csv"
eii_df <- read.csv(eii_csv,
                   stringsAsFactors = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>%
    summarise(total_intros = sum(num_intros))

tmrca_date_range <- as.Date(c("2020-01-01", "2020-06-30"))
eii_eps <- data.frame(date = seq(from = tmrca_date_range[1],
                                 to = tmrca_date_range[2],
                                 by = 1))

eii_df <- full_join(eii_df, eii_eps, by = "date")
eii_df[is.na(eii_df$total_intros),]$total_intros <- 0

intro_dates <- eii_df$date
intro_date_nums <- date_as_day_of_year(eii_df$date)
intro_dist <- eii_df$total_intros / sum(eii_df$total_intros)

intro_prob_df <- data.frame(date = intro_dates,
                            date_num = intro_date_nums,
                            prob = intro_dist)

daily_intro_log_probs <- log(intro_prob_df$prob)
.inf_mask <- is.infinite(daily_intro_log_probs)
daily_intro_log_probs[.inf_mask] <- .Machine$double.min.exp

r_cluster_sizes_factory <- function(cluster_sizes) {
  num_clusters <- length(cluster_sizes)
  function() {
  sample(cluster_sizes,
         size = num_clusters,
         replace = TRUE)
  }
}

data_file <- "../../data/epidemiological/clusters_DTA_MCC_0.5.csv"

cluster_df <- read.csv(data_file,
                      stringsAsFactors = FALSE) %>%
  mutate(tmrca_date = as.Date(tmrca_calendar),
         date_num = date_as_day_of_year(tmrca_date))

r_cluster_sizes <- cluster_df %>%
  use_series("seqs") %>%
  r_cluster_sizes_factory

mean_from_size_factory <- function(alpha, beta) {
  function(c_size) {
    alpha + beta / c_size
  }
}

mean_from_size <- mean_from_size_factory(0.7189865, 28.91369)

estimate_as_df <- function(est) {
  data.frame(alpha = exp(est$estimate[1]),
             beta = exp(est$estimate[2]),
             exit_code = est$code)
}

random_reestimate <- function() {
  foo_n <- nrow(cluster_df)

  valid_sim <- FALSE
  loop_count <- 0
  while (not(valid_sim) && loop_count < 20) {
    foo_sizes <- r_cluster_sizes()
    foo_tmrca_dates <- r_tmrcas(foo_n,
                                intro_dates,
                                intro_dist,
                                function(dummy) {foo_sizes},
                                mean_from_size) %>%
      date_as_day_of_year
    if (max(foo_tmrca_dates) <= length(daily_intro_log_probs)) {
      valid_sim <- TRUE
    } else {
      loop_count <- loop_count + 1
      warning("failed simulation...")
    }
  }

  if (valid_sim) {
    foo_llhd <- model_2_llhd_factory(daily_intro_log_probs,
                                     foo_tmrca_dates,
                                     foo_sizes)

    foo_obj <- function(p) { - foo_llhd(exp(p)) }
    foo_est <- nlm(f = foo_obj, p = rnorm(2))
    estimate_as_df(foo_est)
  } else {
    data.frame(alpha = NA, beta = NA, exit_code = 5)
  }
}

num_replicates <- 100
plot_df <- rerun(.n = num_replicates, random_reestimate()) %>%
  bind_rows

write.table(x = plot_df,
            file = "results/sim-study-3-reestimates.csv",
            sep = ",",
            row.names = FALSE)
