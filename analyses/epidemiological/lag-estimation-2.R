library(dplyr)
library(magrittr)
library(purrr)
library(reshape2)

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

llhd_factory_applier <- function(cutoff_factor, all_clusters_df,
                                 daily_intro_log_probs, eii_df) {
  cluster_df <- filter(.data = all_clusters_df,
                       cutoff == cutoff_factor)

  stopifnot(nrow(cluster_df) > 0)
  stopifnot(min(eii_df$date) >= as.Date("2020-01-01"))
  stopifnot(min(eii_df$date) <= min(cluster_df$tmrca_date))
  stopifnot(max(eii_df$date) >= max(cluster_df$tmrca_date))

  list(llhd_func = model_2_llhd_factory(daily_intro_log_probs,
                                        cluster_df$date_num,
                                        cluster_df$seqs),
       cutoff = cutoff_factor)
}

estimated_mle <- function(llhd_obj) {
  stopifnot(setequal(names(llhd_obj),
                     c("llhd_func", "cutoff")))

  p0 <- rnorm(n = 2)

  .f <- function(p) {
    - llhd_obj$llhd_func(exp(p))
  }

  est <- nlm(f = .f, p = p0)

  result <- est$estimate %>%
    exp %>%
    set_names(c("alpha", "beta")) %>%
    as.list
  result$cutoff <- llhd_obj$cutoff
  result$llhd <- - est$minimum

  if (est$code > 2) {
    warning("Inference failed for cutoff: ",
            result$cutoff)
  }

  result$exit_code <- est$code
  return(result)
}

all_clusters_df <- read.csv("../../data/epidemiological/clusters_DTA_MCC.csv",
                            stringsAsFactors = FALSE) %>%
  mutate(tmrca_date = as.Date(tmrca_calendar),
         date_num = date_as_day_of_year(tmrca_date),
         cutoff = as.factor(cutoff)) %>%
  select(cluster, seqs, tmrca_date, date_num, cutoff)

pipeline <- function(cutoff_factor) {
  llhd_factory_applier(cutoff_factor, all_clusters_df,
                       daily_intro_log_probs, eii_df) %>%
    estimated_mle %>%
    as.data.frame
}

mle_df <- map(unique(all_clusters_df$cutoff), pipeline) %>%
  bind_rows %>%
  mutate(cutoff = as.numeric(as.character(cutoff)))

write.table(x = mle_df,
            file = "results/cutoff-varying-lag-estimates.csv",
            sep = ",",
            row.names = FALSE)
