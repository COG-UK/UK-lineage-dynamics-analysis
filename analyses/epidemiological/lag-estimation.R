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

cluster_df <- read.csv("../../data/epidemiological/clusters_DTA_MCC_0.5.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(tmrca_date = as.Date(tmrca_calendar),
         date_num = date_as_day_of_year(tmrca_date)) %>%
  select(cluster, seqs, tmrca_date, date_num)

stopifnot(min(eii_df$date) >= as.Date("2020-01-01"))
stopifnot(min(eii_df$date) <= min(cluster_df$tmrca_date))
stopifnot(max(eii_df$date) >= max(cluster_df$tmrca_date))

tmrca_dates <- cluster_df$date_num
cluster_sizes <- cluster_df$seqs

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

model_2_llhd <- model_2_llhd_factory(daily_intro_log_probs,
                                     tmrca_dates,
                                     cluster_sizes)

a_vals <- seq(from = 0.1, to = 20, length = 20)
b_vals <- seq(from = 0.1, to = 50, length = 20)
z_vals <- cross2(a_vals, b_vals) %>%
  map(compose(model_2_llhd,as_vector)) %>%
  as_vector
ab_vals <- cross2(a_vals, b_vals) %>%
  transpose %>%
  map(as_vector) %>%
  set_names(c("a", "b"))

result <- data.frame(alpha = ab_vals$a,
                     beta = ab_vals$b,
                     llhd = z_vals)

write.table(result,
            file = "results/lag-estimation-llhds.csv",
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
            file = "results/lag-estimate-mle-estimates.csv",
            quote = FALSE,
            sep = ",",
            row.names = FALSE)

sink(file = "results/llhd-ratio-report-lag-estimate.txt")

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

sink()

mle_estimate <- exp(altr_est$estimate)

mesh_length <- 50

alpha_mesh <- seq(from = 0, to = mle_estimate[1] * 2, length = mesh_length)
alpha_profile <- alpha_mesh %>% map(~ model_2_llhd(c(.x, mle_estimate[2]))) %>% as_vector

beta_mesh <- seq(from = 0, to = mle_estimate[2] * 2, length = mesh_length)
beta_profile <- beta_mesh %>% map(~ model_2_llhd(c(mle_estimate[1], .x))) %>% as_vector

result <- data.frame(param = rep(c("alpha", "beta"), each = mesh_length),
                     value = c(alpha_mesh, beta_mesh),
                     llhd = c(alpha_profile, beta_profile))

write.table(result,
            file = "results/lag-estimate-llhd-profiles.csv",
            quote = FALSE,
            sep = ",",
            row.names = FALSE)

model_criticism_df <- data.frame(cluster_size = cluster_sizes,
                                 tmrca_date = tmrca_dates,
                                 mean_lag_estimate = median(numeric_mle$alpha) + median(numeric_mle$beta) / cluster_sizes)
sorted_ixs <- order(model_criticism_df$tmrca_date, model_criticism_df$cluster_size)
model_criticism_df <- model_criticism_df[sorted_ixs,]
model_criticism_df$ix = 1:nrow(model_criticism_df)
model_criticism_df$lag_lower_bound <- qgeom(p = 0.025, prob = 1 / (1 + model_criticism_df$mean_lag_estimate))
model_criticism_df$lag_upper_bound <- qgeom(p = 0.975, prob = 1 / (1 + model_criticism_df$mean_lag_estimate))


write.table(x = model_criticism_df,
            file = "results/lag-estimate-model-fig.csv",
            sep = ",",
            row.names = FALSE)

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

r_size <- function(n) {
  sample(x = cluster_sizes, size = n, replace = TRUE)
}

mean_from_size_factory <- function(alpha, beta) {
  function(c_size) {
    alpha + beta / c_size
  }
}

mean_from_size <- mean_from_size_factory(median(numeric_mle$alpha), 
                                         median(numeric_mle$beta))

replicated_tmrca_sampes <- rerun(.n = 19, 
                                 r_tmrcas(length(cluster_sizes),
                                          intro_date_nums,
                                          intro_dist,
                                          r_size,
                                          mean_from_size))

map3 <- function(.x, .y, .z, .f, ...) {
  stopifnot(class(.x) == "list")
  stopifnot(class(.y) == "list")
  stopifnot(class(.z) == "list")
  n <- length(.x)
  stopifnot(length(.y) == n)
  stopifnot(length(.z) == n)

  lapply(1:n, function(ix) .f(.x[[ix]],
                              .y[[ix]],
                              .z[[ix]]))
}

foo <- c(list(tmrca_dates), replicated_tmrca_sampes)
bar <- as.list(c("truth", rep("simulation", 19)))
baz <- as.list(1:20)

plot_df <- map3(foo,
                bar,
                baz,
                function(x,y,z) data.frame(tmrca = x,
                                           model = y,
                                           id = z)) %>% 
  bind_rows

write.table(x = plot_df,
            file = "results/lag-estimation-tmrca-replicates.csv",
            sep = ",",
            row.names = FALSE)
