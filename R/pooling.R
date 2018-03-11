#' Calculate the effective sample size
#'
#' This is the work horse function that calculates the effective sample size.
#' Should usually not be called by the user directly.
#'
#' @param size A vector of sample sizes from different surveys (from different
#' pollsters) for one party.
#' @param share The relative share of votes for party of interest ([0-1])
#' @param corr Assumed correlation between surveys (of different pollsters).
#' Defaults to 0.5.
#' @param weights Additional weights for individual surveys.
#' @keywords internal
effective_samplesize <- function(
  size,
  share,
  corr = 0.5,
  weights = NULL){

  assert_numeric(size, lower = 0, finite = TRUE, min.len = 1)
  if (length(size) == 1) {
    message("Only one survey/pollster provided. The provided sample size will
      be returned.")
    return(size)
  }
  assert_numeric(share, lower = 0, upper = 1, len = length(size))
  assert_number(corr, lower = -1, upper = 1)
  assert_numeric(weights, finite = TRUE, len = length(size), null.ok = TRUE)
  if (is.null(weights)) {
    weights <- size
  }

  # calculation
  p.total <- sum(weights * share) / sum(weights)
  var.ind <- p.total * (1 - p.total)
  n_inst  <- length(size)
  n.total <- sum(size)
  var.vec <- share * (1 - share) / size
  sd.vec  <- sqrt(var.vec)
  n.comb  <- 0
  for (i in (n_inst - 1):1){
    n.comb <- n.comb + i
  }
  cov.vec   <- rep(NA, n.comb)
  n.cov.vec <- cov.vec
  k <- n_inst - 1
  count <- 1
  while (k > 0){
    cov.vec[count:(count + k - 1)] <- corr * sd.vec[1:k] *
      sd.vec[(n_inst - k + 1):n_inst]
    n.cov.vec[count:(count + k - 1)] <- weights[1:k] *
      weights[(n_inst - k + 1):n_inst]
    count <- count + k
    k <- k - 1
  }
  var.est <- 1 / sum(weights)^2 * (sum((weights^2) * var.vec) +
    sum(2 * n.cov.vec * cov.vec))
  n.eff <- var.ind / var.est
  return(n.eff)

}

#' Extract surveys from institutes within a specified time-window
#'
#'
#' @param surveys A \code{tibble} containing survey results for multiple
#' pollsters as returned by \code{\link[coalitions]{get_surveys}}.
#' @param pollsters Character vector of pollsters that should be considered
#' for pooling.
#' @param last_date Only surveys in the time-window from \code{last_date} to
#' \code{last_date} - period will be considered for each pollster. Defaults
#' to current date.
#' @param period See \code{last_date} argument.
#' @param period_extended Optional. If specified, all surveys in the time-window
#' from \code{last_date} - period_extended to \code{last_date} - period will
#' also be considered for each pollster, but only after down-weighting them by
#' halving their true sample size.
#' @import dplyr checkmate
#' @keywords internal
get_eligible <- function(
  surveys,
  pollsters,
  last_date       = Sys.Date(),
  period          = 14,
  period_extended = NA) {

  assert_data_frame(surveys, min.rows = 2, min.cols = 2)
  assert_date(last_date)
  assert_character(pollsters, null.ok = TRUE)
  assert_number(period, lower = 1, finite = TRUE)
  assert_number(period_extended, lower = 1, finite = TRUE, na.ok = TRUE)
  assert_true(period < period_extended, na.ok = TRUE)

  first_date <- last_date - ifelse(!is.na(period_extended), period_extended, period)

  surveys %>% filter(.data$pollster %in% pollsters) %>%
    unnest(surveys) %>%
    filter(date >= first_date & date <= last_date) %>%
    unnest() %>%
    mutate(respondents = .data$respondents * ifelse(date >= last_date - period, 1, 0.5),
           votes = .data$votes * ifelse(date >= last_date - period, 1, 0.5)) %>%
    nest(-one_of("pollster", "date", "start", "end", "respondents")) %>%
    group_by(.data$pollster) %>%
    filter(date == max(date)) %>%
    filter(row_number() == 1)
}


#' Extract effective sample size for pooled sample
#'
#' Given a specified time window (defaults to current day - 14 days).
#' calculate the effective sample size of the pooled sample over multiple
#' pollsters.
#'
#' @inherit get_eligible
#' @inheritParams effective_samplesize
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @keywords internal
get_pooled <- function(
  surveys,
  last_date       = Sys.Date(),
  pollsters       = c("allensbach", "emnid", "forsa", "fgw", "gms",
    "infratest", "dimap", "infratestdimap", "insa"),
  period          = 14,
  period_extended = NA,
  corr            = 0.5,
  weights         = NULL) {

  assert_data_frame(surveys, min.rows = 2, min.cols = 2)
  assert_date(last_date)
  assert_character(pollsters, any.missing = FALSE)
  assert_number(period, lower = 1, finite = TRUE)
  assert_number(period_extended, lower = 1, finite = TRUE, na.ok = TRUE)
  assert_true(period < period_extended, na.ok = TRUE)
  assert_number(corr, lower = -1, upper = 1)
  assert_numeric(weights, finite = TRUE, null.ok = TRUE)


  elg_udf <- surveys %>%
    get_eligible(
      pollsters       = pollsters,
      last_date       = last_date,
      period          = period,
      period_extended = period_extended) %>%
    unnest()

  elg_udf %>%
    filter(!is.na(.data$percent)) %>%
    group_by(.data$party) %>%
    summarize(
      from       = min(date),
      to         = max(date),
      Neff       = effective_samplesize(
        size       = .data$respondents,
        share      = .data$percent / 100,
        corr       = corr,
        weights    = weights),
      pollsters = paste0(.data$pollster, collapse = ", ")) %>%
    ungroup()

}


#' Obtain pooled survey during specified period
#'
#' Per default, pools surveys starting from current date and going 14 days back.
#' For each pollster within the defined time-frame, only the most recent survey
#' is used.
#'
#' @inherit get_pooled
#' @importFrom tidyr unnest
#' @examples
#' library(coalitions)
#' library(dplyr)
#' latest <- get_latest(surveys_sample)
#' pool_surveys(surveys_sample, last_date=as.Date("2017-09-02"))
#' @export
pool_surveys <- function(
  surveys,
  last_date       = Sys.Date(),
  pollsters       = c("allensbach", "emnid", "forsa", "fgw", "gms",
    "infratest", "dimap", "infratestdimap", "insa"),
  period          = 14,
  period_extended = NA,
  corr            = 0.5,
  weights         = NULL) {

  assert_data_frame(surveys, min.rows = 2, min.cols = 2)
  assert_date(last_date)
  assert_character(pollsters, any.missing = FALSE)
  assert_number(period, lower = 1, finite = TRUE)
  assert_number(period_extended, lower = 1, finite = TRUE, na.ok = TRUE)
  assert_true(period < period_extended, na.ok = TRUE)
  assert_number(corr, lower = -1, upper = 1)
  assert_numeric(weights, finite = TRUE, null.ok = TRUE)

  pooled_df <- get_pooled(surveys, last_date, pollsters, period,
                          period_extended, corr, weights)

  elg_udf <- surveys %>%
    get_eligible(
      pollsters       = pollsters,
      last_date       = last_date,
      period          = period,
      period_extended = period_extended) %>%
    unnest() %>%
    filter(!is.na(.data$percent))
  nall <- get_n(elg_udf)

  svotes <- elg_udf %>%
    ungroup() %>%
    group_by(.data$party) %>%
    summarize(votes = sum(.data$votes))

  max_party <- svotes %>%
    filter(.data$votes == max(.data$votes)) %>%
    slice(1) %>%
    pull(.data$party)

  Neff <- pooled_df %>%
    filter(.data$party == max_party) %>%
    pull(Neff)

  svotes %>%
    mutate(
      pollster    = "pooled",
      date        = last_date,
      start       = unique(pooled_df$from),
      end         = unique(pooled_df$to),
      respondents = Neff,
      percent     = .data$votes / nall * 100,
      votes       = .data$percent / 100 * Neff) %>%
    select(one_of("pollster", "date", "start", "end", "respondents", "party",
      "percent", "votes"))

}

#' Total number of survey participants from surveys eligible for pooling.
#'
#' @param eligible_df A data frame containing surveys that should be used for
#' pooling as returned by \code{get_eligible}.
#' @keywords internal
get_n <- function(eligible_df) {

  eligible_df %>%
    ungroup() %>%
    group_by(.data$pollster, date) %>%
    slice(1) %>%
    ungroup() %>%
    pull(.data$respondents) %>%
    sum()

}


#' Pool surveys from different pollsters
#'
#' @inherit pool_surveys
#' @keywords internal
#' @seealso pool_surveys
#' @export
pool_austria <- function(
  ...,
  pollsters=c("Market", "Research Affairs", "Unique Research", "OGM", "IMAS",
    "Hajek", "Gallup", "Karmasin")) {

  pool_surveys(..., pollsters = pollsters)

}
