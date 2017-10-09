#' Calculate the effective sample size
#'
#' This is the work horse function that calculates the effective sample size.
#' Should usually not be called by the user directly.
#'
#' @param size A vecotr of sample sizes from different surveys (from different
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

#' Extract surveys from insitutes within a specfied time-window
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
#' @import dplyr checkmate
#' @importFrom magrittr "%<>%"
#' @keywords internal
get_eligible <- function(
  surveys,
  pollsters = c("allensbach", "emnid", "forsa", "fgw", "gms", "infratest",
    "insa"),
  last_date = Sys.Date(),
  period    = 14) {

  assert_data_frame(surveys, min.rows = 2, min.cols = 2)
  assert_date(last_date)
  assert_character(pollsters, null.ok = TRUE)
  assert_number(period, lower = 1, finite = TRUE)

  surveys %>% filter(pollster %in% pollsters) %>%
    unnest(surveys) %>%
    filter(date >= last_date - period & date <= last_date) %>%
    group_by(pollster) %>%
    filter(date == max(date))

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
#' @keywords internal
get_pooled <- function(
  surveys,
  last_date  = Sys.Date(),
  pollsters = c("allensbach", "emnid", "forsa", "fgw", "gms", "infratest",
    "insa"),
  period     = 14,
  corr       = 0.5,
  weights    = NULL) {

  assert_data_frame(surveys, min.rows = 2, min.cols = 2)
  assert_date(last_date)
  assert_character(pollsters, any.missing = FALSE)
  assert_number(period, lower = 1, finite = TRUE)
  assert_number(corr, lower = -1, upper = 1)
  assert_numeric(weights, finite = TRUE, null.ok = TRUE)


  elg_udf <- surveys %>%
    get_eligible(
      pollsters = pollsters,
      last_date  = last_date,
      period     = period) %>%
    unnest()

  elg_udf %>%
    filter(!is.na(percent)) %>%
    group_by(party) %>%
    summarize(
      from       = min(date),
      to         = max(date),
      Neff       = effective_samplesize(
        size       = respondents,
        share      = percent / 100,
        corr       = corr,
        weights    = weights),
      pollsters = paste0(pollster, collapse = ", ")) %>%
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
  last_date  = Sys.Date(),
  pollsters = c("allensbach", "emnid", "forsa", "fgw", "gms", "infratest",
    "insa"),
  period     = 14,
  corr       = 0.5,
  weights    = NULL) {

  assert_data_frame(surveys, min.rows = 2, min.cols = 2)
  assert_date(last_date)
  assert_character(pollsters, any.missing = FALSE)
  assert_number(period, lower = 1, finite = TRUE)
  assert_number(corr, lower = -1, upper = 1)
  assert_numeric(weights, finite = TRUE, null.ok = TRUE)

  pooled_df <- get_pooled(surveys, last_date, pollsters, period, corr, weights)

  elg_udf <- surveys %>%
    get_eligible(
      pollsters = pollsters,
      last_date = last_date,
      period    = period) %>%
    unnest() %>%
    filter(!is.na(percent))
  nall <- get_n(elg_udf)

  svotes <- elg_udf %>%
    ungroup() %>%
    group_by(party) %>%
    summarize(votes = sum(votes))

  max_party <- svotes %>%
    filter(votes == max(votes)) %>%
    slice(1) %>%
    pull(party)

  Neff <- pooled_df %>%
    filter(party == max_party) %>%
    pull(Neff)

  svotes %>%
    mutate(
      pollster    = "pooled",
      date        = last_date,
      start       = unique(pooled_df$from),
      end         = unique(pooled_df$to),
      respondents = Neff,
      percent     = votes / nall * 100,
      votes       = percent / 100 * Neff) %>%
    select(one_of("pollster", "date", "start", "end", "respondents", "party",
      "percent", "votes"))

}

#' Total number of survey participants from surveys elligible for pooling.
#'
#' @param eligible_df A data frame containing surveys that should be used for
#' pooling as returned by \code{get_eligible}.
#' @keywords internal
get_n <- function(eligible_df) {

  eligible_df %>%
    group_by(pollster, date) %>%
    slice(1) %>%
    ungroup() %>%
    pull(respondents) %>%
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
