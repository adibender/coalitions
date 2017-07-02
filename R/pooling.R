#' Calculate the effective sample size 
#' 
#' This is the work horse function that calculates the effective sample size. 
#' Should usually not be called by the user directly. 
#' 
#' @param size A vecotr of sample sizes from different surveys (from different 
#' institutes) for one party.
#' @param share The relative share of votes for party of interest ([0-1])
#' @param corr Assumed correlation between surveys (of different institutes). 
#' Defaults to 0.5.
#' @param weights Additional weights for individual surveys. 
effective_samplesize <- function(
  size, 
  share, 
  corr = 0.5, 
  weights = NULL){

  assert_numeric(size, lower=0, finite=TRUE, min.len = 1)
  if (length(size) == 1) {
    message("Only one survey/institute provided. The provided sample size will be returned.")
    return(size)
  }
  assert_numeric(share, lower=0, upper=1, len = length(size))
  assert_number(corr, lower=-1, upper=1)
  assert_numeric(weights, finite=TRUE, len = length(size), null.ok=TRUE)
  if (is.null(weights)) {
    weights <- size
  }

  # calculation
  p.total = sum(weights*share)/sum(weights)
  var.ind = p.total*(1-p.total)
  n.inst  = length(size)                                            
  n.total = sum(size)           
  var.vec = share*(1-share)/size
  sd.vec  = sqrt(var.vec)       
  n.comb  = 0                   
  for (i in (n.inst-1):1){
    n.comb = n.comb + i
  }
  cov.vec   = rep(NA, n.comb)   
  n.cov.vec = cov.vec           
  k = n.inst-1
  count = 1
  while (k > 0){
    cov.vec[count:(count+k-1)]   = corr*sd.vec[1:k]*sd.vec[(n.inst-k+1):n.inst]
    n.cov.vec[count:(count+k-1)] = weights[1:k]*weights[(n.inst-k+1):n.inst]
    count = count+k
    k = k-1
  }
  var.est = 1/sum(weights)^2*(sum((weights^2)*var.vec) + sum(2*n.cov.vec*cov.vec))
  n.eff   = var.ind/var.est
  return(n.eff)

}


#' @inherit effective_samplesize
#' @importFrom stats optim
optim_eff <- function(size, share, corr) {
  max.start <- rep(1/length(size), length(size)-1)
  opt_res <- optim(
    max.start, 
    function(x) {
      -effective_samplesize(
        size   = size,
        share  = share,
        corr   = corr,
        weights = c(x,1-sum(x)))
    }, 
    method = "L-BFGS-B",
    lower  = rep(0, length(size)-1),
    upper  = rep(1, length(size)-1))

  -opt_res$value
  
}



#' Extract surveys from insitutes within a specfied time-window
#' 
#' 
#' @param surveys A \code{tibble} containing survey results for multiple 
#' institutes as returned by \code{\link[coalitions]{get_surveys}}.
#' @param institutes Character vector of institutes that should be considered
#' for pooling.
#' @param last_date Only surveys in the time-window from \code{last_date} to 
#' \code{last_date} - period will be considered for each institute. Defaults 
#' to current date. 
#' @param period See \code{last_date} argument.
#' @import dplyr checkmate
#' @importFrom magrittr "%<>%"
get_eligible <- function(
  surveys, 
  institutes   = c("allensbach", "emnid", "forsa", "fgw", "gms", "infratest", "insa"),
  last_date = Sys.Date(),
  period    = 14) {

  assert_data_frame(surveys, min.rows=2, min.cols=2)
  assert_date(last_date)
  assert_character(institutes, null.ok = TRUE)
  assert_number(period, lower=1, finite=TRUE)

  surveys %>% filter(institute %in% institutes) %>% 
    unnest(surveys) %>% 
    filter(datum >= last_date - period & datum <= last_date) %>%
    group_by(institute) %>% 
    filter(datum == max(datum))

}


#' Extract effective sample size for pooled sample 
#' 
#' Given a specified time window (defaults to current day - 14 days). 
#' calculate the effective sample size of the pooled sample over multiple 
#' institutes. 
#' 
#' @inherit get_eligible
#' @inheritParams effective_samplesize
#' @importFrom tidyr unnest
get_pooled <- function(
  surveys, 
  last_date  = Sys.Date(),
  institutes = c("allensbach", "emnid", "forsa", "fgw", "gms", "infratest", "insa"),
  period     = 14,
  corr       = 0.5,
  weights    = NULL) {

  assert_data_frame(surveys, min.rows=2, min.cols=2)
  assert_date(last_date)
  assert_character(institutes, any.missing=FALSE)
  assert_number(period, lower=1, finite=TRUE)
  assert_number(corr, lower=-1, upper=1)
  assert_numeric(weights, finite=TRUE, null.ok=TRUE)


  elg_udf <- surveys %>% 
    get_eligible(
      institutes = institutes,
      last_date  = last_date,
      period     = period) %>%
    unnest()
  
  elg_udf %>% 
    filter(!is.na(percent)) %>% 
    group_by(party) %>% 
    summarize(
      from       = min(datum),
      to         = max(datum),
      Neff       = effective_samplesize(
        size    = befragte,
        share   = percent/100,
        corr    = corr,
        weights = weights), 
      institutes = paste0(institute, collapse = ", ")) %>% 
    ungroup()

}


#' Obtain pooled survey during specified period
#' 
#' Per default, pools surveys starting from current date and going 14 days back. 
#' For each institute within the defined time-frame, only the most recent survey
#' is used. 
#' 
#' @inherit get_pooled
#' @importFrom tidyr unnest
#' @examples 
#' surveys <- get_surveys()
#' pool_surveys(surveys)
#' @export
pool_surveys <- function(
  surveys, 
  last_date  = Sys.Date(),
  institutes = c("allensbach", "emnid", "forsa", "fgw", "gms", "infratest", "insa"),
  period     = 14,
  corr       = 0.5,
  weights    = NULL) {

  assert_data_frame(surveys, min.rows=2, min.cols=2)
  assert_date(last_date)
  assert_character(institutes, any.missing=FALSE)
  assert_number(period, lower=1, finite=TRUE)
  assert_number(corr, lower=-1, upper=1)
  assert_numeric(weights, finite=TRUE, null.ok=TRUE)

  pooled_df <- get_pooled(surveys, last_date, institutes, period, corr, weights) 

  elg_udf <- surveys %>% 
    get_eligible(
      institutes = institutes,
      last_date  = last_date,
      period     = period) %>%
    unnest() %>% 
    filter(!is.na(percent)) 
  nall <- get_n(elg_udf)

  svotes <- elg_udf %>% 
    ungroup() %>%
    group_by(party) %>% 
    summarize(votes = sum(votes)) 

  max_party <- svotes %>% filter(votes == max(votes)) %>% pull(party)

  Neff <- pooled_df %>% 
    filter(party == max_party) %>% 
    pull(Neff)

  svotes %>%
    mutate(
      institute = "pooled",
      datum     = last_date,
      start     = unique(pooled_df$from),
      end       = unique(pooled_df$to),
      befragte  = Neff,
      percent   = votes/nall*100,
      votes     = percent/100 * Neff) %>%
    select(institute, datum, start, end, befragte, party, percent, votes)

}


#' Total number of survey participants from surveys elligible for pooling. 
#' 
#' @param eligible_df A data frame containing surveys that should be used for 
#' pooling as returned by \code{get_eligible}.
get_n <- function(eligible_df) {

  eligible_df %>% 
    group_by(institute, datum) %>% 
    slice(1) %>% 
    ungroup() %>% 
    pull(befragte) %>% sum()

}

