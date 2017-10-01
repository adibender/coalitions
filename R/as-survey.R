#' Creates basic survey table from votes in percent
#'
#' This functions takes votes in percent (per party) obtained from a survey,
#' and returns a table containing votes (in percent) and party names. Conducts
#' sanity checks along the way, such as checking that percentages add up to 1.
#'
#' @param percent Votes in percent each party received in the survey
#'  of interest. Can be set to \code{NA}, if parties are specified that are not
#'  mentioned in the specific survey (otherwise the \code{parties} argument has
#'  to be modified).
#' @param samplesize Number of respondents in survey.
#' @param parties Vector of same length and in the same order as \code{percent}
#' @param epsilon The parameter \code{percent} should add up to one.
#' This parameter controls the maximal numerical divergence allowed.
#' @return A \code{data.frame} containing input and absolute number of votes
#' in survey per party.
#' @examples
#' forsa <- as_survey(
#'  percent    = c(0.41, 0.24, 0.13, 0.04, 0.08, 0.03, 0.03, 0.04),
#'  samplesize = 2508,
#'  parties    = c("cdu/csu", "spd", "gruene", "fdp", "linke", "piraten", "afd", "others"))
#' forsa
#' @seealso \code{\link{redistribute}}
#' @keywords internal
#' @export
as_survey <- function(

  percent,
  samplesize,
  parties = c("cdu", "spd", "gruene", "fdp", "linke", "piraten", "afd", "fw",
    "sonstige"),
  epsilon = 10e-6) {

  ## check data validity
  if (length(percent) != length(parties))
    stop("'percent' and 'parties' arguments must be of same length")
  if (abs(sum(percent, na.rm = TRUE) - 1) > epsilon)
    stop("'percent' don't add up to 1")

  # remove parties not respresented in sample, i.e. percent = NA
  dat <- data.frame(party = parties, percent = percent)
  dat <- dat[!is.na(dat$percent), ]
  dat$votes <- dat$percent * samplesize

  # relevel factor variable
  dat$party <- factor(dat$party, levels = parties)

  # return
  dat

}
