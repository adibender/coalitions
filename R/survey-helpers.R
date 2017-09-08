#' Extract "meta" information from survey data base
#'
#' @param surveys_df A data frame containing surveys from different survey
#' institutes as returned by \code{\link{get_surveys}}.
#' @importFrom dplyr select
#' @importFrom tidyr unnest
#' @keywords internal
#' @export
get_meta <- function(surveys_df) {

	surveys_df %>%
		unnest() %>%
		select(pollster, date:respondents)

}


#' Transform surveys in long format
#'
#' Given a data frame containing multiple surveys (one row per survey), transforms
#' the data into long format with one row per party.
#'
#' @inheritParams scrape_wahlrecht
#' @param surveys A data frame with one survey per row.
#' @import checkmate magrittr dplyr
#' @importFrom tidyr gather nest
#' @importFrom purrr compose
#' @return Data frame in long format
#' @examples
#' emnid <- scrape_wahlrecht()
#' emnid.long <- collapse_parties(emnid)
#' @export
collapse_parties <- function(
	surveys,
	parties = c("cdu", "spd", "greens", "fdp", "left", "pirates", "fw", "afd",
		"others")) {

	assert_data_frame(surveys, min.rows=1, min.cols=3)
	assert_character(parties, any.missing=FALSE, min.len=2, unique=TRUE)

	surveys %<>% select_if(compose("!", all, is.na))
	av.parties <- colnames(surveys)[colnames(surveys) %in% parties]
	surveys <- gather(surveys, party, percent,
			select_vars(names(surveys),one_of(av.parties))) %>%
		arrange(desc(date))

	surveys %>% mutate(votes = percent/100 * respondents) %>%
		filter(!is.na(percent)) %>%
	  as_tibble() %>%
		nest(party:votes, .key="survey")

}


#' Extract latest survey
#'
#' Given a specific date, extract the survey from this date or the last one
#' before this date.
#'
#' @rdname get_surveys
#' @param surveys If provided, lates survey will be obtained from this object,
#' otherwise calls \code{\link{get_surveys}}.
#' @param max_date Specifies the date, relative to which latest survey will
#' be searched for. Defaults to \code{Sys.Date}.
#' @importFrom tidyr unnest
#' @importFrom dplyr filter
get_latest <- function(surveys=NULL, max_date = Sys.Date()) {

	if(is.null(surveys)) {
		surveys <- get_surveys()
	}

	surveys %>%
		unnest() %>%
		filter(date <= as.Date(max_date)) %>%
		filter(date == max(date))

}