#' Sanitize character vectors containing percentages
#' 
#' @param vec Character vector containg percentages that can not be transformed 
#' to numerics b/c of special characters
#' @keywords internal
sanitize_percent <- function(vec) {

	vec <- gsub(" %", "",  vec, fixed=TRUE)
	vec <- gsub("%", "",   vec, fixed=TRUE)
	vec <- gsub("N/A", "", vec, fixed=TRUE)
	vec <- gsub(",", ".",  vec, fixed=TRUE)

	return(as.numeric(vec))

}

#' Sanitze character vector 
#' 
#' Substitute all german "Umlaute"
#' @param x A character vector. 
#' @keywords internal
sanitize_strings <- function(x) {

	# lower case letters 
	x <- gsub("\u00f6", "oe", x)
	x <- gsub("\u00fc", "ue", x)
	x <- gsub("\u00e4", "ae", x)
	# upper case letters 
	x <- gsub("\u00d6", "Oe", x)
	x <- gsub("\u00dc", "Ue", x)
	x <- gsub("\u00c4", "Ae", x)

	return(x)

}

#' Extract numericals from string or character
#' 
#' Removes all characters that are not in [0-9].
#' @param x A character vector.
#' @param decimal Logical flag, indicating if x has a decimal separator
#' @keywords internal
extract_num <- function(x, decimal=TRUE) {

	if(decimal) {
		x <- gsub(",", "\\.", x)
		replace_string <- "[^0-9,.]"
	} else {
		replace_string <- "[^0-9]"
	}
	as.numeric(gsub(replace_string, "", x))
}

#' Sanitize column names 
#' 
#' @param df A data frame with party names with special characters that need 
#' to be sanitized.
#' @keywords internal
sanitize_colnames <- function(df) {

	cdf <- colnames(df)
	cdf <- tolower(sanitize_strings(cdf))
	cdf <- sub("cdu/csu", "cdu", cdf)
	cdf <- sub("gruenen", "gruene", cdf)

	colnames(df) <- cdf

	return(df)

}

#' Scrape surveys from wahlrecht.de
#'
#' Scrapes survey tables and perfroms sanitization to output tidy data
#' @rdname scrape
#' @param address http-address from which tables should be scraped.
#' @param parties A character vector containing names of parties to collapse.
#' @import rvest dplyr magrittr
#' @importFrom lubridate dmy
#' @importFrom xml2 read_html
#' @importFrom stringr str_sub
#' @export
scrape_wahlrecht <- function(
	address = "http://www.wahlrecht.de/umfragen/emnid.htm", 
	parties = c("CDU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "FW", "AFD", 
		"SONSTIGE")) {

	atab <- read_html(address) %>%
		html_nodes("table") %>% .[[2]] %>%
		html_table(fill=TRUE)

	if(address == "http://www.wahlrecht.de/umfragen/politbarometer.htm") {
		colnames(atab) <- atab[2, ]
		ind.row.remove <- -1:-3
	} else if (address == "http://www.wahlrecht.de/umfragen/gms.htm" | 
		address == "http://www.wahlrecht.de/umfragen/insa.htm") {
		ind.row.remove <- -1:-4
	} else { 
		ind.row.remove <- -1:-3
	}

	atab           <- atab[ind.row.remove, ]
	atab           <- atab[-nrow(atab), ]
	colnames(atab) <- c("Datum", colnames(atab)[-1])
	ind.empty      <- sapply(atab, function(z) all(z=="")) |
		sapply(colnames(atab), function(z) z=="")
	atab           <- atab[, !ind.empty]

	atab <- sanitize_colnames(atab)
	parties <- colnames(atab)[colnames(atab) %in% tolower(parties)]
	# transform percentage string to numerics
	atab %<>% mutate_at(c(parties), extract_num) %>% 
		mutate_at("befragte", extract_num, decimal=FALSE)

	atab %<>% mutate(datum = dmy(datum))
	atab %<>% mutate(
		start = dmy(paste0(str_sub(zeitraum, 1, 6), str_sub(datum, 1, 4))),
		end   = dmy(paste0(str_sub(zeitraum, 8, 13), str_sub(datum, 1, 4))))

	atab %<>% 
		mutate(total = rowSums(atab[, parties], na.rm=TRUE)) %>% 
		filter(total==100, !is.na(befragte), !is.na(datum)) %>% 
		select(one_of(c("datum", "start", "end", parties, "befragte")))

	return(atab)

}



#' Scrape surveys from all survey institutes
#' 
#' @import dplyr
#' @importFrom purrr map
#' @export
get_surveys <- function() {

	institutes_df %>% 
		mutate(
			surveys = map(address, scrape_wahlrecht), 
			surveys = map(.x=surveys, collapse_parties)) %>%
		select(-address)

}


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
		select(institute, datum:befragte)

}


#' Transform surveys in long format
#' 
#' Given a data frame containing multiple surveys (one row per survey), transforms 
#' the data into long format with one row per party. 
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
	parties = c("cdu", "spd", "gruene", "fdp", "linke", "piraten", "fw", "afd", 
		"sonstige")) {

	assert_data_frame(surveys, min.rows=1, min.cols=3)
	assert_character(parties, any.missing=FALSE, min.len=2, unique=TRUE)

	surveys %<>% select_if(compose("!", all, is.na))
	av.parties <- colnames(surveys)[colnames(surveys) %in% parties]
	surveys <- gather(surveys, party, percent, select_vars(names(surveys),one_of(av.parties))) %>% 
		arrange(desc(datum))

	surveys %>% mutate(votes = percent/100 * befragte) %>% 
		filter(!is.na(percent)) %>% 
	  as_tibble() %>%
		nest(party:votes, .key="survey")
}