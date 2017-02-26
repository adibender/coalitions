
#' Sanitize character vectors containing percentages
#' 
#' @param vec Character vector containg percentages that can not be transformed 
#' to numerics b/c of special characters
sanitize_percent <- function(vec) {

	vec <- gsub(" %", "",  vec, fixed=TRUE)
	vec <- gsub("%", "",   vec, fixed=TRUE)
	vec <- gsub("N/A", "", vec, fixed=TRUE)
	vec <- gsub(",", ".",  vec, fixed=TRUE)

	return(as.numeric(vec))

}

#' Sanitize column names 
#' 
#' @param df A data frame with party names with special characters that need 
#' to be sanitized.
sanitize_colnames <- function(df) {

	cdf <- toupper(colnames(df))
	cdf <- sub("CDU/CSU", "CDU", cdf)
	cdf <- sub("GRÜNE", "GRUENE", cdf)
	cdf <- sub("Grünen", "GRUENE", cdf)

	colnames(df) <- cdf

	return(df)

}

#' scrape surveys from wahlrecht.de
#'
#' Scrapes survey tables and perfroms sanitization to output tidy data
#' @rdname scrape
#' @param parties A character vector containing names of parties to collapse.
#' @import rvest dplyr magrittr
#' @importFrom lubridate dmy
#' @importFrom xml2 read_html
#' @importFrom stringr str_sub
#' @export
scrape_wahlrecht <- function(
	adress = "http://www.wahlrecht.de/umfragen/emnid.htm", 
	parties = c("CDU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "FW", "AFD", 
		"SONSTIGE")) {

	atab <- read_html(adress) %>%
		html_nodes("table") %>% .[[2]] %>%
		html_table(fill=TRUE)

	if(adress == "http://www.wahlrecht.de/umfragen/politbarometer/stimmung.htm") {
		
		adress2 <- sub("/stimmung", "", adress)
		atab2 <- read_html(adress2) %>% 
			html_nodes("table") %>% .[[2]] %>% 
			html_table(fill=TRUE) %>% 
			slice(c(-1:-3, -n()))
		# rename to avoid X11 error in package check 
		colnames(atab2) <- sub("X", "V", colnames(atab2))
		atab2 %<>%	select(V1, V11, V12) %>% 
			transmute(
				DATUM    = dmy(V1),
				BEFRAGTE = as.numeric(V11), 
				ZEITRAUM = V12)
		ind.row.remove <- -1:-2
	} else if(adress == "http://www.wahlrecht.de/umfragen/gms.htm") {
		ind.row.remove <- -1:-4
	} else  {
		ind.row.remove <- -1:-3
	}

	atab           <- atab[ind.row.remove,]
	atab           <- atab[-nrow(atab), ]
	ind.empty      <- sapply(atab, function(z) all(z==""))
	atab           <- atab[, !ind.empty]
	colnames(atab) <- c("Datum", colnames(atab)[-1])
	atab <- sanitize_colnames(atab)
	parties <- colnames(atab)[colnames(atab) %in% parties]
	# transform percentage string to numerics
	atab[, parties] <- apply(atab[, parties], 2,  gsub,  pattern=" %",replacement="", fixed=TRUE)
	atab[, parties] <- apply(atab[, parties], 2,  gsub,  pattern="," ,replacement=".", fixed=TRUE)
	atab[, parties] <- apply(atab[, parties], 2,  as.numeric)

	atab <-  mutate(atab, DATUM = dmy(DATUM))

	if(adress == "http://www.wahlrecht.de/umfragen/politbarometer/stimmung.htm") {
		atab <- left_join(atab, atab2)
	} else if(adress == "http://www.wahlrecht.de/umfragen/gms.htm") {
		atab %<>% mutate(
			BEFRAGTE = str_sub(BEFRAGTE, 5, 9),
			BEFRAGTE = as.numeric(BEFRAGTE))
	} else {
		## remove special characters from BEFRAGTE column, transform to numeric
		atab$BEFRAGTE <- gsub("?", "", atab$BEFRAGTE, fixed=TRUE)
		atab$BEFRAGTE <- gsub("≈", "", atab$BEFRAGTE, fixed=TRUE)
		atab$BEFRAGTE <- gsub(".", "", atab$BEFRAGTE, fixed=TRUE)
		atab$BEFRAGTE <- as.numeric(atab$BEFRAGTE)
	}

	atab %<>% mutate(
		START = dmy(paste0(str_sub(ZEITRAUM, 1, 6), str_sub(DATUM, 1, 4))),
		END   = dmy(paste0(str_sub(ZEITRAUM, 8, 13), str_sub(DATUM, 1, 4))))

	atab %<>% mutate(total = rowSums(atab[, parties], na.rm=TRUE)) %>% 
		filter(total==100, !is.na(BEFRAGTE), !is.na(DATUM)) %>% 
		select(one_of(c("DATUM", "START", "END", parties, "BEFRAGTE")))

	return(atab)

}


#' Scrape tables from whalumfragen.de
#'
#' Scrapes table and performs some sanitization to output tidy data
#'
#' @param adress http-Adress from which tables should be scraped
#' @import magrittr rvest dplyr
#' @importFrom lubridate dmy year month
#' @importFrom xml2 read_html
#' @importFrom stats setNames
#' @export
#' @rdname scrape
#' @examples
#' tab <- scrape_wahlumfragen()
#' head(tab)
scrape_wahlumfragen <- function(
	adress="http://www.wahlumfragen.org/bundestagswahl/wahlumfragen_bundestagswahl.php") {


	atab <- read_html(adress) %>%
		html_nodes("table") %>% .[[5]] %>%
		html_table(fill=TRUE) %>%
		select(-Kommentar)
	colnames(atab) <- gsub("ö", "oe", colnames(atab))
	colnames(atab) <- gsub("ä", "ae", colnames(atab))
	colnames(atab) <- gsub("ü", "ue", colnames(atab))
	atab <- rename(atab, Datum=Veroeffentlichung)

	# transform percentage string to numerics
	atab[, 2:9] <- apply(atab[, 2:9], 2, sanitize_percent)

	atab %<>% mutate(
		Datum = dmy(Datum),
		Year  = year(Datum),
		month = month(Datum))


	sanitize_colnames(atab)

}

#' Transform surveys in long format
#' 
#' Given a data frame containing multiple surveys (one row per survey), transforms 
#' the data into long format with one row per party. 
#' @inheritParams scrape_wahlrecht
#' @param surveys A data frame with one survey per row.  
#' @import checkmate
#' @importFrom tidyr gather
#' @return Data frame in long format
#' @export
#' @examples
#' emnid <- scrape_wahlrecht()
#' emnid.long <- collapse_parties(emnid)
collapse_parties <- function(
	surveys, 
	parties = c("CDU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "FW", "AFD", 
		"SONSTIGE")) {

	assert_data_frame(surveys, min.rows=1, min.cols=3, all.missing=FALSE)
	assert_character(parties, any.missing=FALSE, min.len=2, unique=TRUE)
	av.parties <- colnames(surveys)[colnames(surveys) %in% parties]

	surveys <- gather(surveys, PARTY, PERCENT, one_of(av.parties)) %>% 
		arrange(desc(DATUM))

	return(mutate(surveys, PARTY = factor(PARTY, levels=parties)))

}
