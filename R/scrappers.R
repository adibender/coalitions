# @Author: andreas.bender@stat.uni-muenchen.de
# @Date:   2017-02-14 13:41:16
# @Last Modified by:   andreas.bender@stat.uni-muenchen.de
# @Last Modified time: 2017-02-15 14:19:07



sanitize_percent <- function(vec) {

	vec <- gsub(" %", "", vec, fixed=TRUE)
	vec <- gsub("%", "", vec, fixed=TRUE)
	vec <- gsub("N/A", "", vec, fixed=TRUE)
	vec <- gsub(",", ".", vec, fixed=TRUE)

	return(as.numeric(vec))

}

sanitize_colnames <- function(df) {

	cdf <- toupper(colnames(df))
	cdf <- sub("CDU/CSU", "CDU", cdf)
	cdf <- sub("GRÜNE", "GRUENE", cdf)
	cdf <- sub("Grünnen", "GRUENE", cdf)

	colnames(df) <- cdf

	return(df)

}


#' scrape surveys from wahlrecht.de
#' 
#' Scrapes survey tables and perfroms sanitization to output tidy data 
#' @rdname scrape
#' @import rvest dplyr
#' @importFrom lubridate dmy year month
#' @importFrom xml2 read_html
#' @export
scrape_wahlrecht <- function(adress = "http://www.wahlrecht.de/umfragen/emnid.htm") {

	atab <- read_html(adress) %>% 
		html_nodes("table") %>% .[[2]] %>% 
		html_table(fill=TRUE)

	atab <- atab[-1:-3,]
	atab <- atab[-nrow(atab), ]
	ind.empty <- sapply(atab, function(z) all(z==""))
	atab <- atab[, !ind.empty]
	colnames(atab) <- c("Datum", colnames(atab)[-1])
	# transform percentage string to numerics
	atab[, 2:8] <- apply(atab[, 2:8], 2,  gsub,  pattern=" %",replacement="", fixed=TRUE)
	atab[, 2:8] <- apply(atab[, 2:8], 2,  gsub,  pattern=",",replacement=".", fixed=TRUE)
	atab[, 2:8] <- apply(atab[, 2:8], 2,  as.numeric)

	## remove special characters from befragte column, transform to numeric
	atab$Befragte <- gsub("?", "", atab$Befragte, fixed=TRUE)
	atab$Befragte <- gsub("≈", "", atab$Befragte, fixed=TRUE)
	atab$Befragte <- gsub(".", "", atab$Befragte, fixed=TRUE)
	atab$Befragte <- as.numeric(atab$Befragte)

	atab %>% mutate(
		Datum = dmy(Datum),
		Year  = year(Datum),
		month = month(Datum))

	return(sanitize_colnames(atab))
	
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
#'library(dplyr)
#'library(tidyr)
#'library(plotly)
#'library(ggplot2)
#'theme_set(theme_bw())
#'tab <- scrape_wahlumfragen()
#'ltab <- gather(tab, Partei, Prozent, CDU:SONSTIGE) %>% 
#'	filter(YEAR >= 2016, INSTITUT %in% c("Allensbach", "Emnid", "Forsa", "INSA"))
#'
#'gg.umfragen <- ggplot(ltab, aes(x=DATUM, y=Prozent)) +
#'	geom_point(aes(col=Partei), size=0.5) + 
#'	geom_path(aes(col=Partei)) + 
#'	facet_wrap(~INSTITUT)
#'gg.umfragen

#'ggplotly(gg.umfragen)
#'
scrape_wahlumfragen <- function(
	adress="http://www.wahlumfragen.org/bundestagswahl/wahlumfragen_bundestagswahl.php") {


	atab <- read_html(adress) %>% 
		html_nodes("table") %>% .[[5]] %>% 
		html_table(fill=TRUE) %>% 
		select(-Kommentar) %>% 
		rename_(.dots=setNames("Veröffentlichung", "Datum"))

	# transform percentage string to numerics
	atab[, 2:9] <- apply(atab[, 2:9], 2, sanitize_percent)

	atab %<>% mutate(
		Datum = dmy(Datum),
		Year  = year(Datum),
		month = month(Datum))


	sanitize_colnames(atab)

}
