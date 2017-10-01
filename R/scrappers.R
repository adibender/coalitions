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

  if (decimal) {
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
    html_table(fill = TRUE)

  if (address == "http://www.wahlrecht.de/umfragen/politbarometer.htm") {
    colnames(atab) <- atab[2, ]
    ind_row_remove <- -1:-3
  } else if ( address == "http://www.wahlrecht.de/umfragen/gms.htm" |
    address == "http://www.wahlrecht.de/umfragen/insa.htm" ) {
    ind_row_remove <- -1:-4
  } else {
    ind_row_remove <- -1:-3
  }

  atab           <- atab[ind_row_remove, ]
  atab           <- atab[-nrow(atab), ]
  colnames(atab) <- c("Datum", colnames(atab)[-1])
  ind.empty      <- sapply(atab, function(z) all(z == "")) |
    sapply(colnames(atab), function(z) z == "")
  atab           <- atab[, !ind.empty]

  atab <- sanitize_colnames(atab)
  parties <- colnames(atab)[colnames(atab) %in% tolower(parties)]
  # transform percentage string to numerics
  atab %<>% mutate_at(c(parties), extract_num) %>%
    mutate_at("befragte", extract_num, decimal = FALSE)

  atab <- mutate(atab, datum = dmy(datum))
  atab %<>% mutate(
    start = dmy(paste0(str_sub(zeitraum, 1, 6), str_sub(datum, 1, 4))),
    end   = dmy(paste0(str_sub(zeitraum, 8, 13), str_sub(datum, 1, 4))))

  atab %<>%
    mutate(total = rowSums(atab[, parties], na.rm = TRUE)) %>%
    filter(total == 100, !is.na(befragte), !is.na(datum)) %>%
    select(one_of(c("datum", "start", "end", parties, "befragte")))

  colnames(atab) <- prettify_strings(
    colnames(atab),
    current = .trans_df$german,
    new     = .trans_df$english)

  return(atab)

}

#' Scrape surveys from all survey institutes
#'
#' @import dplyr
#' @importFrom purrr map
#' @export
get_surveys <- function() {

  .pollster_df %>%
    mutate(
      surveys = map(address, scrape_wahlrecht),
      surveys = map(.x = surveys, collapse_parties)) %>%
    select(-address)

}


#' @rdname scrape
#' @inherit scrape_wahlrecht
#' @export
scrape_ltw <- function(
  address = "http://www.wahlrecht.de/umfragen/landtage/niedersachsen.htm",
  parties = c("CDU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "FW", "AFD",
    "SONSTIGE")) {

  atab <- read_html(address) %>%
    html_nodes("table") %>% .[[2]] %>%
    html_table(fill = TRUE)

  ind_row_remove <- -c(1:2)

  atab <- atab[ind_row_remove, ]
  atab <- atab[-nrow(atab), ]
  atab <- atab[, -2]

  atab$Befragte <- extract_num(substr(atab$Befragte, 5, 9), decimal = FALSE)
  ind.empty     <- sapply(atab, function(z) all(z == "")) |
    sapply(colnames(atab), function(z) z == "")
  atab          <- atab[, !ind.empty]

  atab <- sanitize_colnames(atab)
  parties <- colnames(atab)[colnames(atab) %in% tolower(parties)]
  # transform percentage string to numerics
  atab %<>% mutate_at(c(parties), extract_num) %>%
    mutate_at("befragte", extract_num, decimal = FALSE)

  atab <- mutate(atab, datum = dmy(datum))
  atab %<>%
    mutate(total = rowSums(atab[, parties], na.rm = TRUE)) %>%
    filter(total == 100, !is.na(befragte), !is.na(datum)) %>%
    select(one_of(c("institut", "datum", parties, "befragte"))) %>%
    rename(pollster = "institut")

  colnames(atab) <- prettify_strings(
    colnames(atab),
    current = .trans_df$german,
    new     = .trans_df$english)

  return(atab)


}
