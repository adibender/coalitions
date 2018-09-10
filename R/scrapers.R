#' Sanitize character vector
#'
#' Substitute all German "Umlaute"
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

exctract_num_befragte <- function(x) {

}

#' @importFrom purrr map_dbl
sanitize_befragte <- function(x) {

  x <- gsub(".*\u2022", "", x)
  x <- substr(x, 2, 6)
  x <- map_dbl(x, ~if_else(grepl(".", .x, fixed = TRUE),
    extract_num(.x, decimal = FALSE), extract_num(substr(.x, 1, 3))))

}

#' @importFrom stringr regex str_extract_all
sanitize_sonstige <- function(x) {

  q_ind <- grepl("?", x, fixed = TRUE)
  x <- map(x, ~str_extract_all(.x, regex("[0-9],?[0-9]?")))
  x <- map(x, ~map(.x, extract_num))
  x <- map(x, ~sum(unlist(.x)))
  x[q_ind] <- NA

  x

}

#' Extract numerics from string or character
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
#' Scrapes survey tables and performs sanitation to output tidy data
#' @rdname scrape
#' @param address http-address from which tables should be scraped.
#' @param parties A character vector containing names of parties to collapse.
#' @import rvest dplyr
#' @importFrom lubridate dmy
#' @importFrom xml2 read_html
#' @importFrom stringr str_sub
#' @importFrom rlang .data
#' @examples
#' library(coalitions)
#' library(dplyr)
#' # select a polling agency from .pollster_df that should be scraped ...
#' coalitions:::.pollster_df
#' # ... here we choose Forsa
#' address <- coalitions:::.pollster_df %>% filter(pollster == "forsa") %>% pull(address)
#' scrape_wahlrecht(address = address) %>% slice(1:5)
#' @export
scrape_wahlrecht <- function(
  address = "https://www.wahlrecht.de/umfragen/emnid.htm",
  parties = c("CDU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "FW", "AFD",
    "SONSTIGE")) {

  atab <- read_html(address) %>%
    html_nodes("table") %>% .[[2]] %>%
    html_table(fill = TRUE)

  if (address == "https://www.wahlrecht.de/umfragen/politbarometer.htm") {
    colnames(atab) <- atab[2, ]
    ind_row_remove <- -1:-3
  } else if ( address == "https://www.wahlrecht.de/umfragen/gms.htm" |
    address == "https://www.wahlrecht.de/umfragen/insa.htm" ) {
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

  atab    <- sanitize_colnames(atab)
  parties <- colnames(atab)[colnames(atab) %in% tolower(parties)]
  # transform percentage string to numerics
  atab <- atab %>%
    mutate_at(c(parties), extract_num) %>%
    mutate_at("befragte", extract_num, decimal = FALSE)

  atab <- mutate(atab, datum = dmy(.data$datum))
  atab <- atab %>%
    mutate(
      start = dmy(paste0(str_sub(.data$zeitraum, 1, 6),
        str_sub(.data$datum, 1, 4))),
      end   = dmy(paste0(str_sub(.data$zeitraum, 8, 13),
        str_sub(.data$datum, 1, 4))),
      end   = case_when(
        is.na(end) ~ start,
        TRUE ~ end))

  atab <- atab %>%
    mutate(total = rowSums(atab[, parties], na.rm = TRUE)) %>%
    filter(.data$total == 100, !is.na(.data$befragte), !is.na(.data$datum)) %>%
    select(one_of(c("datum", "start", "end", parties, "befragte")))

  colnames(atab) <- prettify_strings(
    colnames(atab),
    current = .trans_df$german,
    new     = .trans_df$english)

  return(atab)

}

#' Scrape surveys from all pollsters
#'
#' @rdname get_surveys
#' @param country Choose country from which surveys should be scraped.
#' Currently \code{"DE"} (Germany) and \code{"AT"} (Austria) are supported.
#' @import dplyr
#' @importFrom purrr map
#' @examples
#' library(coalitions)
#' # scrape data for the German federal election
#' # get_surveys()
#' @export
get_surveys <- function(country = c("DE", "AT")) {

  country <- match.arg(country)

  if (country == "DE") {
    surveys <- .pollster_df %>%
    mutate(
      surveys = map(.data$address, scrape_wahlrecht),
      surveys = map(.x = surveys, collapse_parties)) %>%
    select(-one_of("address"))
  }
  if (country == "AT") {
    surveys <- scrape_austria()
  }

  surveys

}

#' Scrape Bavarian regional polls
#'
#' @rdname scrape
#' @inherit scrape_wahlrecht
#' @export
scrape_by <- function(
  address = "https://www.wahlrecht.de/umfragen/landtage/bayern.htm",
  parties = c("CSU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "FW", "AFD",
              "SONSTIGE")) {

  atab <- read_html(address) %>%
    html_nodes("table") %>% .[[2]] %>%
    html_table(fill = TRUE)

  ind_row_remove <- -c(1)

  atab <- atab[ind_row_remove, ]
  atab <- atab[-nrow(atab), ]
  atab <- atab[, -2]

  atab$Befragte <- sapply(atab$Befragte, function(x) {
    startchar <- ifelse(grepl("TOM",x),7,5)
    endchar <- ifelse(grepl("TOM",x),11,9)
    extract_num(substr(x, startchar, endchar), decimal = FALSE)
  }, USE.NAMES = FALSE)
  ind.empty     <- sapply(atab, function(z) all(z == "")) |
    sapply(colnames(atab), function(z) z == "")
  atab          <- atab[, !ind.empty]

  atab    <- sanitize_colnames(atab)
  parties <- colnames(atab)[colnames(atab) %in% tolower(parties)]
  # transform percentage string to numerics
  atab <- atab %>%
    mutate_at(c(parties), extract_num) %>%
    mutate_at("befragte", extract_num, decimal = FALSE)

  atab <- atab %>%
    mutate(
      datum = dmy(.data$datum)) %>%
    mutate(
      start = .data$datum,
      end   = .data$datum)
  atab <- atab %>%
    mutate(total = rowSums(atab[, parties], na.rm = TRUE)) %>%
    filter(.data$total == 100, !is.na(.data$befragte), !is.na(.data$datum)) %>%
    select(one_of(c("institut", "datum", "start", "end", parties, "befragte"))) %>%
    rename(pollster = "institut") %>%
    mutate(
      pollster = tolower(.data$pollster),
      pollster = case_when(
        .data$pollster == "forschungs-gruppe wahlen" ~ "fgw",
        TRUE                                   ~ .data$pollster))

  colnames(atab) <- prettify_strings(
    colnames(atab),
    current = .trans_df$german,
    new     = .trans_df$english)

  return(atab)

}

#' Obtain (nested) Bavaria surveys object
#'
#' Scrapes data from \url{wahlrecht.de} and performs some sanitizing.
#'
#' @rdname get_surveys
#' @importFrom tidyr nest
#' @export
get_surveys_by <- function() {

  by <- scrape_by()
  by %>%
    collapse_parties(parties = c("csu","spd","greens","fdp","left","pirates","fw","afd","others")) %>%
    nest(-one_of("pollster"), .key = "surveys")

}

#' Scrape Lower Saxony regional polls
#'
#' @rdname scrape
#' @inherit scrape_wahlrecht
#' @param ind_row_remove Negative vector of rows that will be skipped at the beginning.
#' @export
#' @examples
#' # Niedersachsen
#' scrape_ltw() %>% slice(1:5)
#' # Hessen
#' scrape_ltw("http://www.wahlrecht.de/umfragen/landtage/hessen.htm", ind_row_remove=-c(1)) %>%
#'  slice(1:5)
scrape_ltw <- function(
  address = "https://www.wahlrecht.de/umfragen/landtage/niedersachsen.htm",
  parties = c("CDU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "FW", "AFD",
    "SONSTIGE"),
  ind_row_remove = -c(1:2)) {

  atab <- read_html(address) %>%
    html_nodes("table") %>% .[[2]] %>%
    html_table(fill = TRUE)

  atab <- atab[ind_row_remove, ]
  atab <- atab[-nrow(atab), ]
  atab <- atab[, -2]

  atab$Befragte <- sanitize_befragte(atab$Befragte)
  ind.empty     <- sapply(atab, function(z) all(z == "")) |
    sapply(colnames(atab), function(z) z == "")
  atab          <- atab[, !ind.empty]

  atab    <- sanitize_colnames(atab)
  parties <- colnames(atab)[colnames(atab) %in% tolower(parties)]
  # transform percentage string to numerics
  atab <- atab %>%
    mutate(sonstige = sanitize_sonstige(.data$sonstige)) %>%
    mutate_at(c(parties), extract_num) %>%
    mutate_at("befragte", extract_num, decimal = FALSE)

  atab <- atab %>%
    mutate(
      datum = dmy(.data$datum)) %>%
    mutate(
      start = .data$datum,
      end   = .data$datum)
  atab <- atab %>%
    mutate(total = rowSums(atab[, parties], na.rm = TRUE)) %>%
    filter(.data$total == 100, !is.na(.data$befragte), !is.na(.data$datum)) %>%
    select(one_of(c("institut", "datum", "start", "end", parties, "befragte"))) %>%
    rename(pollster = "institut") %>%
      mutate(
        pollster = tolower(.data$pollster),
        pollster = case_when(
          .data$pollster == "forschungs-gruppe wahlen" ~ "fgw",
          TRUE                                         ~ .data$pollster))

  colnames(atab) <- prettify_strings(
    colnames(atab),
    current = .trans_df$german,
    new     = .trans_df$english)

  return(atab)

}

#' Obtain (nested) Lower Saxony surveys object
#'
#' Scrapes data from \url{wahlrecht.de} and performs some sanitizing.
#'
#' @rdname get_surveys
#' @importFrom tidyr nest
#' @export
get_surveys_nds <- function() {

  nds <- scrape_ltw()
  nds %>%
    collapse_parties() %>%
    nest(-one_of("pollster"), .key = "surveys")

}

#' Import Austrian survey results
#'
#' Reads JSON file from neuwal.com
#' @param address URL of the JSON file
#' @import dplyr
#' @importFrom tidyr nest unnest
#' @importFrom purrr map map_dfr flatten_dfc
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl getURL
#' @importFrom forcats fct_collapse
#' @importFrom lubridate dmy
#' @importFrom rlang UQS syms .data
#' @importFrom stringr str_replace
#' @export
scrape_austria <- function(
  address = "https://neuwal.com/wahlumfragen/openwal/neuwal-openwal.json") {

  aut_list <- fromJSON(getURL(address) %>%
                           str_replace('\\"\\"(.*)\\"\\",', "\"'\\1'\",")) # fix for double double-quote bug
  out_df   <- as_tibble(aut_list) %>%
    rename(survey = "results") %>%
    select(one_of(c("institute", "date", "n",  "survey"))) %>%
    unnest() %>%
    rename(pollster = "institute", respondents = "n", party = "partyName",
      percent = "percentage") %>%
    mutate(
      votes = .data$respondents * .data$percent / 100,
      date  = dmy(.data$date)) %>%
    mutate(
      start = .data$date,
      end   = .data$date,
      party = fct_collapse(.data$party, others = c("? ", "So"))) %>%
    mutate(party = as.character(.data$party)) %>%
    group_by(UQS(
      syms(c("pollster", "respondents", "date", "start", "end", "party")))) %>%
    summarize(
      percent = sum(.data$percent),
      votes   = sum(.data$votes)) %>%
    ungroup() %>%
    nest(one_of(c("party", "percent", "votes")), .key = "survey") %>%
    nest(-one_of("pollster"), .key = "surveys")

}
