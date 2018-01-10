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
#' @import rvest dplyr
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

  atab    <- sanitize_colnames(atab)
  parties <- colnames(atab)[colnames(atab) %in% tolower(parties)]
  # transform percentage string to numerics
  atab <- atab %>%
    mutate_at(c(parties), extract_num) %>%
    mutate_at("befragte", extract_num, decimal = FALSE)

  atab <- mutate(atab, datum = dmy(datum))
  atab <- atab %>%
    mutate(
      start = dmy(paste0(str_sub(zeitraum, 1, 6), str_sub(datum, 1, 4))),
      end   = dmy(paste0(str_sub(zeitraum, 8, 13), str_sub(datum, 1, 4))),
      end   = case_when(
        is.na(end) ~ start,
        TRUE ~ end))

  atab <- atab %>%
    mutate(total = rowSums(atab[, parties], na.rm = TRUE)) %>%
    filter(total == 100, !is.na(befragte), !is.na(datum)) %>%
    select(one_of(c("datum", "start", "end", parties, "befragte")))

  colnames(atab) <- prettify_strings(
    colnames(atab),
    current = .trans_df$german,
    new     = .trans_df$english)

  return(atab)

}

#' Scrape surveys from all pollsters
#'
#' @param country Choose country from which surveys should be scrapped.
#' Currently \code{"DE"} (Germany) and \code{"AT"} (Austria) are supported.
#' @import dplyr
#' @importFrom purrr map
#' @export
get_surveys <- function(country = c("DE", "AT")) {

  country <- match.arg(country)

  if (country == "DE") {
    surveys <- .pollster_df %>%
    mutate(
      surveys = map(address, scrape_wahlrecht),
      surveys = map(.x = surveys, collapse_parties)) %>%
    select(-address)
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
  address = "http://www.wahlrecht.de/umfragen/landtage/bayern.htm",
  parties = c("CSU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "FW", "AFD",
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
  
  atab    <- sanitize_colnames(atab)
  parties <- colnames(atab)[colnames(atab) %in% tolower(parties)]
  # transform percentage string to numerics
  atab <- atab %>%
    mutate_at(c(parties), extract_num) %>%
    mutate_at("befragte", extract_num, decimal = FALSE)
  
  atab <- atab %>%
    mutate(
      datum = dmy(datum)) %>%
    mutate(
      start = datum,
      end   = datum)
  atab <- atab %>%
    mutate(total = rowSums(atab[, parties], na.rm = TRUE)) %>%
    filter(total == 100, !is.na(befragte), !is.na(datum)) %>%
    select(one_of(c("institut", "datum", "start", "end", parties, "befragte"))) %>%
    rename(pollster = "institut") %>%
    mutate(
      pollster = tolower(pollster),
      pollster = case_when(
        pollster == "forschungs-gruppe wahlen" ~ "fgw",
        TRUE                                   ~ pollster))
  
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
#' @importFrom tidyr nest
#' @export
get_surveys_by <- function() {
  
  by <- scrape_by()
  by %>%
    collapse_parties(parties = c("csu","spd","greens","fdp","left","pirates","fw","afd","others")) %>%
    nest(-pollster, .key = "surveys")
  
}

#' Scrape regional polls
#'
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

  atab    <- sanitize_colnames(atab)
  parties <- colnames(atab)[colnames(atab) %in% tolower(parties)]
  # transform percentage string to numerics
  atab <- atab %>%
    mutate_at(c(parties), extract_num) %>%
    mutate_at("befragte", extract_num, decimal = FALSE)

  atab <- atab %>%
    mutate(
      datum = dmy(datum)) %>%
    mutate(
      start = datum,
      end   = datum)
  atab <- atab %>%
    mutate(total = rowSums(atab[, parties], na.rm = TRUE)) %>%
    filter(total == 100, !is.na(befragte), !is.na(datum)) %>%
    select(one_of(c("institut", "datum", "start", "end", parties, "befragte"))) %>%
    rename(pollster = "institut") %>%
      mutate(
        pollster = tolower(pollster),
        pollster = case_when(
          pollster == "forschungs-gruppe wahlen" ~ "fgw",
          TRUE                                   ~ pollster))

  colnames(atab) <- prettify_strings(
    colnames(atab),
    current = .trans_df$german,
    new     = .trans_df$english)

  return(atab)

}

#' Obtain (nested) Niedersachsen surveys object
#'
#' Scrapes data from \url{wahlrecht.de} and performs some sanitizing.
#'
#' @importFrom tidyr nest
#' @export
get_surveys_nds <- function() {

  nds <- scrape_ltw()
  nds %>%
    collapse_parties() %>%
    nest(-pollster, .key = "surveys")

}

#' Import austrian survey results
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
#' @importFrom rlang UQS syms
#' @export
scrape_austria <- function(
  address = "https://neuwal.com/wahlumfragen/openwal/neuwal-openwal.json") {

  aut_list <- fromJSON(getURL(address))
  out_df   <- as_tibble(aut_list) %>%
    rename(survey="results") %>%
    select(one_of(c("institute", "date", "n",  "survey"))) %>%
    unnest() %>%
    rename(pollster = "institute", respondents = "n", party = "partyName",
      percent = "percentage") %>%
    mutate(
      votes = respondents * percent / 100,
      date  = dmy(date)) %>%
    mutate(
      start = date,
      end   = date,
      party = fct_collapse(party, others = c("? ", "So"))) %>%
    mutate(party = as.character(party)) %>%
    group_by(UQS(
      syms(c("pollster", "respondents", "date", "start", "end", "party")))) %>%
    summarize(
      percent = sum(percent),
      votes   = sum(votes)) %>%
    ungroup() %>%
    nest(party:votes, .key = "survey") %>%
    nest(-pollster,   .key = "surveys")

}
