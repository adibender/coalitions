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
  x <- gsub(" ", "", x)
  x <- substr(x, 1, 5)
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

#' Try call of read_html that throws an error if the url cannot be resolved
#'
#' @param url http-address that should be scraped.
#' @importFrom xml2 read_html
try_readHTML <- function(url) {

  html_source <- tryCatch({
    read_html(url)
  }, error = function(cond) {
    message(paste("The URL could not be resolved:", url))
    message("Here's the original error message:")
    stop(cond)
  })

  return(html_source)
}

#' Scrape surveys for German general election
#'
#' Scrapes survey tables and performs sanitation to output tidy data
#' @rdname scrape
#' @param address http-address from which tables should be scraped.
#' @param parties A character vector containing names of parties to collapse.
#' @import rvest dplyr
#' @importFrom lubridate dmy
#' @importFrom stringr str_sub
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' library(coalitions)
#' library(dplyr)
#' # select a polling agency from .pollster_df that should be scraped ...
#' coalitions:::.pollster_df
#' # ... here we choose Forsa
#' address <- coalitions:::.pollster_df %>% filter(pollster == "forsa") %>% pull(address)
#' scrape_wahlrecht(address = address) %>% slice(1:5)
#' }
#' @export
scrape_wahlrecht <- function(
  address = "https://www.wahlrecht.de/umfragen/emnid.htm",
  parties = c("CDU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "FW", "AFD", "BSW",
    "SONSTIGE")) {

  atab <- try_readHTML(address) %>%
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
  
  if (any(nchar(atab$Sonstige) > 6)) {
    # correct the 'Sonstige' column if it contains information on
    # one party + other parties (see issue #138)
    weird_rows <- which(nchar(atab$Sonstige) > 6)
    if (length(weird_rows) > 0) {
      for (row in weird_rows) {
        entry  <- atab$Sonstige[row] %>% 
          gsub(pattern = ",", replacement = ".")
        # include '0' instead of decimal commas s.t. gregexpr correctly finds the beginning of each decimal number
        entry_gregexpr <- gsub(atab$Sonstige[row], pattern = ",", replacement = ".")
        shares <- entry_gregexpr %>% 
          gregexpr("[[:digit:]]+", .) %>% 
          regmatches(entry, .) %>% 
          unlist() %>% 
          as.numeric()
        atab$Sonstige[row] <- paste(sum(shares), "%")
      }
    }
  }
  
  ind.empty      <- sapply(atab, function(z) { all(z == "") | all(is.na(z)) } ) |
    sapply(colnames(atab), function(z) z == "") |
    sapply(colnames(atab), function(z) is.na(z))
  atab           <- atab[, !ind.empty]

  atab    <- sanitize_colnames(atab)
  parties <- colnames(atab)[colnames(atab) %in% tolower(parties)]
  # transform percentage string to numerics
  atab <- atab %>%
    mutate_at(c(parties), extract_num) %>%
    mutate_at("befragte", extract_num, decimal = FALSE)

  atab <- mutate(atab, datum = dmy(.data$datum))
  atab <- mutate(atab, zeitraum = case_when(nchar(zeitraum) == 6 ~ paste0(zeitraum, "-", zeitraum),
                                            TRUE                 ~ zeitraum))
  
  atab <- atab %>%
    filter(.data$zeitraum != "Bundestagswahl",
           !grepl("\\?", .data$zeitraum)) %>%
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
  
  # remove potential duplicate entries
  atab <- atab %>%
    group_by(.data$datum) %>% slice(1) %>% ungroup()
  
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
#' @return Nested tibble. When fully unnested, the dataset contains the following
#' columns:
#' \describe{
#'   \item{pollster}{Character name of the polling institute.}
#'   \item{date}{Publication date of the poll.}
#'   \item{start, end}{Start and end date of the field period, i.e. the dates
#'   during which the poll was conducted.}
#'   \item{respondents}{Number of respondents in the poll.}
#'   \item{party}{Character name of an individual party.}
#'   \item{percent}{Percentage of respondents that chose the party. Given in
#'   percentage points, i.e. \code{38\%} is given as \code{38}.}
#'   \item{votes}{Number of respondents that chose the party.}
#' }
#' @examples
#' \dontrun{
#' library(coalitions)
#' # scrape data for the German federal election
#' # get_surveys()
#' }
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
#' @export
scrape_by <- function(
  address = "https://www.wahlrecht.de/umfragen/landtage/bayern.htm",
  parties = c("CSU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "FW", "AFD",
              "SONSTIGE")) {

  atab <- try_readHTML(address) %>%
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


#' @rdname get_surveys
#' @importFrom tidyr nest
#' @export
get_surveys_by <- function() {

  by <- scrape_by()
  by %>%
    collapse_parties(parties = c("csu","spd","greens","fdp","left","pirates","fw","afd","others")) %>%
    nest(surveys = -one_of("pollster"))

}


#' Scrape Rhineland-Palatinate polls
#'
#' @rdname scrape
#' @param ind_row_remove Negative vector of rows that will be skipped at the beginning.
#' @export
scrape_rp <- function(
  address = "https://www.wahlrecht.de/umfragen/landtage/rheinland-pfalz.htm",
  parties = c("CDU", "SPD", "GRUENE", "FDP", "LINKE", "AFD", "FW", "SONSTIGE"),
  ind_row_remove = -c(1:3)) {

  atab <- try_readHTML(address) %>%
    html_nodes("table") %>% .[[2]] %>%
    html_table(fill = TRUE)

  atab <- atab[ind_row_remove, ]
  atab <- atab[-nrow(atab), ]
  atab <- atab[, -2]
  atab <- atab[, -ncol(atab)] # delete last column (completely NA)

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

#' @rdname get_surveys
#' @importFrom tidyr nest
#' @export
get_surveys_rp <- function() {

  rp <- scrape_rp()
  rp %>%
    collapse_parties() %>%
    nest(surveys = -one_of("pollster"))

}


#' Scrape Lower Saxony regional polls
#'
#' @rdname scrape
#' @param ind_row_remove Negative vector of rows that will be skipped at the beginning.
#' @export
#' @examples
#' \dontrun{
#' # Niedersachsen
#' scrape_ltw() %>% slice(1:5)
#' # Hessen
#' scrape_ltw("https://www.wahlrecht.de/umfragen/landtage/hessen.htm", ind_row_remove=-c(1)) %>%
#'  slice(1:5)
#' }
scrape_ltw <- function(
  address = "https://www.wahlrecht.de/umfragen/landtage/niedersachsen.htm",
  parties = c("CDU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "FW", "AFD", "BSW",
    "SONSTIGE"),
  ind_row_remove = -c(1:2)) {

  atab <- try_readHTML(address) %>%
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


#' @rdname get_surveys
#' @importFrom tidyr nest
#' @export
get_surveys_nds <- function() {

  nds <- scrape_ltw()
  nds %>%
    collapse_parties() %>%
    nest(surveys = -one_of("pollster"))

}


#' @rdname get_surveys
#' @export
get_surveys_saxony <- function() {

  saxony <- scrape_ltw(
    "https://www.wahlrecht.de/umfragen/landtage/sachsen.htm",
    ind_row_remove = -1)
  saxony %>% collapse_parties() %>%
    nest(surveys = -one_of("pollster"))

}

#' @rdname get_surveys
#' @export
get_surveys_brb <- function() {

  brb <- scrape_ltw("https://www.wahlrecht.de/umfragen/landtage/brandenburg.htm")
  brb %>% collapse_parties() %>%
    nest(surveys = -one_of("pollster"))

}

#' @rdname get_surveys
#' @export
get_surveys_thuringen <- function() {

  thuringen <- scrape_ltw(
    "https://www.wahlrecht.de/umfragen/landtage/thueringen.htm",
    ind_row_remove = -1)
  thuringen %>% collapse_parties() %>%
    nest(surveys = -one_of("pollster"))

}


#' Import Austrian survey results
#'
#' Reads JSON file from neuwal.com and performs some preprocessing to bring
#' data into standardized format. Returns a nested tibble.
#'
#'
#' @param address URL of the JSON file.
#' @import dplyr
#' @importFrom tidyr nest unnest
#' @importFrom purrr map map_dfr flatten_dfc
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl getURL
#' @importFrom lubridate dmy
#' @importFrom rlang UQS syms .data
#' @importFrom stringr str_replace
#' @export
scrape_austria <- function(
  address = "https://neuwal.com/wahlumfragen/data/neuwal-wahlumfragen-user.json") {

  raw_input <- getURL(address)
  raw_input <- str_replace(raw_input, '\\"\\"(.*)\\"\\",', "\"'\\1'\",") # fix for double double-quote bug
  aut_list <- fromJSON(raw_input)
  out_df <- as_tibble(aut_list[[1]]) %>%
    filter(.data$regionID == 1)
  party <- out_df %>%
    select(one_of("id"), contains("Party")) %>%
    gather("key", "party", contains("Party")) %>%
    arrange(desc(.data$id)) %>%
    select(-.data$key)
  party <- party %>%
    nest(data = .data$party) %>%
    mutate(data = map(.data$data, ~rbind(.x, data.frame(party = "Others")))) %>%
    unnest("data")
  percent <- out_df %>%
    select(one_of(c("id", "n")), contains("Value")) %>%
    gather("key", "percent", contains("Value")) %>%
    arrange(desc(.data$id)) %>%
    mutate(percent = as.numeric(.data$percent)) %>%
    select(-one_of("key"))
  percent <- percent %>% nest(data = .data$percent) %>%
    mutate(data = map(.data$data, ~rbind(.x, data.frame(percent = 100 - sum(.x$percent))))) %>%
    unnest("data") %>%
    mutate(votes   = .data$n * .data$percent / 100) %>%
    select(-n)
  pp <- cbind(party, percent[, -1]) %>%
    nest(survey = -.data$id)

  out_df <- out_df %>%
    select(one_of(c("id", "institut", "n", "datum"))) %>%
    rename(pollster = "institut", date = "datum", respondents = "n")
  out_df <- out_df %>% left_join(pp)
  out_df %>%
    mutate(
      date  = as.Date(.data$date),
      start = .data$date,
      end   = .data$date) %>%
    select(one_of(c("pollster", "date", "start", "end", "respondents", "survey"))) %>%
    nest(surveys = -.data$pollster)

  }
