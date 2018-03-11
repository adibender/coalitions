#' Sample of selected surveys
#'
#' A data set with surveys from seven different pollsters, three surveys per
#' pollster. Surveys report support for different parties in the running
#' for the German Bundestag prior to the 2017 election.
#'
#' @format A nested data frame with 7 rows and 2 columns:
#' \describe{
#'   \item{institute}{name of the pollster}
#'   \item{surveys}{a list of data frames, each containing one survey}
#' }
#' @source \url{https://www.wahlrecht.de/}
"surveys_sample"


#' Colors of German parties
#'
#' A vector of colors associated with German parties.
#'
#' @format A named character vector. Names indicate parties. Values contain
#' color strings for the respective parties
"party_colors_de"
