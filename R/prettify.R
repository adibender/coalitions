#' Replace/prettify matching words/terms in one vector by another
#'
#' The function searches for \code{x} values, that occur in \code{current}
#' and replaces them with entries in \code{new}. Useful for quick
#' renaming/translation of survey column names and by using internal object
#' \code{.trans_df}
#'
#' @rdname prettify
#' @param x A character vector (or factor) that should be renamed.
#' @param current A vector of characters (possibly subset of \code{x}).
#' Entries in \code{x} that match entries in \code{current} will be renamed
#' according to entries in \code{new}.
#' @param new A vector of characters that will replace entries in \code{x} which
#' have matches in \code{current}.
#' @importFrom purrr map2
#' @keywords internal
#' @examples 
#' library(coalitions)
#' library(dplyr) 
#' # look at sample German federal election polls
#' surveys <- surveys_sample %>% tidyr::unnest() %>% group_by(pollster) %>% slice(1)
#' # prettify the polling agency names
#' prettify_strings(surveys$pollster)
#' prettify_en(surveys$pollster)
#' prettify_de(surveys$pollster)
#' @export
prettify_strings <- function(
  x,
  current = .trans_df$english,
  new     = .trans_df$english_pretty) {

  if (is.factor(x)) {
    was_factor <- TRUE
    x <- as.character(x)
    message("x was converted from factor to character!")
  }

  if (!any(x %in% current)) {
    return(x)
  } else {

    indc     <- match(x, current)
    ind.x    <- which(!is.na(indc))
    ind.repl <- indc[!is.na(indc)]
    x[ind.x] <- new[ind.repl]

    x
  }

}


#' @rdname prettify
#' @inherit prettify_strings
#' @export
prettify_de <- function(x) {
  prettify_strings(x, current = .trans_df$english,
    new = .trans_df$german_pretty)
}

#' @rdname prettify
#' @inherit prettify_strings
#' @export
prettify_en <- function(x) {
  prettify_strings(x, current = .trans_df$english,
    new = .trans_df$english_pretty)
}
