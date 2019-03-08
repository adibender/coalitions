#' Plot voter shares observed in one survey
#'
#' Bar chart of the raw voter shares observed in one survey.
#' Additionally to plotting positive voter shares,
#' the function can be used to plot party-specific differences (e.g. between
#' a survey and the election result), including negative numbers.
#'
#' @param data Scraped dataset containing one row per party in the column
#' \code{party} and the observed voter share in the column \code{percent}
#' @param colors Named vector containing party colours. If \code{NULL}
#' (deftault) tries to guess color based on party names, grey otherwise.
#' @param labels Named vector containing party labels. If \code{NULL} (default)
#' tries to guess party names from \code{data}.
#' @param annotate_bars If \code{TRUE} (default) bars are annotated by the
#' respective vote share (percentage).
#' @param hurdle Hurdle for single parties to get into the parliament, e.g. '5'
#' for '5\%'. If set to NULL no horizontal line is plotted.
#' The horizontal line can be suppressed using \code{NULL}.
#' @import ggplot2
#' @importFrom checkmate assert_data_frame assert_character assert_number assert_flag assert_subset
#' @export
#' @examples
#' library(tidyr)
#' library(dplyr)
#' library(coalitions)
#'
#' survey <- scrape_wahlrecht() %>% collapse_parties() %>%
#'   slice(1) %>% select(survey) %>% unnest()
#'
#' gg_survey(survey)
gg_survey <- function(
  data,
  colors     = NULL,
  labels     = NULL,
  annotate_bars = TRUE,
  hurdle     = 5) {

  assert_data_frame(data, any.missing = FALSE, min.rows = 1L, min.cols = 3L)
  assert_subset(c("party", "percent", "votes"), colnames(data))
  assert_character(colors, null.ok = TRUE, any.missing = FALSE, len = nrow(data))
  assert_character(labels, null.ok = TRUE, any.missing = FALSE, len = nrow(data))
  assert_number(hurdle, lower = 0, upper = 100)
  assert_flag(annotate_bars)

  colors <- prep_colors(data, colors)
  if (!is.factor(data$party)) {
    data$party <- factor(data$party, levels = data$party)
  }
  if (is.null(labels)) {
    labels <- prettify_en(data$party)
  }
  if (annotate_bars) {
    data$label_position <- pmax(hurdle, data$percent)
  }

  p_survey <- ggplot(
      data    = data,
      mapping = aes_string(x = "party", y = "percent", fill = "party")) +
    geom_bar(stat = "identity") +
    scale_fill_manual(name = "party", values = colors) +
    scale_x_discrete(labels = labels) +
    theme(legend.position = "none")

  if (!is.null(hurdle)) {
    p_survey <- p_survey + geom_hline(aes(yintercept = hurdle), col = "#616161")
  }

  if (annotate_bars) {
    p_survey <- gg_add_annotate_bars(p_survey, data)
  }

  p_survey

}


prep_colors <- function(data, colors) {

  if (is.null(colors)) {

    colors <- rep("gray80", nrow(data))
    ind_available <- match(data$party, names(party_colors_de))
    colors[ind_available] <- party_colors_de[ind_available]

  }

  colors

}

gg_add_annotate_bars <- function(p_survey,  data) {

  data$percent_label <- as.character(round(data$percent, 1))
  # positive labels
  p_survey <- p_survey +
    geom_text(
      data = data[data$percent >= 0, ],
      mapping = aes_string(
        y     = "label_position",
        label = "percent_label"),
      vjust = -0.5,
      hjust = 0.5,
      col   = "darkgray")
  # negative labels (e.g. if differences and not shares are plotted)
  p_survey +
    geom_text(
      data = data[data$percent < 0, ],
      mapping = aes_string(
        y     = "label_position",
        label = "percent_label"),
      vjust = 1.5,
      hjust = 0.5,
      col = "darkgray")

}
