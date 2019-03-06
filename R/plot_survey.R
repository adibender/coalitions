
#' Plot voter shares observed in one survey
#' 
#' Bar chart of the raw voter shares observed in one survey.
#' Additionally to plotting positive voter shares,
#' the function can be used to plot party-specific differences (e.g. between
#' a survey and the election result), including negative numbers.
#' 
#' @param data scraped dataset containing one row per party in the column 
#' \code{party} and the observed voter share in the column \code{percent}
#' @param cols named vector containing party colours. If set to NULL all
#' parties get the same colour.
#' @param labels named vector containing party labels. If set to NULL the
#' party names from \code{data} are used.
#' @param focus optional name of one party ("cdu","spd", etc.) that should be 
#' highlighted
#' @param perc_labels if TRUE (default) percentage labels are drawn above the
#' horziontal line for \code{hurdle}
#' @param hurdle hurdle for single parties to get into the parliament, e.g. '5'
#' for '5\%'. If set to NULL no horizontal line is plotted.
#' The horizontal line can be suppressed using \code{NULL}.
#' @import ggplot2
#' @export
#' @examples
#' library(tidyr)
#' library(purrr)
#' library(dplyr)
#' library(coalitions)
#' 
#' dat <- scrape_wahlrecht() %>% collapse_parties() %>%
#'   slice(1) %>% select(survey) %>% unnest()
#'
#' gg_survey(dat)
gg_survey <- function(
  data,
  cols        = party_colors_de,
  labels      = party_labels_de,
  focus       = NULL,
  perc_labels = TRUE,
  hurdle      = 5) {
  
  assert_data_frame(data)
  assert_character(cols, null.ok = TRUE)
  assert_character(labels, null.ok = TRUE)
  assert_number(hurdle, lower = 0, upper = 100, null.ok = TRUE)
  
  if (!is.factor(data$party))
    data$party <- factor(data$party, levels = data$party)
  if (is.null(cols)) {
    cols <- rep("gray80", nrow(data))
    names(cols) <- data$party
  }
  if (is.null(labels))
    labels <- data$party
  if (!is.null(focus)) {
    if (!(focus %in% names(cols)))
      stop("The value of 'focus' has to be a name of the 'cols' vector!")
    cols[names(cols) != focus] <- "grey90"
  }
  if (perc_labels) {
    data$label_position <- apply(data, 1, function(x) {
      max(hurdle, as.numeric(x["percent"]), na.rm = T)
    })
  }
  
  gg <- ggplot(data, aes_string(x = "party", y = "percent", fill = "party")) +
    geom_bar(stat = "identity") +
    scale_fill_manual(name="party", values=cols) +
    scale_x_discrete(labels = labels)
  if (!is.null(hurdle))
    gg <- gg + geom_hline(aes(yintercept = hurdle), col = "#616161")
  if (perc_labels) {
    data$percent_label <- as.character(round(data$percent, 1))
    # positive labels
    gg <- gg + 
      geom_text(data = data[data$percent >= 0,], 
                aes_string(y = "label_position", label = "percent_label"),
                vjust = -0.5, hjust = 0.5, col = "darkgray")
    # negative labels (e.g. if differences and not shares are plotted)
    gg <- gg + 
      geom_text(data = data[data$percent < 0,], 
                aes_string(y = "label_position", label = "percent_label"),
                vjust = 1.5, hjust = 0.5, col = "darkgray")
  }
  
  gg
  
}
