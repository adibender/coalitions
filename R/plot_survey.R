
#' Plot voter shares observed in one survey
#' 
#' Bar chart of the raw voter shares observed in one survey.
#' Additionally to plotting positive voter shares,
#' the function can be used to plot party-specific differences (e.g. between
#' a survey and the election result), including negative numbers.
#' 
#' @param data scraped dataset containing one row per party in the column \code{party}
#' and the observed voter share in the column \code{percent}
#' @param cols named vector containing party colours
#' @param labels named vector containing party labels
#' @param focus optional name of one party ("cdu","spd",etc.) that should be highlighted
#' @param base_size base size of all texts in plot, see \code{\link[ggplot2]{ggtheme}}
#' @param minimal if TRUE the plot axes and some other graphic elements are suppressed.
#' Defaults to FALSE.
#' @param ylim optional limits vector for the y axis
#' @param ylab optional label for the y axis
#' @param main optional plot title
#' @param perc_labels if TRUE (default) percentage labels are drawn above the bars
#' @param hurdle hurdle for single parties to get into the parliament, e.g. '5' for '5\%'.
#' The horizontal line can be suppressed using \code{NA}.
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
#' cols <- c("cdu" = "black", "spd" = "#E3000F", "greens" = "#46962b", "fdp" = "gold3",
#'           "left" = "deeppink3", "afd" = "skyblue2", "others" = "grey")
#' labels <- c("cdu" = "Union", "spd" = "SPD", "greens" = "Greens", "fdp" = "FDP",
#'             "left" = "Left", "afd" = "AfD", "others" = "Others")
#' plot_survey(dat, cols = cols, labels = labels)
#' plot_survey(dat, cols = cols, labels = labels, minimal = TRUE)
plot_survey <- function(data, cols = NULL, labels = NULL, focus = NULL, base_size = 15,
                        minimal = FALSE, ylim, ylab = "Voter share in %", main = NULL, perc_labels = TRUE,
                        hurdle = 5) {
  if (!is.factor(data$party))
    data$party <- factor(data$party, levels = data$party)
  if (missing(ylim)) {
    if (perc_labels) # if we need extra space for the percentage labels
      ylim <- c(0,max(data$percent) + 4)
    else
      ylim <- c(0,max(data$percent))
  }
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
  if (!minimal & perc_labels) {
    data$label_position <- apply(data, 1, function(x) max(hurdle, as.numeric(x["percent"]), na.rm = T))
  }
  
  gg <- ggplot(data, aes_string(x = "party", y = "percent", fill = "party"))
  if (minimal)
    gg <- gg + geom_hline(yintercept = seq(0,max(ylim),by = 10), col = grDevices::gray(0.9), lty = 2)
  gg <- gg + geom_bar(stat = "identity") +
    scale_fill_manual(name="Partei", values=cols) +
    scale_x_discrete(labels = labels)
  if (!is.na(hurdle))
    gg <- gg + geom_hline(aes(yintercept = hurdle), col = "#616161")
  gg <- gg + ylab(ylab) + ylim(ylim) + ggtitle(main) +
    theme_bw(base_size = base_size) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 0.95 * base_size))
  if (!minimal & perc_labels) {
    data$percent_label <- as.character(round(data$percent, 1))
    # positive labels
    gg <- gg + geom_text(data = data[data$percent >= 0,], aes_string(y = "label_position", label = "percent_label"), vjust = -0.5, hjust = 0.5, size = base_size / 4, col = "darkgray")
    # negative labels (e.g. if differences and not shares are plotted)
    gg <- gg + geom_text(data = data[data$percent < 0,], aes_string(y = "label_position", label = "percent_label"), vjust = 1.5, hjust = 0.5, size = base_size / 4, col = "darkgray")
  }
  if(minimal) {
    gg <- gg +
      theme(axis.title = element_blank(),
            axis.text.x = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank())
  }
  gg
}
