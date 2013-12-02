#' Select the \emph{n} most recent surveys

#' @param surveys \code{data.frame} containing surveys. See \code{\link{getSurveys}}
#' @param n \code{numeric} Number of surveys to select. Defaults to \code{3}.

#' @return \code{data.frame} containing \code{n} most recent surveys. 
#' @keywords survey, select
#' @seealso \code{\link{getSurveys}}

selectSurveys <- function(surveys, n = 3) {
    
    ## check if surveys have right format 
    ##TODO: define class for surveys (returned by @getSurveys)
    unique.dates <- unique(surveys[, c("institute", "date")])
    recent.dates <- sort(unique.dates[["date"]], 
        partial = n)[nrow(unique.dates):(nrow(unique.dates) -  n + 1)] 
    
    most.recent <- surveys[surveys[["date"]] %in% recent.dates, ]
    if( length(u.i <- unique(most.recent[["institute"]]))  > 1 ) {
        most.recent <- most.recent[most.recent[["institute"]] == u.i[length(u.i)], ]
    }
    
    return(most.recent)
    
}
