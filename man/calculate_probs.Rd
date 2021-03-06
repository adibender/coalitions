% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coalition-probability.R
\name{calculate_probs}
\alias{calculate_probs}
\title{Calculate coalition probabilities for multiple coalitions}
\usage{
calculate_probs(majority_df, coalitions, exclude_superior = TRUE, ...)
}
\arguments{
\item{majority_df}{A data frame containing logical values indicating
if the coalitions (columns) have a majority (rows).}

\item{coalitions}{A list of coalitions for which coalition probabilities should
be calculated. Each list entry must be a vector of party names. Those names
need to correspond to the names in \code{majority_df}.}

\item{exclude_superior}{Logical. If \code{TRUE}, superior coalitions will
be excluded, otherwise total coalition probabilities will be returned.
Usually it makes sense to exclude superior coalitions.}

\item{...}{Further arguments passed to \code{\link[coalitions]{get_superior}}}
}
\description{
Given a table with simulations in the rows and coalitions in the columns,
this function returns the coalition probabilities for a specified coalition,
by default excluding superior coalitions first
}
\examples{
test_df <- data.frame(
 cdu            = c(rep(FALSE, 9), TRUE),
 cdu_fdp        = c(rep(FALSE, 8), TRUE, TRUE),
 cdu_fdp_greens = c(TRUE, TRUE, rep(FALSE, 6), TRUE, TRUE))
calculate_probs(test_df, list("cdu", "cdu_fdp", "cdu_fdp_greens"))
calculate_probs(test_df, list("cdu", "cdu_fdp", "cdu_fdp_greens"), exclude_superior=FALSE)
}
\seealso{
\code{\link[coalitions]{calculate_prob}}
}
