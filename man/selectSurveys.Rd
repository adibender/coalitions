\name{selectSurveys}
\alias{selectSurveys}
\title{Select the \emph{n} most recent surveys}
\usage{
  selectSurveys(surveys, n = 3)
}
\arguments{
  \item{surveys}{\code{data.frame} containing surveys. See
  \code{\link{getSurveys}}}

  \item{n}{\code{numeric} Number of surveys to select.
  Defaults to \code{3}.}
}
\value{
  \code{data.frame} containing \code{n} most recent
  surveys.
}
\description{
  Select the \emph{n} most recent surveys
}
\seealso{
  \code{\link{getSurveys}}
}
\keyword{select}
\keyword{survey,}

