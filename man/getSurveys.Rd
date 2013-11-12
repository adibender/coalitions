\name{getSurveys}
\alias{getSurveys}
\title{Read surveys from csv file}
\usage{
  getSurveys(path.to.surveys, date.var = "date",
    institute.var = "institute", parties.var = "party",
    date.format = "\%d.\%m.\%Y", party.order = NULL, ...)
}
\arguments{
  \item{path.to.surveys}{character. absolute or relative
  path to a file containing survey results, that can be
  read by \code{\link{read.table}}}

  \item{date.var}{character. name of the column containing
  date on which survey was conducted defaults to
  \code{"date"}}

  \item{institute.var}{character. name of the column
  containing the name of the institute that contained the
  survey}

  \item{parties.var}{character. name of the column
  containing the name of the parties for which voter
  preferences have been obtained}

  \item{date.format}{format of the \code{date.var}
  variable. Defaults to \code{%d.%m.%Y}}

  \item{party.order}{optional reordering of parties and
  factor levels. Defaults to \code{NULL}, no factor level
  reordering.}

  \item{...}{additional arguments passed to
  \code{link{read.table}}}
}
\value{
  data.frame with properly formated surveys (optionaly with
  factor levels reordered)
}
\description{
  Read surveys from csv file
}
\seealso{
  \code{\link{selectSurveys}}
}
\keyword{survey}

