#' Read surveys from csv file

#' @param path.to.surveys character. absolute or relative path to a file containing 
#' survey results, that can be read by \code{\link{read.table}}
#' @param date.var character. name of the column containing date on which survey was conducted
#' defaults to \code{"date"}
#' @param institute.var character. name of the column containing the name of the 
#' institute that contained the survey 
#' @param parties.var character. name of the column containing the name of the 
#' parties for which voter preferences have been obtained
#' @param date.format format of the \code{date.var} variable.
#' @param party.order optional reordering of parties and factor levels. Defaults
#' to \code{NULL}, no factor level reordering.
#' @param ... additional arguments passed to \code{link{read.table}}

#' @return data.frame with properly formated surveys (optionaly with 
#' factor levels reordered)

#' @keywords survey
#' @seealso \code{\link{selectSurveys}}
#' @export

getSurveys <- function(path.to.surveys, date.var = "date", 
    institute.var = "institute", parties.var = "party", 
    date.format = "%d.%m.%Y", party.order = NULL, ...) {
    
    surveys <- read.csv(path.to.surveys, ...)
    surveys[[date.var]] <- as.Date(surveys[[date.var]], format = date.format)
    surveys[[institute.var]] <- gsub(" ", "", surveys[[institute.var]])
    surveys <- colwise(na.locf)(surveys) ##TODO: this must be omitted
    surveys[[institute.var]] <- factor(surveys[[institute.var]])
    
    ## reorder parties
    if( is.null(party.order) ) party.order <- unique(surveys[[party.var]])
    
    surveys[[party.var]] <- factor(surveys[[party.var]], levels = party.order)
    
    return(surveys)
}

poolSurveys <- function(surveys, n.surveys = 3) {
    
    recent.surveys <- selectSurveys(surveys, n.surveys = n.surveys)
    recent.surveys$Anteil <- recent.surveys$Anteil/100 * recent.surveys$Befragte
    pooled.surveys <- aggregate(Anteil ~ Partei, sum, data = recent.surveys)
    n <- sum(aggregate(Befragte ~ Institut, unique, data = recent.surveys)$Befragte)
    
    pooled.surveys$Anteil <- pooled.surveys$Anteil/n
    pooled.surveys
    as_survey(votes.in.perc = pooled.surveys$Anteil, samplesize = n, 
            parties = pooled.surveys$Partei)
    
}

poolSurveys2 <- function(recent.surveys) {
    
    recent.surveys$Anteil <- recent.surveys$Anteil/100 * recent.surveys$Befragte
    pooled.surveys <- aggregate(Anteil ~ Partei, sum, data = recent.surveys)
    n <- sum(aggregate(Befragte ~ Institut, unique, data = recent.surveys)$Befragte)
    
    pooled.surveys$Anteil <- pooled.surveys$Anteil/n * 100
    pooled.surveys$Institut <- paste(unique(recent.surveys$Institut), collapse = ", ")
    pooled.surveys$Befragte <- n
    pooled.surveys$Datum <- max(recent.surveys$Datum)
    pooled.surveys[, c("Institut", "Datum", "Befragte", "Partei", "Anteil")]
    
}

surveysToPooledSurveys <- function(surveys, 
        institutes = c("Emnid", "FGW", "Forsa", "Infratest")) {
     
    surveys.list <- selectSurveys2(surveys, institutes = institutes)    
    ps.list <- lapply(surveys.list, poolSurveys2)
    
    ps <- do.call(rbind, ps.list)
    
    ps
    
}



#### function that calculate results for all institute/display them to html
latestSurveys <- function(surveys) {
    
    date.newest <- aggregate(Datum ~ Institut, max, data = surveys)
    ia.newest <- with(date.newest, interaction(Datum, Institut))
    ia.results <- with(surveys, interaction(Datum, Institut))
    newest.results <- surveys[ia.results %in% ia.newest, ]
    dcast(newest.results, Partei ~ Institut+Datum+Befragte, mean, 
            subset = .(Datum %in% date.newest$Datum), value.var = "Anteil")
    
}

updateUsedSurveys <- function(
        party.order = NULL,
        surveys.file = "../data/sonntagsfrageProjektion.csv", 
        latest.file = "../data/lastUpdates.Rds") {
    
    surveys <- getSurveys(surveys.file, party.order = party.order)
    latest <- aggregate(Datum ~ Institut, max, data = surveys)
    saveRDS(latest, latest.file)
    
    Sys.chmod(c(latest.file), 
            mode = "0774", use_umask = FALSE)
    
}



############################## new with 4 institutes 

lastSurveys <- function(surveys) {
    
    date.newest <- aggregate(Datum ~ Institut, max, data = surveys)
    ia.newest <- with(date.newest, interaction(Datum, Institut))
    ia.results <- with(surveys, interaction(Datum, Institut))
    newest.results <- surveys[ia.results %in% ia.newest, ]
    
    newest.results
    
}

selectSurveys2 <- function(surveys, 
        institutes = c("Emnid", "FGW", "Forsa", "Infratest")) {
    
    survs <- droplevels(subset(surveys, Institut %in% institutes))
    viable <- TRUE
    surveys.list <- list()
    
    while( viable ){
        
        last.surveys <- lastSurveys(survs)
        if( !(all(unique(last.surveys$Institut) %in% institutes))) 
            stop("Wrong institutes")
        else surveys.list <- c(surveys.list, list(last.surveys))
        
        survs <- survs[survs$Datum != max(survs$Datum), ]
        
        if( !(all(institutes %in% unique(survs$Institut))) ) {
            viable <- FALSE
        }
        
    } 
    
    surveys.list
}



writeProbsToCSV <- function(
        coalition.results.file, 
        coalition.results.file.ur,
        entry.results.file, 
        pooled.coalition.file, 
        pooled.coalition.file.ur,
        pooled.entry.file, 
        pooled.surveys.file,
        results.path = "../results/", 
        ...)  {
    
    write.table(readRDS(coalition.results.file), 
            paste0(results.path, 
                    tools::file_path_sans_ext(basename(coalition.results.file)), 
                    ".csv"), ...) 
    write.table(readRDS(coalition.results.file.ur), 
            paste0(results.path, 
                    tools::file_path_sans_ext(basename(coalition.results.file.ur)), 
                    ".csv"), ...)
    write.table(readRDS(entry.results.file), 
            paste0(results.path, 
                    tools::file_path_sans_ext(basename(entry.results.file)), 
                    ".csv"), ...)
    write.table(readRDS(pooled.coalition.file), 
            paste0(results.path, 
                    tools::file_path_sans_ext(basename(pooled.coalition.file)), 
                    ".csv"), ...)
    write.table(readRDS(pooled.coalition.file.ur), 
            paste0(results.path, 
                    tools::file_path_sans_ext(basename(pooled.coalition.file.ur)), 
                    ".csv"), ...)
    write.table(readRDS(pooled.entry.file), 
            paste0(results.path, 
                    tools::file_path_sans_ext(basename(pooled.entry.file)), 
                    ".csv"), ...)
    write.table(readRDS(pooled.surveys.file), 
            paste0(results.path, 
                    tools::file_path_sans_ext(basename(pooled.surveys.file)), 
                    ".csv"), ...)
}
