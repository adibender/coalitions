#### functions to obtain pooled results

getSurveys <- function(path.to.surveys, party.order = NULL, ...) {
    
    require(plyr)
    require(zoo)
    
    surveys <- read.csv2(path.to.surveys, na.strings = "")
    surveys$Datum <- as.Date(surveys$Datum, format = "%d.%m.%Y")
    surveys$Institut <- gsub(" ", "", surveys$Institut)
    surveys <- colwise(na.locf)(surveys)
    surveys$Institut <- factor(surveys$Institut)
    
    ## reorder parties
    if( is.null(party.order) ) party.order <- unique(surveys$Partei)
    
    surveys$Partei <- factor(surveys$Partei, levels = party.order )
    
    surveys
}

selectSurveys <- function(surveys, n.surveys = 3) {
    
    ## unique dates per Institute, 
    unique.dates <- unique(surveys[, c("Institut", "Datum")])
    recent.dates <- sort(unique.dates$Datum, 
            partial = n.surveys)[nrow(unique.dates):(nrow(unique.dates) - 
                        n.surveys + 1)] 
    
    most.recent <- surveys[surveys$Datum %in% recent.dates, ]
    if( length(u.i <- unique(most.rechent$Institut))  > 1 ) {
        most.recent <- most.recent[most.recent$Institut == u.i[length(u.i)], ]
    }
    
    most.recent
    
}


poolSurveys <- function(surveys, n.surveys = 3) {
    
    recent.surveys <- selectSurveys(surveys, n.surveys = n.surveys)
    recent.surveys$Anteil <- recent.surveys$Anteil/100 * recent.surveys$Befragte
    pooled.surveys <- aggregate(Anteil ~ Partei, sum, data = recent.surveys)
    n <- sum(aggregate(Befragte ~ Institut, unique, data = recent.surveys)$Befragte)
    
    pooled.surveys$Anteil <- pooled.surveys$Anteil/n
    pooled.surveys
    createTab(votes.in.perc = pooled.surveys$Anteil, sample.size = n, 
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
