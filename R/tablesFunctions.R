## creates list with probs for each institute (and different percentages of lent 
# votes)
as_surveyByInstitute <- function(results, seed, simulations = 1e5, max.lent = 10,
        coalitions = NULL, superior.coalitions = NULL, ...) {
    
    results$Anteil <- results$Anteil/100
    date.newest <- aggregate(Datum ~ Institut, max, data = results)
    sub <- subset(results, Datum %in% date.newest$Datum)
    sub.list <- split(sub, sub$Institut)
    befragte <- sapply(sub.list, function(z) unique(z$Befragte))
    tab.list <- lapply(sub.list, function(z) 
                as_survey(z$Anteil, unique(z$Befragte), z$Partei))
    
    lv.list <- lapply(seq_along(tab.list), function(z) 
                lentVoteMat(tab.list[[z]], unique(sub.list[[z]]$Befragte), 
                        simulationen = simulations, seed = seed, 
                        max.percent.lent = max.lent, coalitions = coalitions, 
                        superior.coalitions = superior.coalitions, ...))
    
    names(lv.list) <- apply(cbind.data.frame(date.newest$Institut, 
                    as.Date(date.newest$Datum), 
                    befragte), 1, paste0, collapse = "_" )
    
    ## return
    lv.list
    
}

#as_surveyByInstitute(results, 123, 100, 10)

as_surveyByDate <- function(results, seed, simulations = 1e5, 
        coalitions = NULL, superior.coalitions = NULL, 
        ...) {
    
    results$Anteil <- results$Anteil/100
    survey.list <- split(results, f = results$Datum)
    befragte <- sapply(survey.list, function(z) unique(z[, "Befragte"]))
    tab.list <- lapply(survey.list, function(z) 
                as_survey(z$Anteil, unique(z$Befragte), z$Partei))
    
    lv.list <- lapply(seq_along(tab.list), function(z) 
                lentVoteMat(tab.list[[z]], befragte[z], 
                        simulationen = simulations, seed = seed, 
                        max.percent.lent = 0, coalitions = coalitions, 
                        superior.coalitions = superior.coalitions, ...))
    names(lv.list) <- names(tab.list)
    
    ## return
    mlv <- melt(lv.list, id.vars = "Koalition", value.name = "Wkt") 
    colnames(mlv)[grep("L1", colnames(mlv))] <- "Datum"
    mlv
    
    
}

as_surveyByDateAndInstitute <- function(results, seed, simulations = 1e5, 
        coalitions = NULL, superior.coalitions = NULL, ...){
    
    inst.list <- split(results, f = results$Institut)
    names(inst.list) <- levels(results$Institut)
    tab.list <- lapply(inst.list, as_surveyByDate, seed = seed, 
            simulations = simulations, coalitions = coalitions, 
            superior.coalitions = superior.coalitions, ...)
    m.list <- melt(tab.list, id.vars = c("Datum", "Koalition", "variable"), 
            measure.vars = "Wkt", value.name = "Wkt")
    m.list 
    
}


printLentVoteMat <- function(lent.vote.mat, coalition.order = NULL, 
        coalition.addon = NULL) {
    
    n.elements <- nlevels(lent.vote.mat$Koalition)
    if( !is.null(coalition.order) )
        lent.vote.mat$Koalition <- factor(lent.vote.mat$Koalition, 
                levels = coalition.order)
    if( !is.null(coalition.addon) )
        lev.names.row <- c(paste(levels(lent.vote.mat$Koalition)[-n.elements], 
                        coalition.addon[-n.elements]), 
                paste(levels(lent.vote.mat$Koalition)[n.elements], "\\\\", 
                        coalition.addon[n.elements]))
    else lev.names.row <- levels(lent.vote.mat$Koalition)
    
    tabular(RowFactor(Koalition, levelnames = lev.names.row, spacing = 0) ~ 
                    Factor(lent, name = "Wkt. Mehrheit (\\%)",
                            levelnames = c("Umfrage", "5\\% Leihstimmen", 
                                    "10\\% Leihstimmen"))*
                    Heading()*value*Heading()*anteilFun, 
            data = subset(lent.vote.mat, lent %in% c("0", "0.05", "0.1")))
    
}

castCoalProb <- function(coal.probs, coalition.order = NULL, 
        coalition.addon = NULL) {
    
    ## create table
    c.cp <- dcast(coal.probs, Koalition ~ lent, mean)
    
    ## cosmetics
    c.cp[, -1] <- c.cp[, -1] * 100
    rownames(c.cp) <- c.cp[, "Koalition"]
    if( !is.null(coalition.order) ) 
        c.cp <- c.cp[order(match(rownames(c.cp), coalition.order)), ]
    if( !is.null(coalition.addon) ) 
        rownames(c.cp) <- paste(rownames(c.cp), coalition.addon)
    c.cp$Koalition <- NULL
    
    # return
    c.cp
    
}
