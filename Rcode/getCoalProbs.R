# getCoalitionProbabilities.R
# 11.06.2013
# Author: bender.at.R@gmail.com
###############################################################################


## calculates number of seats for the respective parties that have received more 
## than 5% of votes (according to the method of Sainte-Lague/Schepers)
## see http://www.wahlrecht.de/verfahren/rangmasszahlen.html

sls <- function(survey, seats = 598, hurdle = 0.05, epsilon = 10e-6) {
    
    require(reshape2)
    
    #get votes.in.perc after excluding parties with votes.in.perc < 0.05 and "Sonstige"
    survey <- get.props(survey, hurdle = hurdle)
    
    # check for data validity
    if( abs(sum(survey$votes.in.perc) - 1) > epsilon  ) 
        stop("wrong percentages provided in sls() function")
    
    divisor.mat <- sum(survey$votes)/sapply(survey$votes, "/",  
            seq(0.5, 598.5, by = 1))
    colnames(divisor.mat) <- survey$party
    
    m.mat <- melt(divisor.mat, id.vars = "party")
    m.mat <- m.mat[rank(m.mat$value, ties.method = "random") <= seats, ]
    rle.seats <- rle(as.character(m.mat$Var2))
    seat.mat <- cbind.data.frame(party = rle.seats$values, seats = rle.seats$lengths)
    
    if( nrow(seat.mat) != nrow(survey) ) 
        stop ("Wrong number of parties after seat distribution")
    if( sum(seat.mat$seats) != seats ) 
        stop(paste("Number of seats distributed not equal to", seats))
    
    survey <- merge(survey, seat.mat, by = "party")
    
    survey
    
}


dHondt <- function(survey, hurdle = 0.04, seats = 183, epsilon = 1e-6) {
    
    require(reshape2)
    
    #get votes.in.perc after excluding parties with votes.in.perc < 0.05 and "Sonstige"
    survey <- get.props(survey, hurdle = hurdle)
    
    # check for data validity
    if( abs(sum(survey$votes.in.perc) - 1) > epsilon  ) 
        stop("wrong percentages provided in dHondt() function")
    
    divisor.mat <- sum(survey$votes)/sapply(survey$votes, "/",  seq(1, 183, by = 1))
    colnames(divisor.mat) <- survey$party
    m.mat <- melt(divisor.mat, id.vars = "party")
    m.mat <- m.mat[rank(m.mat$value, ties.method = "random") <= seats, ]
    rle.seats <- rle(as.character(m.mat$Var2))
    seat.mat <- cbind.data.frame(party = rle.seats$values, seats = rle.seats$lengths)
    
    if( nrow(seat.mat) != nrow(survey) ) 
        stop ("Wrong number of parties after seat distribution")
    if( sum(seat.mat$seats) != seats ) 
        stop(paste("Number of seats distributed not equal to", seats))
    
    survey <- merge(survey, seat.mat, by = "party")
    
    survey
    
}


## calculate percentage of votes/seats after excluding parties with votes.in.perc < 0.05
get.props <- function(survey, hurdle = 0.05) {
    
    ## survey: table containing votes and votes in percent as e.g. returned by createTab()
    
    ## exclude 'Sonstige' and parties with less than 5%
    survey <- survey[survey$votes.in.perc >= hurdle & survey$party != "Sonstige", ]
    survey$votes.in.perc <- survey$votes/sum(survey$votes)
    
    survey
    
}


drawDirichletElections <- function(survey, simulations, seed = NULL, prior = NULL) {
    
    ## for rdirichlet
    require(MCMCpack)
    # simulieren von anteilen
    # rdirichlet ruft rgamma auf mit rate = 1    
    
    ## calculate posteriori
    if( is.null(prior) ) {
        prior <- rep(0.5, nrow(survey))    
    }
    
    else {
        if( length(prior) != nrow(survey) ) 
            stop("length of prior weights and number of observations differ")
    }
    
    alpha <- survey$votes + prior
    
    ## draw n.sim random dirichlet numbers/vectors with concentration weights alpha
    if( !is.null(seed) ) set.seed(seed)
    rn <- as.data.frame(rdirichlet(simulations, alpha = alpha))
    colnames(rn) <- survey$party  
    
    rn
    
}


getProbabilitiesEntry <- function(dirichlet.draws) {
    
    ## dirichlet.draws: matrix off random election results as returned by 
    # drawDirichletElections
    colSums(dirichlet.draws >= 0.05)/nrow(dirichlet.draws)
    
}


## apply sls() to each random dirichlet elections
getSeatDistribution <- function(dirichlet.draws, survey, distrib.fun = sls, 
        sample.size = NULL, ...) {
    
    if( is.null(sample.size) ) sample.size <- sum(survey$votes)
    sim.surveys <- lapply(seq_len(nrow(dirichlet.draws)), function(z) {
                
                survey$votes.in.perc <- as.numeric(dirichlet.draws[z, ])
                survey$votes <- survey$votes.in.perc * sample.size
                survey
                
            })
    
    ## calculate seat distribution for each simulation via sls() function 
    sim.results <- lapply(sim.surveys, distrib.fun, ...)
    
    ## return results
    sim.results
    
}

## lent.votes posteriori, that is after drawing random numbers with original 
## votes, i.e. for each vector of random numbers, redistribute
## votes from CDU/CSU to FDP 
lentVotesRn <- function(rn.mat, lent, from = "CDU/CSU", to = "FDP") {
    
    if( lent != 0) {
        lent.pp <- rn.mat[, from] * lent
        rn.mat[, from] <- rn.mat[, from] -lent.pp
        rn.mat[, to] <- rn.mat[, to] + lent.pp
    }
    rn.mat
    
}

getCoalitionProbSup <- function(seat.tab, coalition, 
        superior, majority = 300) {
    
    ind.coalition <- sapply(seat.tab, function(z) {
                sum(z$seats[z$party %in% coalition]) >= majority
            })
    
    if( !any(is.na(superior)) ) {
        
        ind.sup.list <- lapply(superior, function(superior.coalition) {
                    sapply(seat.tab, function(z) {
                                sum(z$seats[z$party %in% superior.coalition]) >= 
                                        majority
                            })
                })
    }
    else{
        ind.sup.list <- list(rep(FALSE, length(ind.coalition)))
    }
    
    ind.sup <- Reduce("|", ind.sup.list)
    
    mean(ind.coalition & !(ind.sup))
    
}

getCoalitionProbs <- function(seat.tabs, coalitions, superior.coalitions, 
        majority = 300) {
    
    coal.probs <- sapply(seq_along(coalitions), function(z) {
                getCoalitionProbSup(seat.tabs, coalitions[[z]], 
                        superior.coalitions[[z]], majority = majority)
            })  
    names(coal.probs) <- sapply(coalitions, paste0, collapse = "-")
    
    coal.probs
    
}


getLentVoteCoalitionProbs <- function(dirichlet.draws, survey,  
        coalitions, majority = 300, max.percent.lent = 10, 
        distrib.fun = sls,
        superior.coalitions = NULL) {
    
    if( is.null(superior.coalitions)  ) 
        superior.coalitions <- as.list(rep(NA, length(coalitions)))
    
    
    survey.tab <- createTab(survey$Anteil/100, unique(survey$Befragte), 
            survey$Partei)
    
    votes.lent <- 0:max.percent.lent / 100# vektor zu betrachtender anteile 
    # geliehener Stimmen
    
    ## berechne für jeden zu betrachtenden anteil geliehener stimmen die sitzeverteilung
    rn.lent.list <- lapply(votes.lent, lentVotesRn, rn.mat = dirichlet.draws)
    
    seat.lent.list <- lapply(rn.lent.list, getSeatDistribution, survey = survey.tab, 
            distrib.fun = distrib.fun)
    
    ## berechne koalitionswahrscheinlichkeiten etc.
    probs.list <- lapply(seat.lent.list, getCoalitionProbs, 
            coalitions = coalitions, 
            superior.coalitions = superior.coalitions, 
            majority = majority)
    names(probs.list) <- votes.lent
    probs.list <- lapply(probs.list, function(z) cbind.data.frame(Koalition = names(z), 
                        Anteil = z))
    
    m.cp <- melt(probs.list, id.vars = "Koalition")
    colnames(m.cp)[grep("L1", colnames(m.cp))] <- "lent"
    m.cp$Institut <- unique(survey$Institut)
    m.cp$Datum <- unique(survey$Datum)
    m.cp$Befragte <- unique(survey$Befragte)
    
    m.cp
    
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




getLentEntryProbs <- function(dirichlet.draws, max.percent.lent = 10) {
    
    lent.vec <- 0:max.percent.lent / 100
    lent.drn <- lapply(lent.vec, lentVotesRn, rn.mat = dirichlet.draws)
    
    entry.probs <- sapply(lent.drn, getProbabilitiesEntry)
    colnames(entry.probs) <- lent.vec
    
    entry.probs
    
}


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


############## plot 
## plot function for course of coalition probabilities for various percentages 
# of lent votes
ggLent <- function(lent.vote.mat, coalition.order = NULL, coalition.addon = NULL, 
        coalition.colors) {
    
    if( !is.null(coalition.order) )
        lent.vote.mat$Koalition <- factor(lent.vote.mat$Koalition, 
                levels = coalition.order)
    
    lent.vote.mat$x <- as.numeric(lent.vote.mat$lent)*100
    
    ggplot(lent.vote.mat, aes(x = x, y = value) ) + 
            geom_line(aes(group = Koalition, color = Koalition)) + 
            ylab("Koalitionswahrscheinlichkeit (%)") + 
            xlab("Leihstimmenanteil (%)") + 
            scale_color_manual(values = coalition.colors, 
                    labels = paste(levels(lent.vote.mat$Koalition),
                            coalition.addon))
    
    
}


######## hp helpers

## creates list with probs for each institute (and different percentages of lent 
# votes)
createTabByInstitute <- function(results, seed, simulations = 1e5, max.lent = 10,
        coalitions = NULL, superior.coalitions = NULL, ...) {
    
    results$Anteil <- results$Anteil/100
    date.newest <- aggregate(Datum ~ Institut, max, data = results)
    sub <- subset(results, Datum %in% date.newest$Datum)
    sub.list <- split(sub, sub$Institut)
    befragte <- sapply(sub.list, function(z) unique(z$Befragte))
    tab.list <- lapply(sub.list, function(z) 
                createTab(z$Anteil, unique(z$Befragte), z$Partei))
    
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

#createTabByInstitute(results, 123, 100, 10)

createTabByDate <- function(results, seed, simulations = 1e5, 
        coalitions = NULL, superior.coalitions = NULL, 
        ...) {
    
    results$Anteil <- results$Anteil/100
    survey.list <- split(results, f = results$Datum)
    befragte <- sapply(survey.list, function(z) unique(z[, "Befragte"]))
    tab.list <- lapply(survey.list, function(z) 
                createTab(z$Anteil, unique(z$Befragte), z$Partei))
    
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

createTabByDateAndInstitute <- function(results, seed, simulations = 1e5, 
        coalitions = NULL, superior.coalitions = NULL, ...){
    
    inst.list <- split(results, f = results$Institut)
    names(inst.list) <- levels(results$Institut)
    tab.list <- lapply(inst.list, createTabByDate, seed = seed, 
            simulations = simulations, coalitions = coalitions, 
            superior.coalitions = superior.coalitions, ...)
    m.list <- melt(tab.list, id.vars = c("Datum", "Koalition", "variable"), 
            measure.vars = "Wkt", value.name = "Wkt")
    m.list 
    
}



########################### do everything in one step ##########################
updateProbabilities <- function(
        party.order, 
        coalitions, 
        superior.coalitions = NULL,
        seed, 
        simulations, 
        surveys.file,
        coalition.results.file, 
        coalition.results.file.ur,
        entry.results.file,
        distrib.fun = sls,
        majority = 300,
        institutes = c("Emnid", "FGW", "Forsa", "Infratest"), 
        mc.cores = 1, 
        max.percent.lent = 10, 
        pooled.surveys.file) {
    
    require(parallel)
    
    surveys <- getSurveys(path.to.surveys = surveys.file, 
            party.order = party.order)
    message("Updating pooled surveys...")
    pooled.surveys <- surveysToPooledSurveys(surveys, institutes = institutes)
    saveRDS(pooled.surveys, pooled.surveys.file)
    
    
    ind1 <- ind2 <- FALSE  
    
    ######## coalition probabilities
    
    ## check if file with coalition probabilities exists
    if( !file.exists(coalition.results.file) ) {
        message(paste(coalition.results.file, "appears not to exist! 
                                Coalition probability calculations will be 
                                performed for all surveys in", 
                        basename(surveys.file), ".\n"))
        coal.res <- NULL
    }
    else{
        coal.res <- readRDS(coalition.results.file)
    }
    ## check if coalition probabilities up to date
    if( !is.null(coal.res) ) {
        
        available <- paste(coal.res$Institut, coal.res$Datum)
        todo <- paste(surveys$Institut, surveys$Datum)
        
        if( all(unique(todo) %in% unique(available)) ) {
            message("Probabilities allready up to date!")
            ind1 <- TRUE
        }
        else{
            surveys.todo <- surveys[!(todo %in% unique(available)), ]
        }  
    }
    else {
        surveys.todo <- surveys
    }
    
    ##FIXME: also a lot of redundant calculation going on
    if( !ind1 ) {
        surveys.list <- split(surveys.todo, paste0(surveys.todo$Datum, 
                        surveys.todo$Institut))
        
        ##FIXME: not really necessary, but code would need some serious restructuring to avoid this
        tab.list <- mclapply(surveys.list, function(z) 
                    createTab(z$Anteil/100, unique(z$Befragte), z$Partei))
        
        if( !is.null(seed) ) set.seed(seed) 
        drn.list <- mclapply(tab.list, function(z) {
                    drawDirichletElections(survey = z, simulations = simulations, 
                            seed = NULL
                    )}, 
                mc.cores = mc.cores)
        message("Updating coalition probabilities...")
        prob.list <- mclapply(seq_along(drn.list), 
                function(z) {
                    getLentVoteCoalitionProbs(
                            dirichlet.draws = drn.list[[z]], 
                            survey = surveys.list[[z]], 
                            majority = majority,
                            distrib.fun = distrib.fun,
                            max.percent.lent = max.percent.lent, 
                            coalitions = coalitions, 
                            superior.coalitions = superior.coalitions)}, 
                mc.cores = mc.cores)      
        prob.tab <- do.call(rbind, prob.list)
        unique.di <- unique(prob.tab[, c("Datum", "Institut")])
        prob.tab <- rbind(coal.res, prob.tab)
        
        saveRDS(prob.tab, file = coalition.results.file)
        
        Sys.chmod(c(coalition.results.file), 
                mode = "0774", use_umask = FALSE)
        
        ##### entry probabilities
        ## check if file with entry probabilities exists
        if( !file.exists(entry.results.file) ) {
            message(paste(entry.results.file, "appears not to exist! Entry probability 
                                    calculations will be performed for all surveys in", 
                            basename(surveys.file), ".\n"))
            entry.res <- NULL
        }
        else{
            entry.res <- readRDS(entry.results.file)
        }
        
        message("Updating entry probabilities...")
        entry.list <- mclapply(drn.list, function(z) { 
                    getLentEntryProbs(dirichlet.draws = z, 
                            max.percent.lent = max.percent.lent)},
                mc.cores = mc.cores)
        nrows.entry <- sapply(entry.list, nrow)
        entry.tab <- do.call(rbind, entry.list)
        entry.tab <- cbind(unique.di[rep(seq_along(nrows.entry), times = nrows.entry), ], 
                Partei = rownames(entry.tab), entry.tab)
        entry.tab <- rbind(entry.res, entry.tab)
        
        saveRDS(entry.tab, file = entry.results.file)
        
        Sys.chmod(c(entry.results.file, pooled.surveys.file), 
                mode = "0774", use_umask = FALSE)
        
    }
    
    
    ##### unrestricted coalition probabilities
    ## check if file with unrestricted coalition probabilities exists
    if( !file.exists(coalition.results.file.ur) ) {
        message(paste(coalition.results.file.ur, "appears not to exist! 
                                Unrestricted coalition probability calculations will be 
                                performed for all surveys in", 
                        basename(surveys.file), ".\n"))
        coal.res.ur <- NULL
    }
    else{
        coal.res.ur <- readRDS(coalition.results.file.ur)
    }
    ## check if coalition probabilities up to date
    if( !is.null(coal.res.ur) ) {
        
        available.ur <- paste(coal.res.ur$Institut, coal.res.ur$Datum)
        todo.ur <- paste(surveys$Institut, surveys$Datum)
        
        if( all(unique(todo.ur) %in% unique(available.ur)) ) {
            message("Unrestricted probabilities allready up to date!")
            ind2 <- TRUE
        }
        else{
            surveys.todo.ur <- surveys[!(todo.ur %in% unique(available.ur)), ]
        }  
    }
    else {
        surveys.todo.ur <- surveys
    }
    
    
    if( !ind2 ) { 
        surveys.list.ur <- split(surveys.todo.ur, paste0(surveys.todo.ur$Datum, 
                        surveys.todo.ur$Institut))
        
        ##FIXME: not really necessary, but code would need some serious restructuring to avoid this
        tab.list.ur <- mclapply(surveys.list.ur, function(z) 
                    createTab(z$Anteil/100, unique(z$Befragte), z$Partei))
        
        if( !is.null(seed) ) set.seed(seed) 
        drn.list.ur <- mclapply(tab.list.ur, function(z) {
                    drawDirichletElections(survey = z, simulations = simulations, 
                            seed = NULL
                    )}, 
                mc.cores = mc.cores)
        
        ## coalition probabilities
        ##FIXME: also a lot of redundant calculation going on
        message("Updating unrestricted coalition probabilities...")
        prob.list.ur <- mclapply(seq_along(drn.list.ur), 
                function(z) {
                    getLentVoteCoalitionProbs(
                            dirichlet.draws = drn.list.ur[[z]], 
                            survey = surveys.list.ur[[z]], 
                            majority = majority,
                            distrib.fun = distrib.fun,
                            max.percent.lent = max.percent.lent, 
                            coalitions = coalitions, 
                            superior.coalitions = NULL)}, 
                mc.cores = mc.cores)      
        prob.tab.ur <- do.call(rbind, prob.list.ur)
        prob.tab.ur <- rbind(coal.res.ur, prob.tab.ur)
        
        saveRDS(prob.tab.ur, file = coalition.results.file.ur)
        Sys.chmod(coalition.results.file.ur, 
                mode = "0774", use_umask = FALSE)
    }
    
}


updatePooledProbabilities <- function(
        party.order, 
        coalitions, 
        superior.coalitions = NULL,
        seed, 
        simulations, 
        pooled.surveys.file, 
        pooled.coalition.file,
        pooled.coalition.file.ur,
        pooled.entry.file,
        majority = majority,
        distrib.fun = sls,
        mc.cores = 20, 
        max.percent.lent = 10) {
    
    require(parallel)
    
    pooledSurveys <- readRDS(pooled.surveys.file)
    
    ind1 <- ind2 <- FALSE
    
    ##### pooled coalition prob
    ## check if file with coalition probabilities exists
    if( !file.exists(pooled.coalition.file) ) {
        message(paste(pooled.coalition.file, "appears not to exist! 
                                Coalition probability calculations will be 
                                performed for all surveys in", 
                        basename(pooled.surveys.file), ".\n"))
        pooled.res <- NULL
    }
    else{
        pooled.res <- readRDS(pooled.coalition.file)
    }
    
    ## check if coalition probabilities up to date
    if( !is.null(pooled.res) ) {
        
        available <- paste(pooled.res$Institut, pooled.res$Datum)
        todo <- paste(pooledSurveys$Institut, pooledSurveys$Datum)
        
        if( all(unique(todo) %in% unique(available)) ) {
            message("Probabilities allready up to date!")
            ind1 <- TRUE
        }
        else{
            surveys.todo <- pooledSurveys[!(todo %in% unique(available)), ]
        }  
    }
    else {
        surveys.todo <- pooledSurveys
    }
    
    if( !ind1) {
        
        surveys.list <- split(surveys.todo, paste0(surveys.todo$Datum, 
                        surveys.todo$Institut))
        
        ##FIXME: not really necessary, but code would need some serious restructuring to avoid this
        ##FIXME: here Anteil %in% c(0,1)
        tab.list <- mclapply(surveys.list, function(z) 
                    createTab(z$Anteil/100, unique(z$Befragte), z$Partei))
        
        if( !is.null(seed) ) set.seed(seed) 
        drn.list <- mclapply(tab.list, function(z) {
                    drawDirichletElections(survey = z, simulations = simulations, 
                            seed = seed
                    )}, 
                mc.cores = mc.cores)
        
        ## coalition probabilities
        ##FIXME: also a lot of redundant calculation going on
        message("Updating pooled coalition probabilities...\n")
        prob.list <- mclapply(seq_along(drn.list), 
                function(z) {
                    getLentVoteCoalitionProbs(
                            dirichlet.draws = drn.list[[z]], 
                            survey = surveys.list[[z]], 
                            majority = majority,
                            distrib.fun = distrib.fun,
                            max.percent.lent = max.percent.lent, 
                            coalitions = coalitions, 
                            superior.coalitions = superior.coalitions)}, 
                mc.cores = mc.cores)      
        prob.tab <- do.call(rbind, prob.list)
        unique.di <- unique(prob.tab[, c("Datum", "Institut")])
        prob.tab <- rbind(pooled.res, prob.tab)
        
        saveRDS(prob.tab, file = pooled.coalition.file)
        Sys.chmod(c(pooled.coalition.file), 
                mode = "0774", use_umask = FALSE)
        
        ########### entry probs
        ## check if file with entry probabilities exists
        if( !file.exists(pooled.entry.file) ) {
            message(paste(pooled.entry.file, "appears not to exist! Entry probability 
                                    calculations will be performed for all surveys in", 
                            basename(pooled.surveys.file), ".\n"))
            entry.res <- NULL
        }
        else{
            entry.res <- readRDS(pooled.entry.file)
        }
        ## entry probabilities
        message("Updating pooled entry probabilities...\n")
        entry.list <- mclapply(drn.list, function(z) { 
                    getLentEntryProbs(dirichlet.draws = z, 
                            max.percent.lent = max.percent.lent)},
                mc.cores = mc.cores)
        nrows.entry <- sapply(entry.list, nrow)
        entry.tab <- do.call(rbind, entry.list)
        entry.tab <- cbind(unique.di[rep(seq_along(nrows.entry), times = nrows.entry), ], 
                Partei = rownames(entry.tab), entry.tab)
        entry.tab <- rbind(entry.res, entry.tab)   
        
        saveRDS(entry.tab, file = pooled.entry.file)
        
        Sys.chmod(c(pooled.surveys.file, pooled.entry.file), 
                mode = "0774", use_umask = FALSE)
        
    }
    
    ###### unrestricted probabilities
    ## check if file with unrestircted coalition probabilities exists
    if( !file.exists(pooled.coalition.file.ur) ) {
        message(paste(pooled.coalition.file.ur, "appears not to exist! 
                                Unrestricted coalition probability calculations will be 
                                performed for all surveys in", 
                        basename(pooled.surveys.file), ".\n"))
        pooled.res.ur <- NULL
    }
    else{
        pooled.res.ur <- readRDS(pooled.coalition.file.ur)
    }
    ## check if unrestricted coalition probabilities up to date
    if( !is.null(pooled.res.ur) ) {
        
        available.ur <- paste(pooled.res.ur$Institut, pooled.res.ur$Datum)
        todo.ur <- paste(pooledSurveys$Institut, pooledSurveys$Datum)
        
        if( all(unique(todo.ur) %in% unique(available.ur)) ) {
            message("Probabilities allready up to date!")
            ind2 <- TRUE
        }
        else{
            surveys.todo.ur <- pooledSurveys[!(todo.ur %in% unique(available.ur)), ]
        }  
    }
    else {
        surveys.todo.ur <- pooledSurveys
    }
    
    if( !ind2 ){
        
        
        surveys.list.ur <- split(surveys.todo.ur, paste0(surveys.todo.ur$Datum, 
                        surveys.todo.ur$Institut))
        
        tab.list.ur <- mclapply(surveys.list.ur, function(z) 
                    createTab(z$Anteil/100, unique(z$Befragte), z$Partei))
        
        if( !is.null(seed) ) set.seed(seed) 
        drn.list.ur <- mclapply(tab.list.ur, function(z) {
                    drawDirichletElections(survey = z, simulations = simulations, 
                            seed = seed
                    )}, 
                mc.cores = mc.cores)
        
        ## unrestricted 
        message("Updating unrestricted pooled coalition probabilities...\n")
        prob.list.ur <- mclapply(seq_along(drn.list.ur), 
                function(z) {
                    getLentVoteCoalitionProbs(
                            dirichlet.draws = drn.list.ur[[z]], 
                            survey = surveys.list.ur[[z]], 
                            majority = majority,
                            distrib.fun = distrib.fun,
                            max.percent.lent = max.percent.lent, 
                            coalitions = coalitions, 
                            superior.coalitions = NULL)}, 
                mc.cores = mc.cores)      
        prob.tab.ur <- do.call(rbind, prob.list.ur)
        prob.tab.ur <- rbind(pooled.res.ur, prob.tab.ur)
        
        
        saveRDS(prob.tab.ur, file = pooled.coalition.file.ur)
        Sys.chmod(c(pooled.coalition.file.ur), 
                mode = "0774", use_umask = FALSE)
        
    }
    
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



############################## new with 4 institutes 

lastSurveys <- function(surveys){
    
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

