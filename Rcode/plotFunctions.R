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