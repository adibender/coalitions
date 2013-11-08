coalitions
==========

During election period in multi-party democracies media often reports survey 
results as percentages of respondents who would vote for a certain party, 
which results in headlines similar to "Party A and party B have narrow majority", 
based on sum of their respective vote counts compared to other coalitions (eg. 
party C and party D). 

This R-package will implement the methods described 
<a href= "http://www.stablab.stat.uni-muenchen.de/sites/files/wahlen.pdf)"> 
here (in german)</a>. 
In short, we assume the reported survey results to have a *Multinomial* distribution, 
which, assuming a un-informative *Dirichlet-Prior* (Jeffrey's prior) yields a 
Posterior which is also *Dirichlet*. Based on this Posterior we run a 
*Monte-Carlo simulation*, where we <br/>

    1. sample n election results from the Posterior
    2. calculate the seat distribution n times
    (depends on legislation, Saint-Lague/Scheppers in germany) 

Based on these steps we can derive the probabilities of a majority for certain 
coalitions of interest. <br/>


<h3>Note:</h3>

This package is in development and might not work until otherwise noted here. 

Test
