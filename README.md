





<a name="introduction">coalitions</a>
==========

During election period in multi-party democracies media often reports survey 
results as percentages of respondents who would vote for a certain party, 
which results in headlines similar to "Party A and party B have narrow majority", 
based on sum of their respective vote counts compared to other coalitions (e.g. 
party C and party D). 

This R-package will implement the methods described 
<a href= "http://www.stablab.stat.uni-muenchen.de/sites/files/wahlen.pdf)"> 
here (in German)</a>. 
In short, we assume the reported survey results to have a *Multinomial* distribution, 
which, assuming a not-informative *Dirichlet-Prior* (Jeffrey's prior) yields a 
Posterior which is also *Dirichlet*. Based on this Posterior we run a 
*Monte-Carlo simulation*, where we <br/>

    1. sample n election results from the Posterior
    2. calculate the seat distribution n times

Based on these steps we can derive the probabilities of a majority for certain 
coalitions of interest. <br/>



<a name="example">Basic example</a>
===

Here's an example based on a survey by the *Forsa* from the 05.06.2013 with 2508
respondents. Results are shown in the table beneath

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Mon Dec 02 15:54:19 2013 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Results of a Forsa survey released on the fifth 
  of June 2013 </CAPTION>
<TR> <TH>  </TH> <TH> party </TH> <TH> votes </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> CDU/CSU </TD> <TD align="right"> 41.00 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> SPD </TD> <TD align="right"> 24.00 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> GRUENE </TD> <TD align="right"> 13.00 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD> FDP </TD> <TD align="right"> 4.00 </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> LINKE </TD> <TD align="right"> 8.00 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD> PIRATEN </TD> <TD align="right"> 3.00 </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> AfD </TD> <TD align="right"> 3.00 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD> Others </TD> <TD align="right"> 4.00 </TD> </TR>
   </TABLE>


Let's say we want to know the probability for a seat-majority in the parliament 
for two opposing coalitions, *CDU/CSU-FDP* on the one and *SPD-GRUENE-LINKE* on 
the other hand, assuming the survey can be viewed as a simple random sample of 
eligible voters. Simple addition of the votes in percent the respective parties 
received obviously doesn't contain much information. Both coalitions in consideration
would get 45% of the votes (41 + 4 and 24 + 13 + 8 respectively) in this example, 
but that doesn't tell us much about the probability for the majority of seats in 
the parliament, since one has to factor in <a name="issues"/>

1. The threshold of votes for a party to be able to enter the parliament in the 
first place (5% in Germany).
2. The number of votes redistributed (the more parties fail to jump over the 5%   
  hurdle and the closer they are to it, the more votes are redistributed).
3. The fact that the survey is an (ideally random) sample of voters a thus 
  insecurity about the "real" preferences needs to be taken into account.
4. The specific rules of seat distribution (given a specific vote count, 
  e.g. <a href="http://www.wahlrecht.de/verfahren/stlague12.html" target="_blank">
  Sainte-Laguë/Scheppers</a> in Germany).


On request of the German ZEIT magazine the 
<a href="http://www.stablab.stat.uni-muenchen.de/index" target = "_blank">
Statistical Consulting Unit
</a> at the <a href="http://www.stat.uni-muenchen.de/index_e.html" target="_blank">
Departement of Statistics, Munich</a> developed a method that takes all of the points 
mentioned above into consideration to derive probabilities for the coalitions of 
interest.

As mentioned in the [introduction](#introduction), our approach is based on a 
*Bayesian* formulation of election/survey results and we use *Monte Carlo* 
simulations for an approximation of the posterior distribution of the probability 
for a seat majority.

Thus, assuming a *Multinomial* distribution of the survey results and using 
a non-informative *conjugate Dirichlet* as the *Prior*, the *Posterior* is also a 
*Dirichlet* 

\[\begin{equation}
    Dir(n_1 + 1/2, n_2 + 1/2, ..., n_k + 1/2)
\end{equation}
\]

Where $n_i$ are the votes received by party $i$, $i = 1,...,k$.

Thus picking up the [example](#example) above we get this *Posterior*:


```r
forsa <- data.frame(party = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", 
    "AfD", "Others"), votes = c(41, 24, 13, 4, 8, 3, 3, 4))
posterior <- forsa$votes/100 * 2508 + 1/2
names(posterior) <- forsa$party
posterior
```

```
## CDU/CSU     SPD  GRUENE     FDP   LINKE PIRATEN     AfD  Others 
## 1028.78  602.42  326.54  100.82  201.14   75.74   75.74  100.82
```


Note that we use the actual sample size from the survey, because this controls the 
variance when drawing random number from the *Dirichlet* distribution.

A random sample from the *posterior* can be drawn with the 
`rdirichlet` function from the **MCMCpack** library


```r
library(MCMCpack)
sample_1 <- rdirichlet(n = 1, alpha = posterior)
colnames(sample_1) <- names(posterior)
sample_1
```

```
##      CDU/CSU    SPD GRUENE     FDP   LINKE PIRATEN     AfD  Others
## [1,]  0.4081 0.2461 0.1278 0.03496 0.07915 0.03108 0.03627 0.03656
```


By considering such a draw  an election outcome we can calculate the percentage of 
votes each party receives after dropping parties with less than 
5% of the votes and redistributing these votes. Using Saint-Lague/Scheppers we can 
than calculate the seat-distribution for every such draw. This is a little 
cumbersome, therefore this packages provides functions to do so in a more coherent 
fashion. 

Let's again consider the above [example](#example): 

First we create an survey object from the survey results: 


```r
forsa <- as_survey(votes.in.perc = c(0.41, 0.24, 0.13, 0.04, 0.08, 0.03, 0.03, 
    0.04), samplesize = 2508, parties = c("CDU/CSU", "SPD", "GRUENE", "FDP", 
    "LINKE", "PIRATEN", "AfD", "Others"))
forsa
```

```
##     party votes.in.perc   votes
## 1 CDU/CSU          0.41 1028.28
## 2     SPD          0.24  601.92
## 3  GRUENE          0.13  326.04
## 4     FDP          0.04  100.32
## 5   LINKE          0.08  200.64
## 6 PIRATEN          0.03   75.24
## 7     AfD          0.03   75.24
## 8  Others          0.04  100.32
```


`as_survey` calculates vote count from votes in percent and performs some 
sanity checks, such that number of parties equals number of entered percentages 
and votes in percent add up to $1$.

Given the survey (which can be an actual survey result or a random sample from 
the *Posterior*) we can calculate the 


```r
result <- redistribute(forsa, hurdle = 0.05)
result  ## votes in percent after redistribution
```

```
##     party votes.in.perc  votes
## 1 CDU/CSU       0.47674 1028.3
## 2     SPD       0.27907  601.9
## 3  GRUENE       0.15116  326.0
## 5   LINKE       0.09302  200.6
```

```r
seats <- sls(result, seats = 598)  ## calculate seat distribution in parliament via Sainte-Lague/Scheppers
seats
```

```
##     party votes.in.perc  votes seats
## 1 CDU/CSU       0.47674 1028.3   285
## 2  GRUENE       0.15116  326.0    90
## 3   LINKE       0.09302  200.6    56
## 4     SPD       0.27907  601.9   167
```

```r

sum(seats$seats[seats$party != "CDU/CSU"])
```

```
## [1] 313
```


In this case, if the survey did reflect the actual preferences in the population, the 
coalition of *SPD-GRUENE-LINKE* would get the seats needed for 
a majority (300). But we don't know that and need to take random sample deviation 
into account. Therefore instead of using the actual survey we determine the posterior 
as described above and draw 10000 election results from the posterior: 


```r
dirichlet.draws <- draw_from_posterior(survey = forsa, nsim = 10000, seed = 123, 
    prior = NULL)
```

```
## Error: Objekt 's' nicht gefunden
```

```r
head(dirichlet.draws)
```

```
## Error: Objekt 'dirichlet.draws' nicht gefunden
```


The prior defaults to Jeffrey's prior, but it's up to the user to plug in 
other prior information. For example one could use the (weighted) results of 
prior surveys as prior information. 

Now we can for example calculate the probabilities of jumping over the 5% hurdle, 
given survey results, for each of the parties in the survey


```r
get_entryprobability(dirichlet.draws)
```

```
## Error: Objekt 'dirichlet.draws' nicht gefunden
```


or calculate the seat distribution for each of the draws and determine the 
probabilities for a seat-majority for the coalitions of interest


```r
seat.distributions <- get_seat_distribution(dirichlet.draws, survey = forsa, 
    distrib.fun = sls)
seat.distributions[1:2]
```

```
## [[1]]
##     party votes.in.perc  votes seats
## 1 CDU/CSU        0.4670 1016.4   279
## 2  GRUENE        0.1369  298.0    82
## 3   LINKE        0.1043  227.0    62
## 4     SPD        0.2918  635.2   175
## 
## [[2]]
##     party votes.in.perc  votes seats
## 1 CDU/CSU       0.48435 1039.9   290
## 2  GRUENE       0.15117  324.6    90
## 3   LINKE       0.08746  187.8    52
## 4     SPD       0.27702  594.8   166
```

```r
length(seat.distributions)
```

```
## [1] 10000
```

```r

## coalition probability CDU/CSU-FDP
get_coalition_probability(seat.distributions, coalition = c("CDU/CSU", "FDP"))
```

```
## [1] 0.018
```

```r
## coalition probability SPD-GRUENE-LINKE
get_coalition_probability(seat.distributions, coalition = c("SPD", "GRUENE", 
    "LINKE"))
```

```
## [1] 0.9755
```


Thus, given the [example](#example), the probability of a seat-majority for 
*CDU/CSU-FDP* wasn't very big in June, although this changed during the course of 
the respective campaigns. See 
<a href="http://www.stablab.stat.uni-muenchen.de/Koalitionen2013" target="_blank">
here</a> for development of respective coalition probabilities in 
the period before the German election in 2012.















