---
title: 'coalitions: Coalition probabilities in multi-party democracies'
tags:
  - coalitions
  - rstats
  - coalition probabilities
  - political science
authors:
 - name: Andreas Bender
   orcid: 0000-0001-5628-8611
   affiliation: "1"
 - name: Alexander Bauer
   orcid: 0000-0003-3495-5131
   affiliation: 1
affiliations:
 - name: Statistical Consulting Unit, StaBLab, LMU Munich
   index: 1
date: 03 March 2018
bibliography: paper.bib
---

# Summary

In multi-party democracies, election coverage usually focuses on raw results
from polls on questions like

> Who would you vote for if the election was tomorrow?

Whether a coalition (union of multiple parties) will obtain enough votes to form a governing coalition is discussed by adding up votes obtained by the parties in question, while ignoring sample uncertainty and redistribution of votes for parties beneath a specific threshold (e.g., 5% in Germany).


The [**R**](https://www.r-project.org/) [@R] package [`coalitions`](https://adibender.github.io/coalitions/) [@coalitions_zenodo] implements methods that overcome those shortcomings and quantifies sample uncertainty in terms of probabilities for events of interest. Specifically, it contains functions to

  - Obtain survey results from different polling agencies,
  - Aggregate (pool) multiple surveys (from different pollsters) within a pre-specified time-window, taking into account the correlation between different polling agencies
  - Perform Monte Carlo simulations of election outcomes based on the (pooled) survey results
  - Redistribute votes based on the method specific to the election of interest (e.g., Saint-Lague-Scheppers for German *Bundestag* election)
  - Calculate Bayesian posterior probabilities for specific events, e.g., to obtain enough votes (> 50%) to form a governing coalition


To get started

- the [workflow vignette ](https://adibender.github.io/coalitions/articles/workflow.html) describes the usual steps during the analysis
- the [pooling vignette](https://adibender.github.io/coalitions/articles/pooling.html) gives details on the aggregation of multiple surveys.

An example for the (backend) application of the package can be found at

- [http://koala.stat.uni-muenchen.de](http://koala.stat.uni-muenchen.de),

where it is applied to German (federal and state wide) elections/surveys.


Currently, the functionality focuses on German federal and state-wide elections. However, it can be easily extended to other multi-party democracies, given the user can obtain the necessary data and transform it to a suitable format. For example, the methods have been successfully applied to
calculate coalition probabilities for the 2017 elections in Austria. <br>
Contributions are welcome at: [https://github.com/adibender/coalitions](https://github.com/adibender/coalitions)




# References
