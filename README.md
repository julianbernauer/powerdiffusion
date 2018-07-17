## Repository for the book "Power Diffusion and Democracy"
The repository is home of documentation, data and replication material for the paper book "Power Diffusion and Democracy", forthcoming with Cambridge University Press. 

## Authors
**Julian Bernauer (University of Mannheim, MZES)** and **Adrian Vatter (University of Bern, IPW)**

## How to cite 
Book: Bernauer, Julian and Adrian Vatter (Forthcoming). Power Diffusion and Democracy: Institutions, Deliberation and Outcomes. Cambridge: Cambridge University Press. 

Data: Cite the book, and add: Book: Bernauer, Julian and Adrian Vatter (YEAR): Power Diffusion and Democracy Data Repository, https://github.com/julianbernauer/powerdiffusion, [downloaded XX].

## Guide to the repository 
The repository contains the data and replication code for the etimation of scores of power diffusion in four variants (see Chapter 2 of the book for theory) - proportional, decentral, presidential and direct power diffusion (empirics described in Chapter 3 of the book): 

- Indicator data set (61 countries between 1990 and 2015) as <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July_indicators.csv"> .csv </a>, <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July_indicators.dta"> .dta </a> and <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July_indicators.Rdata"> .Rdata </a>
- Codebook and Sources 
- <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/code/fairt.r"> Code to estimate the country scores of power diffusion </a> and replicate the figures 
- Country scores of power diffusion estimated (mean 1990-2015) with standard deviations as <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July_scores.csv"> .csv </a>, <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July_scores.dta"> .dta </a> and <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July_scores.Rdata"> .Rdata </a> 

The indicator data set is constructed from a number of sources listed here. The underlying data and code to produce the indicator data set is available at https://dataverse.harvard.edu/dataverse/dpd. 

The repository also provides replication code and data for the chapters of the book analyzing determinants and consequences of power diffusion: 
- Chapter 4 on power diffusion and performance: migrant integration policy (<a href="https://github.com/julianbernauer/powerdiffusion/blob/master/code/PDD2018_ch4_perf_mipex.Rdata">data</a>; <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/PDD_perf_mipex.r"> code</a>),
- Chapter 5 on power diffusion : (data and code) 
- Chapter 6 on the explanation of power diffusion: Data and code 
- Chapter 7 on the convergence of democratic architectures: Data and code 
- Chapter 8 on subnational power diffusion: Data and code 

Additonally, we provide scores similar to those of consensus democracy in Lijphart (2012) in the section "Lijphart-Style Scores" as a nod to the grandmaster of empirical patterns of democracy. 


## Some Illustrations 

Here is a plot of proportional power diffusion in the countries covered: 

![Map of Proportional Power Diffusion (1990-2015)](figures/Fig_pd1.jpeg)

**Figure: Map of Proportional Power Diffusion (1990-2015)**

Bivariate maps of the four types of power diffusion: 

