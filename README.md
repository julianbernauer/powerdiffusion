## Repository for the book "Power Diffusion and Democracy"
The repository is home of documentation, data and replication material for the book "Power Diffusion and Democracy", <a href="https://www.cambridge.org/de/academic/subjects/politics-international-relations/comparative-politics/power-diffusion-and-democracy-institutions-deliberation-and-outcomes?format=HB">  published by Cambridge University Press </a> in 2019. 

## Authors
<a href="https://www.mzes.uni-mannheim.de/d7/en/profiles/julian-bernauer"> **Julian Bernauer** </a>  (University of Mannheim, MZES) and <a href="https://www.ipw.unibe.ch/about_us/people/prof_dr_vatter_adrian/index_eng.html"> **Adrian Vatter** </a> (University of Bern, IPW)

## How to cite 
Book: Bernauer, Julian and Adrian Vatter (2019). Power Diffusion and Democracy: Institutions, Deliberation and Outcomes. Cambridge: Cambridge University Press. 

Data: Cite the book, and add: Bernauer, Julian and Adrian Vatter (YEAR): Power Diffusion and Democracy Data Repository, https://github.com/julianbernauer/powerdiffusion, [downloaded on DD.MM.YYYY].

## Guide to the repository 
The repository contains the data and replication code for the estimation of scores of power diffusion in four variants (see Chapter 2 of the book for theory) - proportional, decentral, presidential and direct power diffusion (empirics described in Chapter 3 of the book): 

- Indicator data set (61 countries between 1990 and 2015) as <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July_indicators.csv"> .csv </a>, <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July_indicators.dta"> .dta </a> and <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July_indicators.Rdata"> .Rdata </a>
- <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/documentation/codebook.md"> Codebook and Sources </a> 
- <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/code/fairt.r"> Code to estimate the country scores of power diffusion </a> and replicate the figures in Chapter 3 of the book, drawing on indicator data with some additional variables as <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July.csv"> .csv </a>, <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July.dta"> .dta </a> and <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July.Rdata"> .Rdata </a>
- Country scores of power diffusion estimated (mean 1990-2015) with standard deviations as <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July_scores.csv"> .csv </a>, <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July_scores.dta"> .dta </a> and <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/DPD2018July_scores.Rdata"> .Rdata </a> 

The indicator data set is constructed from a number of sources listed <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/documentation/codebook.Rmd">here</a>. The underlying data and code to produce the indicator data set is available at https://dataverse.harvard.edu/dataverse/dpd. 

The repository also provides replication code and data for the chapters of the book analyzing determinants and consequences of power diffusion: 
- Chapter 4 on power diffusion and performance: migrant integration policy (<a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/PDD2018_ch4_perf_mipex.Rdata">data</a>; <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/code/PDD_perf_mipex.R"> code</a>),
corruption (<a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/PDD2018_ch4_perf_corr.Rdata">data</a>; <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/code/PDD_perf_corr.R"> code</a>),
economic inequality (<a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/PDD2018_ch4_perf_inequ.Rdata">data</a>; <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/code/PDD_perf_inequ_github.R"> code</a>),
infant mortality (<a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/PDD2018_ch4_perf_infmort.Rdata">data</a>; <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/code/PDD_perf_infmort_github.R"> code</a>),
- Chapter 5 on power diffusion and legitimacy: <a href="https://github.com/julianbernauer/powerdiffusion/blob/master/data/PDD2018_ch5_leg.Rdata">data for all subanalyses</a>, 
policy congruence (<a href="https://github.com/julianbernauer/powerdiffusion/blob/master/code/PDD_leg_cong_github.r">code</a>),
accountability perceptions (<a href="https://github.com/julianbernauer/powerdiffusion/blob/master/code/PDD_leg_acc_github.r">code</a>),
turnout (<a href="https://github.com/julianbernauer/powerdiffusion/blob/master/code/PDD_leg_turn_github.r">code</a>),
satisfaction with democracy (<a href="https://github.com/julianbernauer/powerdiffusion/blob/master/code/PDD_leg_demsat_github.r">code</a>),
descriptives (<a href="https://github.com/julianbernauer/powerdiffusion/blob/master/code/PDD_leg_descr_github.r"> code</a>),
- Chapter 6 on the explanation of power diffusion: Data and code (under construction)
- Chapter 7 on the convergence of democratic architectures: Data and code (under construction)
- Chapter 8 on subnational power diffusion: Data and code (under construction)

Additonally, we provide scores similar to those of consensus democracy in Lijphart (2012) in the section "Lijphart-Style Scores" as a nod to the grandmaster of empirical patterns of democracy (under construction). 

## Illustration
A Shiny App for the illustration and exploration of the data base is under construction. 


