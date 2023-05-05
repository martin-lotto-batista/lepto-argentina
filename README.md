<div align="center">

# Towards a Leptospirosis Early Warning System in North-Eastern Argentina

<div align="justify">

Data and R code to support Lotto Batista, Rees et al., (2023). Towards a Leptospirosis Early Warning System in North-Eastern Argentina. *The Journal of the Royal Society Interface.*

The analyses were done using R version 4.2.3 (2023-03-15).

To cite this repository:

> *Lotto Batista, Martín, Rees, Eleanor M, Gómez, Andrea, López, Soledad, Castell, Stefanie, Kucharski, Adam J, Ghozzi, Stéphane, Müller, Gabriela V, & Lowe, Rachel. (2023). Data and R-code to accompany 'Towards an Early Warning System for Leptospirosis in North-Eastern Argentina' (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.7865639*

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7865639.svg)](https://doi.org/10.5281/zenodo.7865639)

## Abstract    
Leptospirosis is a zoonotic disease with a high burden in Latin America, including North-Eastern Argentina, where flooding events linked to El Niño are associated with leptospirosis outbreaks. The aim of this study was to evaluate the value of using hydrometeorological indicators to predict leptospirosis outbreaks in this region. 

We quantified the effects of El Niño, precipitation, and river height on leptospirosis risk in Santa Fe and Entre Ríos provinces between 2009 and 2020, using a Bayesian modelling framework. Based on several goodness of fit statistics, we selected candidate models using a long-lead El Niño 3.4 index and shorter-lead local climate variables. We then tested predictive performance to detect leptospirosis outbreaks using a two-stage early warning approach. 

Three-month lagged Niño 3.4 index and one-month lagged precipitation and river height were positively associated with an increase in leptospirosis cases in both provinces. El Niño models correctly detected 89% of outbreaks, while short-lead local models gave similar detection rates with lower number of false positives.

Climatic events are strong drivers of leptospirosis incidence in North-East Argentina. Impact-based forecasting models could be used as outbreak prediction tools in an early warning system harnessing the predictive power of hydrometeorological drivers of disease.

## Contents

* [data](/data/): folder containing the database (.csv), and shapefiles (.shp, .shx, .prj, .dbf, .rds)
* [figures](/figures/): folder containing figures from the main text and supplementary materials (.eps and .pdf)
* [model_out](/model_out/): folder containing model outputs (.rds)
* [00_source_functions](00_source_functions.R): `R` Script to attach and install packages, load data and create functions
* [01_distribution_check](01_distribution_check.R): `R` Script to check probability distribution (Poisson vs Negative Binomial)
* [02_fit_models](02_fit_models.R): `R` Script to fit all combinations of variables per province
* [03_out_of_sample_predictions](03_out_of_sample_predictions.R): `R` Script to compute out of sample predictions with candidate models
* [04_main_figures](04_main_figures.R): `R` Script to create figures and tables for main text
* [05_supplementary_figures](05_supplementary_figures.R): `R` Script to create figures and tables for supplementary materials
    

#### Contact about this repository: martin.lotto@bsc.es
