# Replication package for "The impact of a partner’s nursing home entry on individuals' mental well-being"

https://doi.org/10.1016/j.socscimed.2023.115941

This repo consists of 5 subdirectories which when run sequentially produce Tables 1-4 and Figures 2-7 in the main text of the paper.
To replicate these results, copy all subdirectories into your working directory (defined as `dir_working` in the following code).

### Base data
The analysis is based on data from the Health and Retirement Study and uses 
+ RAND HRS Longitudinal File 2018 Version 2 `randhrs1992_2018v2.dta`
+ RAND HRS Detailed Imputations File 2018 (V2) `randhrsimp1992_2018v2.dta`
+ Harmonized HRS Version C (1992-2019) `H_HRS_c.dta`
+ Langa-Weir Classification of Cognitive Function (1995-2018) `cogfinalimp_9518wide.dta`

The HRS (Health and Retirement Study) is sponsored by the National Institute on Aging (grant number NIA U01AG009740) and is conducted by the University of Michigan. 
The data can be obtained from https://hrs.isr.umich.edu/ subject to registration. 

All raw data files should be located in the same subdirectory (defined as `dir_input` in the following code).

## Step 1 - Construct HRS panel
`/01constructdata/` contains code to construct the analysis dataset. 

Running `ConstructData.R` will:
1. Extract and join required variables into a panel in long format using `extract.R`
2. Clean and define new variables using `prep_data.R`
3. Construct a panel of leads and lags to be used in sample selection using `panel_leadslags.R`

## Step 2 - Sample selection
`/02selectsample/` contains code which selects candidate treated and never-treated individuals based on pre-treatment characteristics.

Running `SelectSample.R` will:
1. Construct index of nursing home entry using `define_treatindex.R`
2. Select treated individuals based on sample selection criteria using `sampselect_treated.R`
3. Select never-treated individuals based on sample selection criteria using `sampselect_nevtreat.R`
4. Keep cases with required covariates using `samp_prematch.R` and `durations.R`

## Step 3 - Propensity score estimation and matching
`/03preanalysis/` contains code which estimates propensity score models and matches on those propensity scores to construct the analysis sample

Requires `MatchIt`

Running `MatchingPScoreCarestat.R` will:
1. Estimate (logit) propensity score model, define trimmed p-scores and perform Nearest-neighbour matching for non-caregivers (using `matching_pscore_noncare.R`) and caregivers (using `matching_pscore_caregive.R`)
2. Construct analysis panel dataset 

## Step 4 - Estimation
`/04estimation/` contains code which performs the regression analysis

Requires `did`


## Step 5 - Produce tables and figures
`/05results/` contains code which compiles the Tables 1-4 and plots the Figures 2-7 in the paper.
