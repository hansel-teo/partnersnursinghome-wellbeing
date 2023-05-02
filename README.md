# Replication package for "The impact of a partner’s nursing home entry on individuals' mental wellbeing"

This repo consists of 5 subdirectories which when run sequentially produce Tables 1-4 and Figures 2-7 in the main text of the paper.
To replicate these results, copy all subdirectories into `/your_working_directory` and replace `dir_working <- your_working_directory` in the following code.

### Base data
The analysis is based on data from the Health and Retirement Study and uses 
+ RAND HRS Longitudinal File 2018 Version 2 `randhrs1992_2018v2.dta`
+ RAND HRS Detailed Imputations File 2018 (V2) `randhrsimp1992_2018v2.dta`
+ Harmonized HRS Version C (1992-2019) `H_HRS_c.dta`
+ Langa-Weir Classification of Cognitive Function (1995-2018) `cogfinalimp_9518wide.dta`

All raw data files should be located in the same subdirectory and its location defined as `dir_input <- ` in the following code.

The HRS (Health and Retirement Study) is sponsored by the National Institute on Aging (grant number NIA U01AG009740) and is conducted by the University of Michigan. 
The data can be obtained from https://hrs.isr.umich.edu/ subject to registration. 

## Step 1 - Construct HRS panel
Subdirectory `/01ConstructData/` contains code to construct the analysis dataset. 

It requires: , , all to be located in a directory "~/data/hrs/"

All steps can be executed by running `ConstructData.R` which will:
1. Extract and join required variables into a panel in long format using `extract.R`
2. Clean and define new variables using `prep_data.R`
3. Construct a panel of leads and lags to be used in sample selection using `panel_leadslags.R`

## Step 2 - Sample selection

## Step 3 - Propensity score estimation and matching

## Step 4 - Estimation

## Step 5 - Produce tables and figures
