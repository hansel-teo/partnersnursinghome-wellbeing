# Replication package for "The impact of a partner’s nursing home entry on individuals' mental wellbeing"

This repo consists of 5 subdirectories which when run sequentially produce Tables 1-4 and Figures 2-7 in the main text of the paper.
To replicate these results, copy all subdirectories into a working directory and replace `dir_working <- paste0(dir_root, "projects/nhspouse_wellbeing/")` with your directory.

## Step 1 - Construct HRS panel
Subdirectory `/01ConstructData/` contains code to construct the analysis dataset. 

It requires: `randhrs1992_2018v2.dta`, `randhrsimp1992_2018v2.dta`, `H_HRS_c.dta`, all to be located in a directory "~/data/hrs/"

All steps can be executed by running `ConstructData.R` which will:
1. Extract and join required variables into a panel in long format using `extract.R`
2. Clean and define new variables using `prep_data.R`
3. Construct a panel of leads and lags to be used in sample selection using `panel_leadslags.R`

## Step 2 - Sample selection

## Step 3 - Propensity score estimation and matching

## Step 4 - Estimation

## Step 5 - Produce tables and figures
