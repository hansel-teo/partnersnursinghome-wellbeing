# HOUSEHOLD AND SPOUSE IDENTIFIERS (RAND Longitudinal)
##############################
vars_id  <- list()
vars_id["hhid"] = "Person Specific Identifier: HHold ID"
vars_id["pn"] = "Person Specific Identifier: Person Number"
vars_id["h\\d+\\d*hhid"] = "Household Identifier: HHold ID + SubHHold"
vars_id["s\\d+\\d*hhidpn"] = "Spouse Identifier: Spouse HHIDPN"
##############################

# R SURVEY ADMIN VARIABLES (RAND Longitudinal)
##############################
vars_resp <- list()
vars_resp["iwstat"] = "R interview status"
vars_resp["wthh"] = "Household analysis weight"
vars_resp["wtresp"] = "R person-level analysis weight"
vars_resp["wtr_nh"] = "R NursHm resident analysis weight"
vars_resp["wtcrnh"] = "R combined respondent weight and NursHm resident weight"
vars_resp["proxy"] = "R whether proxy interview"
vars_resp["pexit"] = "Exit interview administered"
vars_resp["iwend$"] = "R Interview End Date"
vars_resp["iwendm"] = "R Interview End Month"
vars_resp["iwendy"] = "R Interview End Year"
##############################

# R DEMOGRAPHICS (RAND Longitudinal)
##############################
vars_demog  <- list()
vars_demog["gender"] = "R gender"
vars_demog["racem"] = "R race"
vars_demog["educ"] = "R education (categ)"
vars_demog["byear"] = "R birth year"
vars_demog["dyear"] = "R death year"
vars_demog["cohbyr"] = "R Cohort based on birth yr"
vars_demog["hrsamp"] = "R HRS sample-age eligible"
vars_demog["bplace"] = "R Place of Birth (Cens Region)"

vars_demog["agey_e"] = "R age at Ivw EndMon"
vars_demog["mstat"] = "R marital status w/partners"
vars_demog["cenreg"] = "R Census Region"
vars_demog["cendiv"] = "R Census Division"
##############################

# R WORK AND INSURANCE (RAND Longitudinal)
##############################
vars_insur <- list()
vars_insur["lbrf"] = "Labor force status"
vars_insur["govmr"] = "Has Gov plan-Medicare"
vars_insur["govmd"] = "Has Gov plan-Medicaid"
vars_insur["hiltc"] = "Has Long Term Care Ins"
vars_insur["tyltc"] = "Type of Long Term Care Ins"
##############################

# R NURSING HOME VARIABLES (RAND Longitudinal)
##############################
vars_nrshom <- list()
vars_nrshom["nrshom$"] <- "Nurs home stay, prv 2 yrs"
vars_nrshom["nrstim$"] <- "Nurs home stays, prv 2 yrs"
vars_nrshom["nrsnit$"] <- "Nights in nurs home, prv 2 yrs"
vars_nrshom["nhmliv$"] <- "Live in Nurs home at Iview"
vars_nrshom["nhmmvy$"] <- "Year moved to Nurs home"
vars_nrshom["nhmmvm$"] <- "Month moved to Nurs home"
vars_nrshom["nhmday$"] <- "Day moved to Nurs home"
##############################

# R CES-D VARIABLES (RAND Longitudinal)
##############################
vars_cesd <- list()
vars_cesd["cesd$"] <- "CESD score"
vars_cesd["depres$"] <- "CESD: Felt depressed"
vars_cesd["effort$"] <- "CESD: Everything an effort"
vars_cesd["sleepr$"] <- "CESD: Sleep was restless"
vars_cesd["whappy$"] <- "CESD: Was happy"
vars_cesd["flone$"] <- "CESD: Felt lonely"
vars_cesd["fsad$"] <- "CESD: Felt sad"
vars_cesd["going$"] <- "CESD: Could not get going"
vars_cesd["enlife$"] <- "CESD: Enjoyed life"
##############################

# R ADLs (RAND Longitudinal)
##############################
vars_adl <- list()
vars_adl["adl"] = ""
vars_adl["adl6"] = "" # 2018v2 onwards
vars_adl["walkr"] = "R Some Diff-Walk across room"
vars_adl["dress"] = "R Some Diff-Dressing"
vars_adl["bath"] = "R Some Diff-Bathing"
vars_adl["eat"] = "R Some Diff-Eating"
vars_adl["bed"] = "R Some Diff-Get in/out bed"
vars_adl["toilt"] = "R Some Diff-Using the toilet"
##############################

# R IADLs (RAND Longitudinal)
##############################
vars_iadl <- list()
vars_iadl["iadlz"] = ""
vars_iadl["iadl5"] = ""
vars_iadl["phone"] = "R Some Diff-Use a telephone"
vars_iadl["money"] = "R Some Diff-Managing money"
vars_iadl["meds"] = "R Some Diff-Take medications"
vars_iadl["shop"] = "R Some Diff-Shop for grocery"
vars_iadl["meals"] = "R Some Diff-Prepare hot meal"
vars_iadl["map"] = "R Some Diff-Use a map"
##############################

# R FUNCTIONAL LIMITATIONS (RAND Longitudinal)
##############################
vars_mobl  <- list()
vars_mobl["walk1"] = "Some Diff-Walk one block"
vars_mobl["walks"] = "Some Diff-Walk several blocks"
vars_mobl["sit"] = "R Some Diff-Sit for 2 hours"
vars_mobl["chair"] = "R Some Diff-Get up fr chair"
vars_mobl["clims"] = "R Some Diff-Clmb sev flt str"
vars_mobl["clim1"] = "R Some Diff-Clmb 1 flt str"
vars_mobl["stoop"] = "R Some Diff-Stoop/Kneel/Crch"
vars_mobl["lift"] = "R Some Diff-Lift/carry 10lbs"
vars_mobl["dime"] = "R Some Diff-Pick up a 5p coin"
vars_mobl["arms"] = "R Some Diff-Rch/xtnd arms up"
vars_mobl["push"] = "R Some Diff-Push/pull lg obj"
##############################

# R Medical care use (RAND Longitudinal)
##############################
vars_med <- list()
vars_med["hosp$"] <- "Hospital stay, prv 2 yrs"
vars_med["homcar$"] <- "Home hlth care, prv 2 yrs"
##############################

# R Expectations (RAND Longitudinal)
##############################
vars_exp <- list()
vars_exp["pnhm5y$"] <- "Prob moving to NHM in 5 yrs"
vars_exp["homcar$"] <- "Home hlth care, prv 2 yrs"
##############################

# R HEALTH AND DISEASES (RAND Longitudinal)
##############################
vars_hlth <- list()
vars_hlth["shlt"] = "Self-report of health"
vars_hlth["hibpe"] = "Ever had hi BP"
vars_hlth["diabe"] = "Ever had diabetes"
vars_hlth["cancre"] = "Ever had cancer"
vars_hlth["lunge"] = "Ever had lung disease"
vars_hlth["hearte"] = "Ever had heart"
vars_hlth["stroke"] = "Ever had stroke"
vars_hlth["psyche"] = "Ever had psych prob"
vars_hlth["arthre"] = "Ever had arthritis"

vars_hlth["alzhee"] = "Ever had alzheimer's"
vars_hlth["demene"] = "Ever had dementia"
vars_hlth["memrye"] = "Ever had memory prob"
##############################

# R ACUTE HEALTH EVENTS (Harmonized HRS)
##############################
vars_acute <- list()
vars_acute["hrtatt"] = "Reports heart attack since last wave"
vars_acute["angin"] = "Reports angina since last wave"
vars_acute["conhrtf"] = "Reports congestive heart failure since last"

vars_acute["fall"] = "Fallen down last 2 years"
vars_acute["fallinj"] = "Injured from fall last 2 years"
vars_acute["hipe"] = "Ever fractured hip"
##############################

# R SPOUSAL CARE (Harmonized HRS)
##############################
vars_spcare  <- list()
vars_spcare["rscare"] = ""
vars_spcare["rscaren"] = ""
vars_spcare["rscaredpw"] = ""
vars_spcare["rscaredpwm"] = ""
vars_spcare["rscarehr"] = ""
vars_spcare["rscarehrm"] = ""
##############################

# R INFORMAL CARE (Harmonized HRS)
##############################
vars_incare  <- list()
vars_incare["r[crf]care"] = ""
vars_incare["r[crf]caren"] = ""
vars_incare["r[crf]caredpw"] = ""
vars_incare["r[crf]caredpwm"] = ""
vars_incare["r[crf]carehr"] = ""
vars_incare["r[crf]carehrm"] = ""
vars_incare["r[crf]carepd"] = ""
##############################

# R FORMAL CARE (Harmonized HRS)
##############################
vars_focare  <- list()
vars_focare["r[pu]fcare"] = ""
vars_focare["r[pu]fcaren"] = ""
vars_focare["r[pu]fcaredpw"] = ""
vars_focare["r[pu]fcaredpwm"] = ""
vars_focare["r[pu]fcarehr"] = ""
vars_focare["r[pu]fcarehrm"] = ""
##############################

# R SUPPLEMENTARY WELLBEING MEASURES (Harmonized HRS)
##############################
vars_supp  <- list()
vars_supp["lsatsc$"] <- "Satisfaction with life scale score 7-point"
vars_supp["satlifez$"] <- "Satisfied with life z-score"

vars_supp["lnlys$"] <- "4-item loneliness summary mean score"
vars_supp["lnlys3$"] <- "3-item loneliness summary mean score"

vars_supp["kcnt$"] <- "Any weekly contact with children in person/phone"
vars_supp["pcnt$"] <- "Any weekly contact with children in person/phone"
vars_supp["rfcnt$"] <- "Any weekly contact with relative/friend in person/phone"
##############################

# R MEDICAL EXPENDITURE (RAND Detailed Imputations)
##############################
vars_mds <- list()
vars_mds["onhm"] <- "Any medical expenditures: Nursing home"
vars_mds["mnhm"] <- "Amount medical expenditures: Nursing home"

vars_mds["ohhc"] <- "Any medical expenditures: Nursing home"
vars_mds["mhhc"] <- "Amount medical expenditures: Nursing home"

vars_mds["oopmd$"] <- "Total Out of Pocket Medical Expenditures"

vars_mds["iossdi$"] <- "Receives:R SSI + SS Disability"
##############################

# HH DEMOGRAPHICS (RAND Longitudinal)
##############################
vars_hhold <- list()
vars_hhold["pickhh"] <- ""
vars_hhold["hhresp$"] <- "Number of core respondents in HH"
vars_hhold["cpl$"] <- "Whether couple HHold"

vars_hhold["hhres$"] <- "Number of residents"
vars_hhold["child$"] <- "Number of living children"
##############################

# HH STRUCTURE AND SOCIAL CONTACT (Harmonized HRS)
##############################
vars_fam <- list()
vars_fam["lvwith$"] <- "Living arrangement"
vars_fam["kidu14$"] <- "Number children/grandchildren in hh under age 14"

vars_fam["kcnt$"] <- "Any weekly contact with children in person/phone"
vars_fam["pcnt$"] <- "Any weekly contact with children in person/phone"
vars_fam["rfcnt$"] <- "Any weekly contact with relative/friend in person/phone"
##############################

# HH FINANCES (RAND Detailed Imputations)
##############################
vars_fin <- list()
vars_fin["atotb$"] <- "HH: Total Wealth"
vars_fin["atotn$"] <- "HH: Total Non-Housing Assets"
vars_fin["atotf$"] <- "HH: Non-Housing Financial Wealth"
vars_fin["atoth$"] <- "HH: Net Value of House /prim res"
vars_fin["aohous$"] <- "HH: Owns home"

vars_fin["itot$"] <- "HH: Total household income"
##############################

