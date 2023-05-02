
### REGRESSION SPECIFICATIONS
####################################
rdiags <- c("rhearte_pre",  "rlunge_pre", "rstroke_pre",
            "rcancre_pre", "rhibpe_pre", "rdiabe_pre", "rpsyche_pre", "rarthre_pre")
racute <- c("rhrtatt_pre",  "rangin_pre", "rconhrtf_pre")
radls <- c("rwalkra_pre", "rdressa_pre", "rbatha_pre", "reata_pre", "rbeda_pre", "rtoilta_pre",  "rclim1a_pre")


### BASELINE
############
rhs <- paste0(c("safemale_pre", 
                "sagey_e_pre",
                # "semployed_pre", "sacollege_pre", "sahsdropout_pre", "sanonwhite_pre",
                # "hchild_pre", "hspouseonly_pre",
                # "rafemale_pre", "racollege_pre", "ranonwhite_pre",
                "ragey_e_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rimpaired_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rdemented_i_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, rdiags), collapse = " + ")
# rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, racute), collapse = " + ")
# rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, radls), collapse = " + ")
# rhs <- paste0(c(rhs, "radlcount_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rhosp_e_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rhomecare_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rgovmd_pre", "rhiltc_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rinsur_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "hatotf_defl_qtile_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "hatoth_defl_qtile_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "hitot_defl.oecdeq_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscare_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_grp_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_grp2_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_qtile"), collapse = " + ")

rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre", "I(rrscarehpw_i_pre^3)"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_i_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscaredur_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur1_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur2_pre"), collapse = " + ")

regspec_main <- formula(paste0(" ~ ", rhs))

rm(rhs)
############


### NON-CAREGIVERS
############
rhs <- paste0(c("safemale_pre", "sagey_e_pre", "ragey_e_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rimpaired_i_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, rdiags), collapse = " + ")
# rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

regspec_noncare <- formula(paste0(" ~ ", rhs))
############
rm(rhs)

### CAREGIVERS
############
rhs <- paste0(c("safemale_pre", 
                "sagey_e_pre", 
                "ragey_e_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rimpaired_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rdemented_i_pre"), collapse = " + ")

rhs <- paste0(c(rhs, rdiags), collapse = " + ")
rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, radls), collapse = " + ")
# rhs <- paste0(c(rhs, "radlcount_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscarehpw_grp_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_grp2_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscarehpw_qtile"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscaredur_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur1_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur2_pre"), collapse = " + ")

regspec_caregive <- formula(paste0(" ~ ", rhs))
############
rm(rhs)


# SUBGROUP ANALYSIS
### CARE DURATION
############
rhs <- paste0(c("safemale_pre", 
                "sagey_e_pre", 
                "ragey_e_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rdemented_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rimpaired_i_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, rdiags), collapse = " + ")
rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "radlcount_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscarehpw_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_qtile"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscaredur1_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscaredur2_pre"), collapse = " + ")

regspec_caredur <- formula(paste0(" ~ ", rhs))
############
rm(rhs)



### CARE INTENSITY
############
rhs <- paste0(c("safemale_pre", 
                "sagey_e_pre", 
                "ragey_e_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rdemented_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rimpaired_i_pre"), collapse = " + ")

rhs <- paste0(c(rhs, rdiags), collapse = " + ")
# rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "radlcount_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rrscaredur1_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur2_pre"), collapse = " + ")

regspec_careint <- formula(paste0(" ~ ", rhs))
############
rm(rhs)



### GENDER SUBGROUPS
############
rhs <- paste0(c("sagey_e_pre", "ragey_e_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rimpaired_i_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscarehpw_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rrscaredur1_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur2_pre"), collapse = " + ")


regspec_gender <- formula(paste0(" ~ ", rhs))
############
rm(rhs)


### AD DIAGNOSIS SUBGROUPS
############
rhs <- paste0(c("safemale_pre", "sagey_e_pre", "ragey_e_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rimpaired_i_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscarehpw_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rrscaredur1_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur2_pre"), collapse = " + ")


regspec_memrye <- formula(paste0(" ~ ", rhs))
############
rm(rhs)


### ADL SUBGROUPS
############
rhs <- paste0(c("safemale_pre", "sagey_e_pre", "ragey_e_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscarehpw_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rrscaredur1_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur2_pre"), collapse = " + ")


regspec_adl <- formula(paste0(" ~ ", rhs))
############
rm(rhs)


### COGNITIVE FUNCTION SUBGROUPS
############
rhs <- paste0(c("safemale_pre", "sagey_e_pre", "ragey_e_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rinsur_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscarehpw_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rrscaredur1_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur2_pre"), collapse = " + ")

regspec_cogfunction <- formula(paste0(" ~ ", rhs))
############
rm(rhs)


# FALSIFICATION TESTS
############
rhs <- paste0(c("safemale_pre", 
                "sagey_e_pre", 
                "ragey_e_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
# 
# rhs <- paste0(c(rhs, rdiags), collapse = " + ")
# rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")
# 
# rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")
# 
# rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

regspec_falsification <- formula(paste0(" ~ ", rhs))
############
rm(rhs)



### POOLED - BASELINE
############
rhs <- paste0(c("safemale_pre", "sagey_e_pre", "ragey_e_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rimpaired_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rdemented_i_pre"), collapse = " + ")

rhs <- paste0(c(rhs, rdiags), collapse = " + ")
# rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
rhs <- paste0(c(rhs, racute), collapse = " + ")
# rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, radls), collapse = " + ")
# rhs <- paste0(c(rhs, "radlcount_grp_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rhosp_e_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rhomecare_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rgovmd_pre", "rhiltc_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rinsur_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "hatotf_defl_qtile_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "hatoth_defl_qtile_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "hitot_defl.oecdeq_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscare_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscarehpw_grp2_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_qtile"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre", "I(rrscarehpw_i_pre^3)"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscarehpw_i_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscaredur_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur1_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur2_pre"), collapse = " + ")

regspec_main_pooled <- formula(paste0(" ~ ", rhs))

rm(rhs)
############

### POOLED -  NON-CAREGIVERS
############
rhs <- paste0(c("safemale_pre", "sagey_e_pre", "ragey_e_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rimpaired_i_pre"), collapse = " + ")

rhs <- paste0(c(rhs, rdiags), collapse = " + ")
# rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, racute), collapse = " + ")
rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

regspec_noncare_pooled <- formula(paste0(" ~ ", rhs))
############
rm(rhs)

### POOLED - CAREGIVERS
############
rhs <- paste0(c("safemale_pre", 
                "sagey_e_pre", 
                "ragey_e_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rimpaired_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rdemented_i_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, rdiags), collapse = " + ")
rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, racute), collapse = " + ")
rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, radls), collapse = " + ")
# rhs <- paste0(c(rhs, "radlcount_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscare_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscarehpw_grp2_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_qtile"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre", "I(rrscarehpw_i_pre^3)"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscarehpw_i_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscaredur_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur1_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur2_pre"), collapse = " + ")

regspec_caregive_pooled <- formula(paste0(" ~ ", rhs))
############
rm(rhs)


### CEM- NON-CAREGIVERS
############
rhs <- paste0(c("safemale_pre", "sagey_e_pre", "ragey_e_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rimpaired_i_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, rdiags), collapse = " + ")
rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

regspec_noncare_cem <- formula(paste0(" ~ ", rhs))
############
rm(rhs)

### CEM - CAREGIVERS
############
rhs <- paste0(c("safemale_pre", 
                "sagey_e_pre", 
                "ragey_e_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rimpaired_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rdemented_i_pre"), collapse = " + ")

rhs <- paste0(c(rhs, rdiags), collapse = " + ")
# rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, radls), collapse = " + ")
# rhs <- paste0(c(rhs, "radlcount_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscarehpw_grp_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_grp2_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_qtile"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscaredur_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur1_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur2_pre"), collapse = " + ")

regspec_caregive_cem <- formula(paste0(" ~ ", rhs))
############
rm(rhs)

### CAREGIVERS
############
rhs <- paste0(c("safemale_pre", 
                "sagey_e_pre", 
                "ragey_e_pre"), collapse = " + ")

rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rimpaired_i_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rdemented_i_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, rdiags), collapse = " + ")
rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, radls), collapse = " + ")
# rhs <- paste0(c(rhs, "radlcount_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rshlt_grp_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscarehpw_grp_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_grp2_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscarehpw_qtile"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre"), collapse = " + ")

# rhs <- paste0(c(rhs, "rrscaredur_grp_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur1_pre"), collapse = " + ")
rhs <- paste0(c(rhs, "rrscaredur2_pre"), collapse = " + ")

regspec_caregive_caliper <- formula(paste0(" ~ ", rhs))
############
rm(rhs)