# ### REGRESSION SPECIFICATION
# ####################################
# rdiags <- c("rhearte_pre",  "rlunge_pre", "rstroke_pre",
#             "rcancre_pre", "rhibpe_pre", "rdiabe_pre", "rpsyche_pre", "rarthre_pre")
# racute <- c("rhrtatt_pre",  "rangin_pre", "rconhrtf_pre")
# 
# rhs <- paste0(c("safemale_pre", 
#                 "sagey_e_pre",
#                 "semployed_pre",
#                 # "sacollege_pre",
#                 # "sahsdropout_pre",
#                 "sanonwhite_pre",
#                 "hchild_pre",
#                 "hspouseonly_pre",
#                 # "rafemale_pre",
#                 # "racollege_pre",
#                 # "ranonwhite_pre",
#                 "ragey_e_pre"), collapse = " + ")
# 
# rhs <- paste0(c(rhs, "rmemrye_i_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rcogfunction_i_pre"), collapse = " + ")
# 
# rhs <- paste0(c(rhs, rdiags), collapse = " + ")
# # rhs <- paste0(c(rhs, "rdiagcount_pre"), collapse = " + ")
# # rhs <- paste0(c(rhs, racute), collapse = " + ")
# rhs <- paste0(c(rhs, "rheartprb_pre"), collapse = " + ")
# 
# # rhs <- paste0(c(rhs, "radlcount_grp_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "radlcount_pre"), collapse = " + ")
# 
# # rhs <- paste0(c(rhs, "rhosp_e_pre"), collapse = " + ")
# # rhs <- paste0(c(rhs, "rhomecare_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rgovmd_pre", "rhiltc_pre"), collapse = " + ")
# # rhs <- paste0(c(rhs, "rinsur_pre"), collapse = " + ")
# 
# # rhs <- paste0(c(rhs,
# #                 "hatoth_defl_qtile_pre",
# #                 "hatotf_defl_qtile_pre",
# #                 "hitot_defl.oecdeq_pre"), collapse = " + ")
# 
# rhs <- paste0(c(rhs, "rrscarehpw_grp_pre"), collapse = " + ")
# # rhs <- paste0(c(rhs, "rrscarehpw_i_pre", "rrscarehpw_i.sq_pre"), collapse = " + ")
# 
# # rhs <- paste0(c(rhs, "rrscaredur_grp_pre"), collapse = " + ")
# # rhs <- paste0(c(rhs, "rrscaredur1_pre"), collapse = " + ")
# rhs <- paste0(c(rhs, "rrscaredur2_pre"), collapse = " + ")
# 
# print(regspec <- formula(paste0(" ~ ", rhs)))
# 
# rm(rhs)
# ####################################

# ATT <- list()

# ### ESTIMATE PRE-TREATMENT EFFECTS
# ####################################
# rm(attg_pre)
# summary(attg_pre  <- att_gt(yname = "r.cesd",
#                             tname = "wave",
#                             idname = "e.hhidpn",
#                             gname = "treatwave",
#                             xformla = regspec,
#                             est_method = "reg",
#                             base_period = "universal",
#                             control_group = "nevertreated",
#                             # anticipation = 2,
#                             panel = TRUE,
#                             allow_unbalanced_panel = TRUE,
#                             bstrap = TRUE,
#                             cband = FALSE, alp = 0.05,
#                             clustervars = "e.hhidpn",
#                             weightsname = "weights",
#                             data = dat %>%
#                                   filter(et>=test_min)))
# ####################################

# ESTIMATE TREATMENT EFFECTS AT EACH EVENT TIME
####################################
### Estimate ATT-G
rm(attg, attet)
attg  <- att_gt(yname = "r.cesd",
                tname = "wave",
                idname = "e.hhidpn",
                gname = "treatwave",
                xformla = regspec,
                est_method = "reg",
                base_period = "universal",
                control_group = "nevertreated",
                # anticipation = 1,
                panel = TRUE,
                allow_unbalanced_panel = TRUE,
                bstrap = TRUE,
                cband = TRUE, alp = 0.05,
                clustervars = "e.hhidpn",
                weightsname = "weights",
                data = dat)

### Aggregate to obtain ATT at each event time
# With 95% pointwise confidence intervals for each estimate
attet <- aggte(attg, type = "dynamic", 
               min_e = window_min,
               max_e = window_max,
               balance_e = window_max,
               na.rm=TRUE, bstrap = TRUE, cband = FALSE, clustervars = "e.hhidpn")  

# summary(attet)
rm(regspec)
####################################


# ### COMPILE ESTIMATES
# ####################################
# rm(est.attet.cesd)
# est.attet.cesd <- bind_cols(et=attet$egt, 
#                             att = attet$att.egt,
#                             se=attet$se.egt,
#                             ci.lo = attet$att.egt - attet$crit.val.egt*attet$se.egt,
#                             ci.hi = attet$att.egt + attet$crit.val.egt*attet$se.egt)
# 
# 
# est.attet.cesd <- est.attet.cesd %>%
#   mutate(depvar_pre = mean(attet$DIDparams$data[attet$DIDparams$data$wave == attet$DIDparams$data$treatwave-1,]$r.cesd),
#          relchg = attet$att.egt/depvar_pre,
#          Treated = attet$DIDparams$n/2,
#          Observations = nrow(attet$DIDparams$data))
# 
# est.attet.cesd <- est.attet.cesd %>%
#   mutate(Wald = as.numeric(attg_pre$W)) %>%
#   mutate(pval = as.numeric(attg_pre$Wpval))
# 
# ####################################
