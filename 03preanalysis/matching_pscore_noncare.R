# Define estimation subsample
samp_noncare <- samp_pre %>% 
  filter(rrscare_i ==0)


# PROPENSITY SCORE MODEL FOR NON-CAREGIVERS
##################
rhs <- "factor(wave) + rcenreg"
# rhs <- paste0(rhs, " + ", paste0("rafemale*", c("ragey_e"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0("rafemale*", c("ragey_e", "ragey.sq"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0("rafemale*", c("ragey_grp"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0("rafemale*", c("ranonwhite", "raeduc"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0("rafemale*", c("racohbyr"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0("rafemale*", c("hchild_grp"), collapse = " + "))

rhs <- paste0(rhs, " + ",
              paste0("rafemale*", c("rwalkra", "rdressa", "rbatha", "reata", 
                                    "rbeda", "rtoilta", "rclim1a"), collapse = " + "))
rhs <- paste0(rhs, " + ",
              paste0("rafemale*", c("rhearte", "rlunge", "rstroke", "rcancre", 
                                    "rhibpe", "rdiabe", "rpsyche", "rarthre"), collapse = " + "))
# rhs <- paste0(rhs, " + ",
#               paste0("rafemale*", c("rhrtatt", "rangin", "rconhrtf"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0("rafemale*", c("rheartprb"), collapse = " + "))

rhs <- paste0(rhs, " + ", paste0("rafemale*", c("rcogfunction_i", "rmemrye_i"), collapse = " + "))

# rhs <- paste0(rhs, " + ", paste0("rafemale*", c("rshlt_grp"), collapse = " + "))

rhs <- paste0(rhs, " + ", paste0("safemale*", c("sagey_e"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0("safemale*", c("sagey_e", "sagey.sq"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0("safemale*", c("sagey_grp"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0("safemale*", c("sanonwhite", "saeduc"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0("safemale*", c("semployed"), collapse = " + "))

# rhs <- paste0(rhs, " + ", paste0("safemale*", c("sshlt_grp"), collapse = " + "))

rhs <- paste0(rhs, " + ", paste0(c("rgovmd"), collapse = " + "))

rhs <- paste0(rhs, " + ", paste0(c("factor(hatoth_defl_qtile)", 
                                   "factor(hatotn_defl_qtile)"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0(c("hitot_defl.oecdeq"), collapse = " + "))

rhs <- paste0(rhs, " + ", paste0(c("rrnscare_i"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0(c("rrfrcare_i"), collapse = " + "))

rhs <- paste0(rhs, " + ", paste0(c("rhomcar_e"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0(c("ragey_grp*rhomcar_e"), collapse = " + "))

# rhs <- paste0(rhs, " + ", paste0(c("rhosp_e"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0(c("ragey_grp*rhosp_e"), collapse = " + "))

modspec0 <- paste0("treated ~ ", rhs)

rm(rhs)
##################



# ESTIMATE PROPENSITY SCORE MODEL
######
gc()
summary(nrshom.mod0 <- glm(formula(modspec0), family = "binomial", data = samp_noncare))

######



# DEFINE TRIMMED PSCORES
######
vars_pscore_nrshom0 <- bind_cols(samp_noncare %>% select(cr.hhidpn, wave), 
                                 pscore.link =predict(nrshom.mod0, type = "link",
                                                      newdata = samp_noncare),
                                 pscore.prob =predict(nrshom.mod0, type = "response",
                                                      newdata = samp_noncare))

vars_pscore_nrshom0 %>% select(pscore.prob) %>% summary()

pscores.trim <- left_join(samp_noncare %>% 
                            select(cr.hhidpn, wave, treated),
                          vars_pscore_nrshom0, by = c("cr.hhidpn", "wave")) %>%
  filter(!is.na(pscore.prob))

pscores.trim %>% 
  group_by(wave, treated) %>% 
  summarise(n(), 
            min = min(pscore.prob),
            q25 = quantile(pscore.prob, 0.25),
            q50 = quantile(pscore.prob, 0.5),
            q75 = quantile(pscore.prob, 0.75),
            max = max(pscore.prob))

# Trim from below by wave to keep only p-scores at least equal to the min TREATED p-score.
# This ensures that all observations in the estimation sample have probability 
# of treatment (in terms of pscore) bounded away from zero
pscores.trim <-left_join(pscores.trim,
                         pscores.trim %>%
                           filter(treated==TRUE) %>%
                           group_by(wave) %>%
                           summarise(lb = min(pscore.prob)), by = "wave")

pscores.trim <- pscores.trim %>% 
  filter(pscore.prob < 1 & pscore.prob >= lb)

pscores.trim <- pscores.trim %>% 
  rename_with(.cols = c(pscore.link, pscore.prob),
              .fn = ~paste0(.,".trim_carestat"))

pscores.trim %>% group_by(treated) %>% 
  summarise(n(), max = max(pscore.prob.trim_carestat), min = min(pscore.prob.trim_carestat))

vars_pscore_nrshom0 <- left_join(vars_pscore_nrshom0,
                                 pscores.trim %>% select(-treated),
                                 by = c("cr.hhidpn", "wave"))

rm(pscores.trim)

save(vars_pscore_nrshom0, file = paste0(dir_working, "03preanalysis/temp/pscore_carestat_noncare.rda"))
######



# NEAREST NEIGHBOUR MATCHING
######
# load(paste0(dir_working, "03preanalysis/temp/PScore_carestat_noncare-20230406.rda"))

samp_prematch <- left_join(samp_noncare, vars_pscore_nrshom0, by = c("cr.hhidpn", "wave"))

# No caregiver candidates with treatwave 5
samp_prematch <- samp_prematch %>% filter(wave >4)

samp_prematch %>% select(treated) %>% table(useNA="always")

# samp_prematch %>% select(rrscaredur1, rrscaredur2, treated) %>% table(useNA="ifany")

samp_prematch %>%
  group_by(treated) %>% 
  summarise(n(), 
            pscore = sum(!is.na(pscore.link)), 
            trimmed = sum(!is.na(pscore.link.trim_carestat)))

samp_prematch <- samp_prematch %>% 
  mutate(pscore = pscore.link.trim_carestat)

samp_prematch <- samp_prematch %>% filter(!is.na(pscore))

samp_prematch %>% select(treated) %>% table(useNA="always")

# samp_prematch <- samp_prematch %>% mutate(radlcount2 = (radlcount>1))
# samp_prematch <- samp_prematch %>% mutate(radlcount1 = (radlcount>0))
match_exacts <- c("safemale",
                  # "rrnscare_i",
                  # "rdemented_i",
                  # "radlcount2",
                  # "rrscare.lag1",
                  # "rrscare.lag2",
                  "rmemrye_i")

# 1-to-1 NN-matching using pre-estimated p-scores
matchspec <- formula(paste0("treated ~ ", paste0(c("wave", match_exacts), collapse = " + ")))

match.NNpscore <- matchit(matchspec,
                          method = "nearest",
                          exact = c("wave", match_exacts),
                          replace = FALSE,
                          distance = samp_prematch$pscore,
                          data = samp_prematch)

# # Balance statistics
# summary(match.NNpscore, addlvariables = samp_prematch[, vars_health], un = FALSE)
# plot(summary(match.NNpscore, addlvariables = samp_prematch[, vars_health], un = FALSE), threshold = c(0.25, 0.1))
# 
# summary(match.NNpscore, addlvariables = samp_prematch[, vars_demog], un = FALSE)
# plot(summary(match.NNpscore, addlvariables = samp_prematch[, vars_demog], un = FALSE), threshold = c(0.25, 0.1))
# 
# summary(match.NNpscore, addlvariables = samp_prematch[, vars_care], un = FALSE)
# plot(summary(match.NNpscore, addlvariables = samp_prematch[, vars_care], un = FALSE), threshold = c(0.25, 0.1))

rm(matchspec)
######



### SAVE
match.NNpscore_noncare <- match.NNpscore
rm(match.NNpscore)

save(match.NNpscore_noncare, file = paste0(dir_working, "03preanalysis/temp/output_NNpscore_carestat_noncare.rda"))

# matchstats.NNpscore_noncare <- summary(match.NNpscore_noncare, addlvariables = samp_prematch[, c(vars_demog, vars_health, vars_care)], un = TRUE)
# save(matchstats.NNpscore_noncare, file = paste0(dir_working, "03preanalysis/output/matchstats_NNpscore_carestat_noncare-20230406.rda"))
# 
# rm(matchstats.NNpscore_noncare)


dat_matched0 <- match.data(match.NNpscore_noncare) %>% 
  mutate(t0 = wave + 1) %>%
  mutate(e.hhidpn = paste0(str_pad(as.character(t0), width = 2, side = "left", pad = "0"), cr.hhidpn)) %>%
  select(e.hhidpn, cr.hhidpn, treatwave, t0, treated, weights, subclass, wave) 

dat_matched0 <- dat_matched0 %>%
  mutate(e.hhidpn = as.numeric(e.hhidpn))

dat_matched0 <- dat_matched0 %>%
  mutate(subclass = paste0("0.", subclass))
###

rm(samp_prematch)
rm(samp_noncare)

rm(nrshom.mod0, modspec0)
rm(vars_pscore_nrshom0)
rm(match.NNpscore_noncare)