# Define estimation subsamples
samp_caregive <- samp_pre %>% 
  filter(rrscare_i ==1)

samp_caregive <- samp_caregive %>%
  mutate(rrscarehpw_qtile = cut(rrscarehpw_i, 
                                breaks = c(quantile(rrscarehpw_i, probs = seq(0,1,0.2)), 167, Inf),
                                labels = FALSE, include.lowest = TRUE, right = FALSE)) 

samp_caregive <- samp_caregive %>%
  mutate(rrscarehpw_qtile = factor(rrscarehpw_qtile))

samp_caregive %>% 
  select(rrscarehpw_qtile) %>% table(useNA="always")
samp_caregive %>%
  group_by(treated, rrscarehpw_qtile) %>%
  summarise(n(), mean(rrscarehpw_i))


# PROPENSITY SCORE MODEL FOR CAREGIVERS
##################
rhs <- "factor(wave) + rcenreg"
rhs <- paste0(rhs, " + ", paste0("rafemale*", c("ragey_e"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0("rafemale*", c("ragey_e", "ragey.sq"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0("rafemale*", c("ragey_grp"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0("rafemale*", c("ranonwhite", "racollege", "rahsdropout"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0("rafemale*", c("racohbyr"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0("rafemale*", c("hchild_grp"), collapse = " + "))

rhs <- paste0(rhs, " + ",
              paste0("rafemale*", c("rwalkra", "rdressa", "rbatha", "reata", 
                                    "rbeda", "rtoilta", "rclim1a"), collapse = " + "))
rhs <- paste0(rhs, " + ",
              paste0("rafemale*", c("rhearte", "rlunge", "rstroke", "rcancre", 
                                    "rhibpe", "rdiabe", "rpsyche", "rarthre"), collapse = " + "))
rhs <- paste0(rhs, " + ",
              paste0("rafemale*", c("rhrtatt", "rangin", "rconhrtf"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0("rafemale*", c("rheartprb"), collapse = " + "))

rhs <- paste0(rhs, " + ", paste0("rafemale*", c("rcogfunction_i", "rmemrye_i"), collapse = " + "))

# rhs <- paste0(rhs, " + ", paste0("rafemale*", c("rshlt_grp"), collapse = " + "))

rhs <- paste0(rhs, " + ", paste0("safemale*", c("sagey_e"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0("safemale*", c("sagey_e", "sagey.sq"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0("safemale*", c("sagey_grp"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0("safemale*", c("sanonwhite", "sacollege", "sahsdropout"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0("safemale*", c("semployed"), collapse = " + "))

rhs <- paste0(rhs, " + ", paste0("safemale*", c("sshlt_grp"), collapse = " + "))

# rhs <- paste0(rhs, " + ", paste0(c("rgovmd", "rhiltc"), collapse = " + "))

rhs <- paste0(rhs, " + ", paste0(c("factor(hatoth_defl_qtile)", 
                                   "factor(hatotn_defl_qtile)"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0(c("hitot_defl.oecdeq"), collapse = " + "))

# rhs <- paste0(rhs, " + ", paste0(c("rrnscare_i"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0(c("rrfrcare_i", "rhomcar_e"), collapse = " + "))

rhs <- paste0(rhs, " + ", paste0("safemale*", c("rrscare.lag1"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0("safemale*", c("rrscare.lag2"), collapse = " + "))

# rhs <- paste0(rhs, " + ", paste0("safemale*", c("rrscarehpw_i", "rrscarehpw_i.sq"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0("safemale*", c("rrscarehpw_qtile"), collapse = " + "))


rhs <- paste0(rhs, " + ", paste0(c("rcogfunction_i:rmemrye_i"), collapse = " + "))
# rhs <- paste0(rhs, " + ", paste0(c("rcogfunction_i:radlcount_grp"), collapse = " + "))
rhs <- paste0(rhs, " + ", paste0(c("radlcount_grp:sshlt_grp"), collapse = " + "))

# rhs <- paste0(rhs1, " + ",
#                paste0("rafemale*rcendiv*", c("hatoth_defl_qtile"), collapse = " + "))

modspec1 <- paste0("treated ~ ", rhs)

rm(rhs)
##################



# ESTIMATE PROPENSITY SCORE MODEL
######
gc()
summary(nrshom.mod1 <- glm(formula(modspec1), family = "binomial", data = samp_caregive))

######



# DEFINE TRIMMED PSCORES
######
vars_pscore_nrshom1 <- bind_cols(samp_caregive %>% select(cr.hhidpn, wave), 
                                 pscore.link =predict(nrshom.mod1, type = "link",
                                                      newdata = samp_caregive),
                                 pscore.prob =predict(nrshom.mod1, type = "response",
                                                      newdata = samp_caregive))

vars_pscore_nrshom1 %>% select(pscore.prob) %>% summary()

pscores.trim <- left_join(samp_caregive %>% select(cr.hhidpn, wave, treated),
                          vars_pscore_nrshom1, 
                          by = c("cr.hhidpn", "wave")) %>%
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

pscores.trim %>% 
  group_by(wave,treated) %>% 
  summarise(n(), max = max(pscore.prob.trim_carestat), min = min(pscore.prob.trim_carestat))

vars_pscore_nrshom1 <- left_join(vars_pscore_nrshom1,
                                 pscores.trim %>% select(-treated),
                                 by = c("cr.hhidpn", "wave"))

rm(pscores.trim)

save(vars_pscore_nrshom1, file = paste0(dir_working, "03preanalysis/temp/pscore_carestat_caregive.rda"))
######



# NEAREST NEIGHBOUR MATCHING
######
# load(paste0(dir_working, "03preanalysis/temp/PScore_carestat_caregive-20230406.rda"))

samp_prematch <- left_join(samp_caregive, vars_pscore_nrshom1, by = c("cr.hhidpn", "wave"))

# No caregiver candidates with treatwave 5
samp_prematch <- samp_prematch %>% filter(wave >4)

samp_prematch %>% select(treated) %>% table(useNA="always")

samp_prematch %>% select(rrscaredur1, treated) %>% table(useNA="ifany")
samp_prematch %>% select(rrscaredur2, treated) %>% table(useNA="ifany")

# samp_prematch <- samp_prematch %>% filter(!is.na(rrscaredur1))
# samp_prematch <- samp_prematch %>% filter(!is.na(rrscaredur2))
# samp_prematch %>% select(rrscarehpw_grp, rrscaredur_grp, treated) %>% table(useNA="ifany")

samp_prematch %>%
  group_by(treated) %>% 
  summarise(n(), 
            pscore = sum(!is.na(pscore.link)), 
            trimmed = sum(!is.na(pscore.link.trim_carestat)))

# Keep only candidates with trimmed p-scores
samp_prematch <- samp_prematch %>% 
  mutate(pscore = pscore.link.trim_carestat)

samp_prematch <- samp_prematch %>% filter(!is.na(pscore))

samp_prematch %>% select(treated) %>% table(useNA="always")

samp_prematch <- samp_prematch %>% mutate(radlcount2 = (radlcount>1))
samp_prematch <- samp_prematch %>% mutate(radlcount1 = (radlcount>0))

match_exacts <- c("safemale",
                  # "rimpaired_i",
                  "radlcount1",
                  # "rrscare.lag1",
                  # "rrscare.lag2",
                  "rrscaredur2",
                  "rmemrye_i")

# 1-to-1 NN-matching using pre-estimated p-scores
matchspec <- formula(paste0("treated ~ ", paste0(c("wave", match_exacts), collapse = " + ")))

require(MatchIt)
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
# 

rm(matchspec)
######



### SAVE
match.NNpscore_caregive <- match.NNpscore
rm(match.NNpscore)

save(match.NNpscore_caregive, file = paste0(dir_working, "03preanalysis/temp/output_NNpscore_carestat_caregive.rda"))

# rm(matchstats.NNpscore_caregive)


dat_matched1 <- match.data(match.NNpscore_caregive) %>% 
  mutate(t0 = wave + 1) %>%
  mutate(e.hhidpn = paste0(str_pad(as.character(t0), width = 2, side = "left", pad = "0"), cr.hhidpn)) %>%
  select(e.hhidpn, cr.hhidpn, treatwave, t0, treated, weights, subclass, wave) 

dat_matched1 <- dat_matched1 %>%
  mutate(e.hhidpn = as.numeric(e.hhidpn))

dat_matched1 <- dat_matched1 %>%
  mutate(subclass = paste0("1.", subclass))
###

rm(samp_prematch)
rm(samp_caregive)


rm(nrshom.mod1, modspec1)
rm(vars_pscore_nrshom1)
rm(match.NNpscore_caregive)





