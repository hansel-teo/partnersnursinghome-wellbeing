# Estimation sample
load(paste0(dir_working, "03preanalysis/samp_event-20230410.rda"))


rdemographics_n <- c("safemale", "sanonwhite", "sacollege", "sagey_e", "semployed")
rdemographics_l <- c("Is female", "Is non-white", "Has college education", "Age (years)", "Is employed")

crdemographics_n <- c("rafemale", "ranonwhite", "racollege", "ragey_e")
crdemographics_l <- c("Is female", "Is non-white", "Has college education", "Age (years)")

crconditions_n <- c("radlcount", 
                    # "rwalkra", "rdressa", "rbatha", "reata", "rbeda", "rtoilta",
                    # "rwalk1a", "rclim1a",
                    "rmemrye_i", 
                    "rhibpe", "rdiabe", "rcancre", "rlunge", 
                    "rhearte", "rstroke", "rpsyche", "rarthre",
                    "rhrtatt", "rangin", "rconhrtf")

crconditions_l <- c("Number of difficulties with ADLs",
                    # "Difficulty walking across room", "Difficulty getting dressed", "Difficulty bathing/shower", "Difficulty eating", "Difficulty getting in/out of bed", "Difficulty using toilet",
                    # "Difficulty walking one block", "Difficulty climbing one flight of stairs",
                    "Ever diagnosed with Alzheimer's/Dementia", 
                    "Ever diagnosed with high blood pressure", "Ever diagnosed with diabetes", "Ever diagnosed with cancer", "Ever diagnosed with lung disease", 
                    "Ever diagnosed with heart problems", "Ever had a stroke", "Ever had psychiatric problems", "Ever had arthritis",
                    "Had heart attack since last wave", "Had angina since last wave", "Had congestive heart failure since last wave")

carerecv_n <- c("rrscare_i", "rrscarehpw_i",
                "rrnscare_i",
                # "rrccare_i", 
                # "rrrfcare_i",
                # "rrfrcare_i",
                "rhomcar_e",
                "rhosp_e")
carerecv_l <- c("Received informal care from partner", 
                "Hours of care per week from partner",
                "Received informal care from other sources", 
                # "Receives informal care from (grand)children", 
                # "Receives informal care from relatives or friends", 
                # "Receives formal care",
                "Received home health care",
                "Had a hospital stay")


financial_n <- c("hspouseonly", "hhhres", "hchild",
                 "haohous",
                 "hatotf_defl", "hatoth_defl", "hitot_defl.oecdeq",
                 "rhiltc", "rgovmd")
financial_l <- c("Couple only household",
                 "Number of household residents",
                 "Number of living children",
                 "Owns home",
                 "Couple's non-housing financial wealth (2012 USD)", 
                 "Net value of home (2012 USD)", 
                 "Per-person household income (2012 USD)",
                 "Care recipient has private LTC insurance", 
                 "Care recipient has Medicaid coverage")

vars <- bind_cols(
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>%
    select(paste0(c(rdemographics_n), "_pre")),
  model.matrix(~ -1 + sshlt_i_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE)) %>%
    as.data.frame() %>%
    select(ends_with(sort(levels(dat_event$sshlt_i_pre))))) %>%
  select(starts_with(paste0(c(rdemographics_n,
                              "sshlt_i"), "_pre"))) %>%
  summarise(across(.cols = everything(), .fns = ~mean(.))) %>%
  pivot_longer(cols = everything(), values_to = "All treated")

vars <- bind_rows(vars,
                  bind_cols(
                    dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>%
                      select(paste0(c(crdemographics_n), "_pre")),
                    model.matrix(~ -1 + rshlt_i_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE)) %>%
                      as.data.frame() %>%
                      select(ends_with(sort(levels(dat_event$rshlt_i_pre))))) %>%
                    select(starts_with(paste0(c(crdemographics_n,
                                                "rshlt_i"), "_pre"))) %>%
                    summarise(across(.cols = everything(), .fns = ~mean(.)))
                  
                  %>%
                    pivot_longer(cols = everything(), values_to = "All treated"))
  
vars <- bind_rows(vars,
                  bind_cols(
                    dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>%
                      select(paste0(c(crconditions_n[1]), "_pre")),
                    model.matrix(~ -1 + rcogfunction_i_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE)) %>%
                      as.data.frame() %>%
                      select(ends_with(sort(levels(dat_event$rcogfunction_i_pre)))),
                    dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>%
                      select(paste0(c(crconditions_n[10:length(crconditions_n)]), "_pre"))) %>%
                    select(starts_with(paste0(c(crconditions_n[1],
                                                "rcogfunction_i",
                                                crconditions_n[10:length(crconditions_n)]), "_pre"))) %>%
                    summarise(across(.cols = everything(), .fns = ~mean(.))) %>%
                    pivot_longer(cols = everything(), values_to = "All treated"))

vars <- bind_rows(vars,
                  bind_cols(
                    dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>%
                      select(paste0(c(carerecv_n), "_pre")),
                    model.matrix(~ -1 + rcenreg_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE)) %>%
                      as.data.frame() %>%
                      select(ends_with(sort(levels(dat_event$rcenreg_pre)))),
                    dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>%
                      select(paste0(c(financial_n), "_pre"))) %>%
                    select(starts_with(paste0(c(carerecv_n,
                                                financial_n,
                                                "rcenreg"), "_pre"))) %>%
                    summarise(across(.cols = everything(), .fns = ~mean(.))) %>%
                    pivot_longer(cols = everything(), values_to = "All treated"))

vars <- bind_cols(
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% 
    select(paste0(c(rdemographics_n), "_pre")),
  model.matrix(~ -1 + sshlt_i_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE)) %>%
    as.data.frame() %>%
    select(ends_with(sort(levels(dat_event$sshlt_i_pre)))),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>%
    select(paste0(c(crdemographics_n), "_pre")),
  model.matrix(~ -1 + rshlt_i_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE)) %>%
    as.data.frame() %>%
    select(ends_with(sort(levels(dat_event$rshlt_i_pre)))),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>%
    select(paste0(c(crconditions_n[1]), "_pre")),
  model.matrix(~ -1 + rcogfunction_i_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE)) %>%
    as.data.frame() %>%
    select(ends_with(sort(levels(dat_event$rcogfunction_i_pre)))),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>%
    select(paste0(c(crconditions_n[2:length(crconditions_n)]), "_pre")),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>%
    select(paste0(c(carerecv_n), "_pre")),
  model.matrix(~ -1 + rcenreg_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE)) %>%
    as.data.frame() %>%
    select(ends_with(sort(levels(dat_event$rcenreg_pre)))),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>%
    select(paste0(c(financial_n), "_pre"))) %>%
  select(starts_with(paste0(c(rdemographics_n,
                              "sshlt_i",
                              crdemographics_n,
                              "rshlt_i",
                              crconditions_n[1],
                              "rcogfunction_i",
                              crconditions_n[2:length(crconditions_n)],
                              carerecv_n,
                              financial_n,
                              "rcenreg"), "_pre"))) 


vars0 <- bind_cols(
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==0) %>%
    select(paste0(c(rdemographics_n), "_pre")),
  model.matrix(~ -1 + sshlt_i_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==0)) %>%
    as.data.frame() %>%
    select(ends_with(sort(levels(dat_event$sshlt_i_pre)))),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==0) %>%
    select(paste0(c(crdemographics_n), "_pre")),
  model.matrix(~ -1 + rshlt_i_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==0)) %>%
    as.data.frame() %>%
    select(ends_with(sort(levels(dat_event$rshlt_i_pre)))),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==0) %>%
    select(paste0(c(crconditions_n[1]), "_pre")),
  model.matrix(~ -1 + rcogfunction_i_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==0)) %>%
    as.data.frame() %>%
    select(ends_with(sort(levels(dat_event$rcogfunction_i_pre)))),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==0) %>%
    select(paste0(c(crconditions_n[2:length(crconditions_n)]), "_pre")),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==0) %>%
    select(paste0(c(carerecv_n), "_pre")),
  model.matrix(~ -1 + rcenreg_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==0)) %>%
    as.data.frame() %>%
    select(ends_with(sort(levels(dat_event$rcenreg_pre)))),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==0) %>%
    select(paste0(c(financial_n), "_pre"))) %>%
  select(starts_with(paste0(c(rdemographics_n,
                              "sshlt_i",
                              crdemographics_n,
                              "rshlt_i",
                              crconditions_n[1],
                              "rcogfunction_i",
                              crconditions_n[2:length(crconditions_n)],
                              carerecv_n,
                              financial_n,
                              "rcenreg"), "_pre")))


vars1 <- bind_cols(
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==1) %>%
    select(paste0(c(rdemographics_n), "_pre")),
  model.matrix(~ -1 + sshlt_i_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==1)) %>%
    as.data.frame() %>%
    select(ends_with(sort(levels(dat_event$sshlt_i_pre)))),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==1) %>%
    select(paste0(c(crdemographics_n), "_pre")),
  model.matrix(~ -1 + rshlt_i_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==1)) %>%
    as.data.frame() %>%
    select(ends_with(sort(levels(dat_event$rshlt_i_pre)))),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==1) %>%
    select(paste0(c(crconditions_n[1]), "_pre")),
  model.matrix(~ -1 + rcogfunction_i_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==1)) %>%
    as.data.frame() %>%
    select(ends_with(sort(levels(dat_event$rcogfunction_i_pre)))),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==1) %>%
    select(paste0(c(crconditions_n[2:length(crconditions_n)]), "_pre")),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==1) %>%
    select(paste0(c(carerecv_n), "_pre")),
  model.matrix(~ -1 + rcenreg_pre, dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==1)) %>%
    as.data.frame() %>%
    select(ends_with(sort(levels(dat_event$rcenreg_pre)))),
  dat_event %>% filter(wave == t0-1) %>% filter(treated==TRUE) %>% filter(rrscare_i_pre==1) %>%
    select(paste0(c(financial_n), "_pre"))) %>%
  select(starts_with(paste0(c(rdemographics_n,
                              "sshlt_i",
                              crdemographics_n,
                              "rshlt_i",
                              crconditions_n[1:9],
                              "rcogfunction_i",
                              crconditions_n[10:length(crconditions_n)],
                              carerecv_n,
                              financial_n,
                              "rcenreg"), "_pre")))


desc <- vars %>%
  summarise(across(.cols = everything(), .fns = ~mean(.))) %>%
  pivot_longer(cols = everything(), values_to = "All treated") %>%
  left_join(vars0  %>%
              summarise(across(.cols = everything(), .fns = ~mean(.))) %>%
              pivot_longer(cols = everything(), values_to = "Non-caregivers"), by = "name") %>%
  left_join(vars1  %>%
              summarise(across(.cols = everything(), .fns = ~mean(.))) %>%
              pivot_longer(cols = everything(), values_to = "Caregivers"), by = "name")


print(desc, n=100)

desc <- bind_cols(desc, 
                  "t-stat" = rbind(bind_rows(lapply(colnames(vars[1:41]), function(x){t.test(vars0[, x], vars1[, x])$statistic})),
                                   NA,
                                   bind_rows(lapply(colnames(vars[43:length(vars)]), function(x){t.test(vars0[, x], vars1[, x])$statistic}))))

desc <- bind_cols(Variable = c(rdemographics_l,
                               c("","","",
                                 "Self-reported health: Excellent", 
                                 "Self-reported health: Very good",
                                 "Self-reported health: Good",
                                 "Self-reported health: Fair",
                                 "Self-reported health: Poor"),
                               crdemographics_l,
                               c("","","",
                                 "Self-reported health: Excellent", 
                                 "Self-reported health: Very good",
                                 "Self-reported health: Good",
                                 "Self-reported health: Fair",
                                 "Self-reported health: Poor"),
                               crconditions_l[1],
                               c("Cognitive function: Normal", 
                                 "Cognitive function: Impaired", 
                                 "Cognitive function: Severely impaired"),
                               crconditions_l[2:length(crconditions_l)],
                               carerecv_l,
                               financial_l,
                               c("",
                                 "Census region: Northeast", 
                                 "Census region: Midwest", 
                                 "Census region: South", 
                                 "Census region: West", "")), desc)


desc <- desc %>% filter(Variable!="")
desc <- bind_rows(desc, bind_cols(Variable = "Number of individuals", 
                                  name = "", 
                                  `All treated` = nrow(dat_event[dat_event$wave==dat_event$t0-1 & dat_event$treated==TRUE,]),
                                  `Non-caregivers` = nrow(dat_event[dat_event$wave==dat_event$t0-1 & dat_event$treated==TRUE & dat_event$rrscare_i_pre==0,]),
                                  `Caregivers` = nrow(dat_event[dat_event$wave==dat_event$t0-1 & dat_event$treated==TRUE & dat_event$rrscare_i_pre==1,])))


tab <- bind_cols(left_join(dat_event %>% 
                             filter(wave==t0-1 & treated==TRUE) %>%
                             summarise(`Respondent's CES-D: Mean` = mean(r.cesd), 
                                       `Respondent's CES-D: 25th percentile` = quantile(r.cesd, prob = 0.25), 
                                       `Respondent's CES-D: Median` = median(r.cesd), 
                                       `Respondent's CES-D: 75th percentile` = quantile(r.cesd, prob = 0.75)) %>%
                             pivot_longer(cols = everything(), values_to = "All treated"),
                           dat_event %>% 
                             filter(wave==t0-1 & treated==TRUE & rrscare_i_pre==0) %>%
                             summarise(`Respondent's CES-D: Mean` = mean(r.cesd), 
                                       `Respondent's CES-D: 25th percentile` = quantile(r.cesd, prob = 0.25), 
                                       `Respondent's CES-D: Median` = median(r.cesd), 
                                       `Respondent's CES-D: 75th percentile` = quantile(r.cesd, prob = 0.75)) %>%
                             pivot_longer(cols = everything(), values_to = "Non-caregivers"),
                           by = "name") %>%
                   left_join(dat_event %>% 
                               filter(wave==t0-1 & treated==TRUE & rrscare_i_pre==1) %>%
                               summarise(`Respondent's CES-D: Mean` = mean(r.cesd), 
                                         `Respondent's CES-D: 25th percentile` = quantile(r.cesd, prob = 0.25), 
                                         `Respondent's CES-D: Median` = median(r.cesd), 
                                         `Respondent's CES-D: 75th percentile` = quantile(r.cesd, prob = 0.75)) %>%
                               pivot_longer(cols = everything(), values_to = "Caregivers"), 
                             by = "name"),
                 `t-stat` = rbind(t.test(dat_event[dat_event$wave==dat_event$t0-1 & dat_event$treated==TRUE & dat_event$rrscare_i_pre==0, ]$r.cesd, 
                                         dat_event[dat_event$wave==dat_event$t0-1 & dat_event$treated==TRUE & dat_event$rrscare_i_pre==1, ]$r.cesd)$statistic,
                                  NA, NA, NA)) %>%
  as.data.frame() 



desc <- bind_rows(tab %>% rename(Variable=name),
                  desc %>% rename(`t-stat`=t) %>% select(-name))

desc <- desc %>% 
  mutate(across(.cols = -c(Variable), .fns = ~signif(.,4)))

rm(vars, vars0, vars1)
rm(tab)

rm(carerecv_l, carerecv_n, crconditions_l, crconditions_n, financial_l, financial_n, rdemographics_l, rdemographics_n, crdemographics_l, crdemographics_n)


write.csv(desc, 
          row.names = FALSE, 
          paste0(dir_working, "05results/tables/tab1-20230411.csv"))

