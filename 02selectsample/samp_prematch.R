
samp_pre <- bind_rows(select_treated %>% arrange(wave, cr.hhidpn),
                      select_nevtreat %>% arrange(wave, cr.hhidpn) %>% select(colnames(select_treated)))

samp_pre <- samp_pre %>% mutate(treated = (treatwave<999))


# Join with covariate data
######
samp_pre <- left_join(samp_pre,
                      dat_hrs %>%
                        select(hhidpn, wave, all_of(rvars)),
                      by = c("cr.hhidpn"="hhidpn", "wave"="wave"))

samp_pre %>% 
  group_by(treated) %>% 
  summarise(n(), n_distinct(cr.hhidpn))

svars <- c(sdemographics, shlth)
samp_pre <- left_join(samp_pre,
                      dat_hrs %>%
                        select(hhidpn, wave, str_replace(svars, "^s", "r")) %>%
                        rename_with(.cols = -c(hhidpn, wave),
                                    .fn = ~str_replace(., "^r", "s")),
                      by = c("r.hhidpn" = "hhidpn", "wave" = "wave"))


# svars <- str_replace(svars, "^s", "r.")
samp_pre %>% select(all_of(rvars), all_of(svars)) %>% summary()

samp_pre <- samp_pre %>% 
  mutate(rcogtot27_imp = if_else(is.na(rcogtot27_imp) & !is.na(rprxyscore_imp),
                                 as.integer(-3), as.integer(rcogtot27_imp))) %>% 
  mutate(rprxyscore_imp = if_else(!is.na(rcogtot27_imp) & is.na(rprxyscore_imp),
                                  as.integer(-3), as.integer(rprxyscore_imp)))

samp_pre <- samp_pre %>% 
  mutate(rcogproxy = if_else(!is.na(rprxyscore_imp), as.integer(1),
                             if_else(is.na(rprxyscore_imp) & is.na(rcogtot27_imp), as.integer(NA), 
                                     as.integer(0))))

rcog <- c(rcog, "rcogproxy")
samp_pre %>% select(all_of(rcog)) %>% summary()
######


samp_pre %>% select(wave, treated, rrscare_i) %>% table(useNA="ifany")

samp_pre %>%
  group_by(wave) %>%
  summarise(n(), T = sum(treated==TRUE), C = sum(treated==FALSE), T/C)

# REMARK:
# Small number and low rate of treated vs. non-treated  for treatment wave 14?


samp_pre %>% 
  filter(if_all(.cols = c(all_of(rdemographics),
                          all_of(rins),
                          all_of(rcarerecv),
                          all_of(radls[-7]),
                          all_of(rcog),
                          all_of(rdiag),
                          all_of(racute),
                          all_of(rhlth), all_of(shlth),
                          all_of(svars),
                          all_of(hdemographics),
                          all_of(hfinancial)),
                .fn = ~!is.na(.))) %>%
  group_by(treated) %>% 
  summarise(n(), n_distinct(cr.hhidpn))

# samp_pre <- samp_pre %>% 
#   filter(if_all(.cols = c(all_of(rvars), all_of(svars)), .fn = ~!is.na(.)))
samp_pre <- samp_pre %>% 
  filter(if_all(.cols = c(all_of(rvars[-c(36)]), all_of(svars)), .fn = ~!is.na(.)))

samp_pre %>% 
  group_by(treated) %>% 
  summarise(n(), n_distinct(cr.hhidpn))

samp_pre <- samp_pre %>% filter(!(rrscare_i==1 & rrscarehpw_i==0))

samp_pre %>% 
  group_by(treated) %>% 
  summarise(n(), n_distinct(cr.hhidpn))

samp_pre %>% 
  filter(rrscare_i==1) %>%
  group_by(treated, rrscarehpw_i==168) %>% 
  summarise(n(),
            min = min(rrscarehpw_i), 
            avg = mean(rrscarehpw_i), 
            q50 = median(rrscarehpw_i), 
            q75 = quantile(rrscarehpw_i, 0.75),
            max = max(rrscarehpw_i))

samp_pre <- samp_pre %>% 
  mutate(rrscarehpw_grp = cut(rrscarehpw_i, breaks = c(0,1,29,Inf), 
                              include.lowest = TRUE, right = FALSE,
                              labels = FALSE)) %>%
  mutate(rrscarehpw_grp = factor(rrscarehpw_grp))


samp_pre <- samp_pre %>% 
  mutate(rrscarehpw_grp2 = cut(rrscarehpw_i, breaks = c(0,1,22,167,Inf), 
                               include.lowest = TRUE, right = FALSE,
                               labels = FALSE)) %>%
  mutate(rrscarehpw_grp2 = factor(rrscarehpw_grp2))

samp_pre %>% select(treated, rrscarehpw_grp) %>% table(useNA="ifany")
samp_pre %>% select(treated, rrscarehpw_grp2) %>% table(useNA="ifany")

# Define ADL count
# samp_pre$radlcount <- rowSums(samp_pre[, c("rwalkra", "rdressa", "rbatha", "reata", "rbeda", "rtoilta", "rwalk1a", "rclim1a")], na.rm=TRUE)
samp_pre$radlcount <- rowSums(samp_pre[, c("rwalkra", "rdressa", "rbatha", "reata", "rbeda", "rtoilta", "rclim1a")], na.rm=TRUE)

samp_pre %>% select(radlcount) %>% table()
samp_pre <- samp_pre %>% 
  mutate(radlcount_grp = cut(radlcount, breaks = c(0,1,2,Inf), include.lowest = TRUE, right = FALSE,
                             labels = FALSE)) %>%
  mutate(radlcount_grp = factor(radlcount_grp))

samp_pre %>% select(radlcount_grp) %>% table(useNA="always")
samp_pre %>% select(radlcount, radlcount_grp) %>% table(useNA="always")

  
samp_pre <- samp_pre %>%  mutate(radlcount2 = (radlcount>1))
samp_pre <- samp_pre %>%  mutate(radlcount1 = (radlcount>0))


# Define diagnosed health conditions count
samp_pre$rdiagcount <- rowSums(samp_pre[, c("rhearte", "rlunge", "rstroke", "rcancre", "rhibpe", "rdiabe", "rpsyche", "rarthre")], na.rm=TRUE)

samp_pre %>% select(rdiagcount) %>% summary()
samp_pre <- samp_pre %>% 
  mutate(rdiagcount_grp = cut(rdiagcount, breaks = c(0,1,2,3,Inf), include.lowest = TRUE, right = FALSE,
                              labels = FALSE)) %>%
  mutate(rdiagcount_grp = factor(rdiagcount_grp))

samp_pre %>% select(rdiagcount_grp) %>% table(useNA="always")
samp_pre %>% select(rdiagcount, rdiagcount_grp) %>% table(useNA="always")

colnames(samp_pre)

