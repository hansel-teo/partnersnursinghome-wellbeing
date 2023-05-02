
reqvars <- c("rcesd", "shhidpn", "rmstat", "rrscare_i")

lgrid <- left_join(dat_index_bal,
                   dat_hrs %>% 
                     select(hhidpn, wave, all_of(reqvars)),
                   by = c("hhidpn", "wave"))

lgrid %>%
  group_by(hhidpn) %>%
  mutate(nwaves = n_distinct(wave)) %>%
  ungroup() %>% select(nwaves) %>% table()

I <- max(unique(lgrid$wave))

### DEFINE LEADS AND LAGS OF VARIABLES
###############
gc()
for(j in 1:(I-1)) {
  lgrid <- lgrid %>%
    arrange(hhidpn, wave) %>%
    group_by(hhidpn) %>%
    mutate(across(.cols = c(riwstat, all_of(reqvars)),
                  .fns = ~lead(., j),
                  .names = paste0("{.col}.lead", j))) %>%
    ungroup()
}
gc()

gc()
for(j in 0:(I-1)) {
  lgrid <- lgrid %>%
    arrange(hhidpn, wave) %>%
    group_by(hhidpn) %>%
    mutate(across(.cols = c(riwstat, all_of(reqvars)),
                  .fns = ~lag(., j),
                  .names = paste0("{.col}.lag", j))) %>%
    ungroup()
}
gc()
###############

colnames(lgrid)

lgrid <- lgrid %>% select(hhidpn, wave, starts_with("riwstat"), starts_with(reqvars))
lgrid <- lgrid %>% select(-all_of(reqvars))


rm(reqvars)
# lgrid %>%
#   group_by(hhidpn) %>%
#   mutate(nwaves = n_distinct(wave)) %>%
#   ungroup() %>% select(nwaves) %>% table(useNA="always")