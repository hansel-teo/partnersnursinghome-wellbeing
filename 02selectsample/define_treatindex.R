dat_using <- dat_hrs %>% select(hhidpn, wave, riwstat, hhhid, all_of(vars_tr))
dat_using <- dat_using %>% arrange(hhidpn, wave)

lapply(all_of(vars_tr), function(x) {
  dat_using %>% select(all_of(x)) %>% table(useNA="always")
})


dat_using <- dat_using %>%
  mutate(across(.cols = all_of(vars_tr), 
                .fns = ~ as.integer(replace_na(., 0)),
                .names = "{.col}"))


### Define treatment variables
dat_using <- dat_using %>%
  group_by(hhidpn) %>%
  mutate(across(.cols = all_of(vars_tr), 
                .fns = ~ as.integer(.==1)*as.integer(cumsum(.)),
                .names = "{.col}_index")) %>%
  ungroup() 

# dat_using %>%
#   group_by(hhidpn) %>%
#   filter(sum(rnrshom)>1) %>%
#   ungroup() %>%
#   select(hhidpn, wave, rnrshom, rnrshom_index) %>% head(20)


dat_using <- dat_using %>%
  mutate(across(.cols = all_of(vars_tr), 
                .fns = ~if_else(.==1, as.integer(wave), as.integer(999)),
                .names = "{.col}_wv"))

# dat_using %>%
#   group_by(hhidpn) %>%
#   filter(sum(rnrshom)>1) %>%
#   ungroup() %>%
#   select(hhidpn, wave, contains(vars_tr)) %>% print(n=100)

dat_using <- dat_using %>%
  arrange(hhidpn, wave) %>%
  group_by(hhidpn) %>%
  mutate(across(.cols = all_of(paste0(vars_tr, "_wv")),
                .fns = ~ as.integer(min(., na.rm=TRUE)),
                .names = "{.col}1")) %>%
  ungroup()


# Check
# lapply(vars_tr, function(x) {
#   dat_using %>%
#     group_by(hhidpn) %>%
#     filter(sum(get(x))>1) %>%
#     ungroup() %>%
#     select(-hhidpn, - hhhid, -all_of(vars_tr)) %>% head(20)
# })
lapply(vars_tr, function(x) {
  dat_using %>%
    select(paste0(x, "_wv1")) %>% table(useNA="always")
})


dat_using <- dat_using %>%
  group_by(hhidpn) %>%
  mutate(across(.cols = all_of(paste0(vars_tr, "_index")),
                .fns = ~as.integer(max(., na.rm=TRUE)),
                .names = "{.col}_n")) %>%
  ungroup()


# dat_using %>% 
#   group_by(hhidpn) %>%
#   filter(sum(rnrshom)>1) %>%
#   ungroup() %>%
#   select(hhidpn, wave, rnrshom, rnrshom_index, rnrshom_wv1, rnrshom_index_n) %>% head(20)
lapply(vars_tr, function(x) {
  dat_using %>%
    select(paste0(x, "_index_n")) %>% table(useNA="always")
})

dat_using %>%
  filter(rnrshom_index_n ==10) %>% head(20)

### Define treated respondent
dat_using <- dat_using %>%
  group_by(hhidpn) %>%
  mutate(across(.cols = paste0(vars_tr, "_index_n"),
                .fns = ~ if_else(.==0, as.integer(0), as.integer(1)),
                .names = "tr_{.col}")) %>%
  ungroup()

dat_using <- dat_using %>%
  rename_with(.cols = starts_with("tr_"),
              .fn = ~ str_replace(., "_index_n", ""))

lapply(vars_tr, function(x) {
  dat_using %>%
    select(paste0(x, "_wv1"), paste0("tr_", x)) %>% table(useNA="always")
})


# ### Define treated couple
# dat_using <- dat_using %>%
#   group_by(hhhid, wave) %>%
#   mutate(across(.cols = paste0("tr_", vars_tr),
#                 .fns = ~ max(.),
#                 .names = "cpl{.col}")) %>%
#   ungroup() %>%
#   group_by(hhhid) %>%
#   mutate(across(.cols = paste0("cpltr_", vars_tr),
#                 .fns = ~ max(.),
#                 .names = "{.col}")) %>%
#   ungroup()
# 
# # dat_using %>% group_by(tr_rnrshom_e) %>% summarise(n(), sum(rnrshom_e_wv1<999))
# lapply(vars_tr, function(x) {
#   dat_using %>%
#     group_by(get(paste0("cpltr_", x))) %>%
#     summarise(n(), n_distinct(hhidpn), n_distinct(hhhid))
# })
# 
# dat_using %>% group_by(cpltr_rnrshom_e) %>% summarise(n(), sum(rnrshom_e_wv1<999))


### Identify first spell and its length
####################################
spells <- lapply(Map(list, vars_tr, paste0(vars_tr, "_wv1")), 
                 function(x){
                   dat_using %>%
                     filter(wave >= get(x[[2]])) %>%
                     arrange(hhidpn, wave) %>%
                     group_by(hhidpn) %>%
                     mutate(tspell = cumall(get(x[[1]]))) %>%
                     ungroup() %>%
                     mutate(tspell = if_else(is.na(tspell) & wave == get(x[[2]]), TRUE, tspell)) %>%
                     arrange(hhidpn, wave) %>%
                     group_by(hhidpn) %>%
                     mutate(tdur = cumsum(tspell)) %>%
                     mutate(tdur = max(tdur)) %>%
                     ungroup() %>%
                     select(hhidpn, wave, tspell, tdur) %>%
                     rename('tspl1_{x[[1]]}' := tspell,
                            'tdur1_{x[[1]]}' := tdur)})

spells$rnrshom_e %>% tail(20)

for(i in 1:length(spells)) {
  dat_using <- left_join(dat_using, spells[[i]], by = c("hhidpn", "wave"))
}
rm(spells)
####################################



### Identify second spell and its length
####################################
dat_using <- left_join(dat_using, 
                       dat_using %>% 
                         filter(wave > rnrshom_e_wv1) %>% 
                         filter(tspl1_rnrshom_e == FALSE) %>%
                         group_by(hhidpn) %>%
                         mutate(across(.cols = all_of(paste0(vars_tr, "_wv")),
                                       .fns = ~ as.integer(min(., na.rm=TRUE)),
                                       .names = "{.col}2")) %>%
                         ungroup() %>%
                         select(hhidpn, wave, ends_with("_wv2")),
                       by = c("hhidpn", "wave"))

dat_using %>% select(ends_with("_wv2")) %>% table(useNA="always")

dat_using <- dat_using %>% 
  group_by(hhidpn) %>% 
  fill(ends_with("_wv2"), .direction = "downup") %>%
  ungroup()

dat_using %>% select(ends_with("_wv2")) %>% table(useNA="always")


spells <- lapply(Map(list, vars_tr, paste0(vars_tr, "_wv2")), 
                 function(x){
                   dat_using %>%
                     filter(wave >= get(x[[2]])) %>%
                     arrange(hhidpn, wave) %>%
                     group_by(hhidpn) %>%
                     mutate(tspell = cumall(get(x[[1]]))) %>%
                     ungroup() %>%
                     mutate(tspell = if_else(is.na(tspell) & wave == get(x[[2]]), TRUE, tspell)) %>%
                     arrange(hhidpn, wave) %>%
                     group_by(hhidpn) %>%
                     mutate(tdur = cumsum(tspell)) %>%
                     mutate(tdur = max(tdur)) %>%
                     ungroup() %>%
                     select(hhidpn, wave, tspell, tdur) %>%
                     rename('tspl2_{x[[1]]}' := tspell,
                            'tdur2_{x[[1]]}' := tdur)})

spells$rnrshom_e %>% tail(20)

for(i in 1:length(spells)) {
  dat_using <- left_join(dat_using, spells[[i]], by = c("hhidpn", "wave"))
}
rm(spells)
####################################



### Identify third spell and its length
####################################
dat_using <- left_join(dat_using, 
                       dat_using %>% 
                         filter(wave > rnrshom_e_wv2) %>% 
                         filter(tspl2_rnrshom_e == FALSE) %>%
                         group_by(hhidpn) %>%
                         mutate(across(.cols = all_of(paste0(vars_tr, "_wv")),
                                       .fns = ~ as.integer(min(., na.rm=TRUE)),
                                       .names = "{.col}3")) %>%
                         ungroup() %>%
                         select(hhidpn, wave, ends_with("_wv3")),
                       by = c("hhidpn", "wave"))

dat_using %>% select(ends_with("_wv3")) %>% table(useNA="always")

dat_using <- dat_using %>% 
  group_by(hhidpn) %>% 
  fill(ends_with("_wv3"), .direction = "downup") %>%
  ungroup()

dat_using %>% select(ends_with("_wv3")) %>% table(useNA="always")


spells <- lapply(Map(list, vars_tr, paste0(vars_tr, "_wv3")), 
                 function(x){
                   dat_using %>%
                     filter(wave >= get(x[[2]])) %>%
                     arrange(hhidpn, wave) %>%
                     group_by(hhidpn) %>%
                     mutate(tspell = cumall(get(x[[1]]))) %>%
                     ungroup() %>%
                     mutate(tspell = if_else(is.na(tspell) & wave == get(x[[2]]), TRUE, tspell)) %>%
                     arrange(hhidpn, wave) %>%
                     group_by(hhidpn) %>%
                     mutate(tdur = cumsum(tspell)) %>%
                     mutate(tdur = max(tdur)) %>%
                     ungroup() %>%
                     select(hhidpn, wave, tspell, tdur) %>%
                     rename('tspl3_{x[[1]]}' := tspell,
                            'tdur3_{x[[1]]}' := tdur)})

for(i in 1:length(spells)) {
  dat_using <- left_join(dat_using, spells[[i]], by = c("hhidpn", "wave"))
}
rm(spells)
####################################


dat_using %>%
  summarise(n(), 
            `N. first spells` = sum(rnrshom_e_wv1==wave), 
            `N. second spells` = sum(rnrshom_e_wv2==wave, na.rm=TRUE),
            `N. third spells` = sum(rnrshom_e_wv3==wave, na.rm=TRUE))

dat_using %>%
  filter(rnrshom_e_wv1==wave) %>%
  filter(riwstat==1) %>%
  select(tdur1_rnrshom_e) %>% summary()

dat_using %>%
  filter(rnrshom_e_wv2==wave) %>%
  filter(riwstat==1) %>%
  select(tdur2_rnrshom_e) %>% summary()

dat_using %>%
  filter(rnrshom_e_wv3==wave) %>%
  filter(riwstat==1) %>%
  select(tdur3_rnrshom_e) %>% summary()


vars_tr_nrshom <- dat_using %>% select(-riwstat, -hhhid, -all_of(vars_tr))

rm(dat_using, vars_tr)
