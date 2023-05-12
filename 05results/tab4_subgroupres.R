# TABLE 4
############
load(paste0(dir_working, "04estimation/output/event_scesd_subgroups.rda"))

names_subgroups <- names(EST.ATT.CESD)

x <- EST.ATT.CESD[[names_subgroups[1]]]

tab <- pivot_longer(x[,1:3], cols = -et, values_to = "estimate")

tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Mean CES-D at e=-1", 
                                 estimate = signif(unique(x$depvar_pre), 4)))
tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Relative change from e=-1 to e=0", 
                                 estimate = signif(unique(x[x$et==0,]$relchg), 4)))
# tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Wald test-statistic",
#                                  estimate = signif(x[x$et==0,]$Wald, 4)))
# tab <- bind_rows(tab,  bind_cols(et = NULL, name = "p-value",
#                                  estimate = signif(x[x$et==0,]$pval, 4)))
tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Treated individuals", 
                                 estimate = signif(unique(x$Treated), 4)))
tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Observations", 
                                 estimate = signif(unique(x$Observations), 4)))
TAB <- tab
rm(tab)

rm(x)

for(x in lapply(names_subgroups[-1], function(x) EST.ATT.CESD[[x]])) {
  tab <- pivot_longer(x[,1:3], cols = -et, values_to = "estimate")
  
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Mean CES-D at e=-1", 
                                   estimate = signif(unique(x$depvar_pre), 4)))
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Relative change from e=-1 to e=0", 
                                   estimate = signif(unique(x[x$et==0,]$relchg), 4)))
  # tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Wald test-statistic",
  #                                  estimate = signif(x[x$et==0,]$Wald, 4)))
  # tab <- bind_rows(tab,  bind_cols(et = NULL, name = "p-value",
  #                                  estimate = signif(x[x$et==0,]$pval, 4)))
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Treated individuals", 
                                   estimate = signif(unique(x$Treated), 4)))
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Observations", 
                                   estimate = signif(unique(x$Observations), 4)))
  TAB <- bind_cols(TAB, tab[,3])
  
  rm(tab)
}
rm(x)

colnames(TAB) <- c("Event time", "Variable", names_subgroups)

write.csv(TAB, 
          row.names = FALSE, 
          paste0(dir_working, "05results/tables/tab4.csv"))

rm(TAB)
rm(names_subgroups)
rm(EST.ATT.CESD)
############