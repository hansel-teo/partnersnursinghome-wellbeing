
# TABLE 2A
############
load(paste0(dir_working, "04estimation/output/event_scesd_mainresults.rda"))

x <- EST.ATT.CESD$Pooled

tab <- pivot_longer(x[,1:3], cols = -et, values_to = "estimate")

tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Mean CES-D at e=-1", 
                                 estimate = signif(unique(x$depvar_pre), 4)))
tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Relative change from e=-1 to e=0", 
                                 estimate = signif(unique(x[x$et==0,]$relchg), 4)))
tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Wald test-statistic",
                                 estimate = signif(x[x$et==0,]$Wald, 4)))
tab <- bind_rows(tab,  bind_cols(et = NULL, name = "p-value",
                                 estimate = signif(x[x$et==0,]$pval, 4)))
tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Treated individuals", 
                                 estimate = signif(unique(x$Treated), 4)))
tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Observations", 
                                 estimate = signif(unique(x$Observations), 4)))
TAB <- tab
rm(tab)

rm(x)

for(x in list(EST.ATT.CESD$`Non-caregivers`, EST.ATT.CESD$Caregivers)) {
  tab <- pivot_longer(x[,1:3], cols = -et, values_to = "estimate")
  
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Mean CES-D at e=-1", 
                                   estimate = signif(unique(x$depvar_pre), 4)))
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Relative change from e=-1 to e=0", 
                                   estimate = signif(unique(x[x$et==0,]$relchg), 4)))
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Wald test-statistic",
                                   estimate = signif(x[x$et==0,]$Wald, 4)))
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "p-value",
                                   estimate = signif(x[x$et==0,]$pval, 4)))
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Treated individuals", 
                                   estimate = signif(unique(x$Treated), 4)))
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Observations", 
                                   estimate = signif(unique(x$Observations), 4)))
  TAB <- bind_cols(TAB, tab[,3])
  
  rm(tab)
}


colnames(TAB) <- c("Event time", "Variable", "Pooled", "Non-caregivers", "Caregivers")

write.csv(TAB, 
          row.names = FALSE, 
          paste0(dir_working, "05results/tables/tab2a.csv"))

############
rm(EST.ATT.CESD)
rm(TAB)

# TABLE 2B
############
load(paste0(dir_working, "04estimation/output/event_scesd_pooledpsresults-20230410.rda"))

x <- EST.ATT.CESD$Pooled

tab <- pivot_longer(x[,1:3], cols = -et, values_to = "estimate")

tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Mean CES-D at e=-1", 
                                 estimate = signif(unique(x$depvar_pre), 4)))
tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Relative change from e=-1 to e=0", 
                                 estimate = signif(unique(x[x$et==0,]$relchg), 4)))
tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Wald test-statistic",
                                 estimate = signif(x[x$et==0,]$Wald, 4)))
tab <- bind_rows(tab,  bind_cols(et = NULL, name = "p-value",
                                 estimate = signif(x[x$et==0,]$pval, 4)))
tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Treated individuals", 
                                 estimate = signif(unique(x$Treated), 4)))
tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Observations", 
                                 estimate = signif(unique(x$Observations), 4)))
TAB <- tab
rm(tab)

rm(x)

for(x in list(EST.ATT.CESD$`Non-caregivers`, EST.ATT.CESD$Caregivers)) {
  tab <- pivot_longer(x[,1:3], cols = -et, values_to = "estimate")
  
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Mean CES-D at e=-1", 
                                   estimate = signif(unique(x$depvar_pre), 4)))
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Relative change from e=-1 to e=0", 
                                   estimate = signif(unique(x[x$et==0,]$relchg), 4)))
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Wald test-statistic",
                                   estimate = signif(x[x$et==0,]$Wald, 4)))
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "p-value",
                                   estimate = signif(x[x$et==0,]$pval, 4)))
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Treated individuals", 
                                   estimate = signif(unique(x$Treated), 4)))
  tab <- bind_rows(tab,  bind_cols(et = NULL, name = "Observations", 
                                   estimate = signif(unique(x$Observations), 4)))
  TAB <- bind_cols(TAB, tab[,3])
  
  rm(tab)
}


colnames(TAB) <- c("Event time", "Variable", "Pooled", "Non-caregivers", "Caregivers")

write.csv(TAB, 
          row.names = FALSE, 
          paste0(dir_working, "05results/tables/tab2b.csv"))

############
rm(EST.ATT.CESD)
rm(TAB)

