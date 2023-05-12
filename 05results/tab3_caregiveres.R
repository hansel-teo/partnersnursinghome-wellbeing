# TABLE 3
############
load(paste0(dir_working, "04estimation/output/event_scesd_caregiving.rda"))

x <- EST.ATT.CESD$`< 2 waves of caregiving`

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

for(x in lapply(names(EST.ATT.CESD)[2:4], function(i) EST.ATT.CESD[[i]])) {
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

colnames(TAB) <- c("Event time", "Variable", names(EST.ATT.CESD))

write.csv(TAB, 
          row.names = FALSE, 
          paste0(dir_working, "05results/tables/tab3.csv"))

############
