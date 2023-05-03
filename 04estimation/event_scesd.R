### ESTIMATE ATT-G AND AGGREGATE TO OBTAIN ATT-ET
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
####################################