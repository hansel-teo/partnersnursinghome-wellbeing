### ESTIMATE ATT-G AND AGGREGATE TO OBTAIN ATT-ET
rm(attg, attet)
attg  <- att_gt(yname = outcome,
                tname = "wave",
                idname = "e.hhidpn",
                gname = "treatwave",
                xformla = regspec,
                est_method = "reg",
                base_period = "universal",
                control_group = "nevertreated",
                # anticipation = 2,
                panel = TRUE,
                allow_unbalanced_panel = TRUE,
                bstrap = TRUE,
                cband = FALSE, alp = 0.05,
                clustervars = "e.hhidpn",
                weightsname = "weights",
                data = dat)

attet <- aggte(attg, type = "dynamic", 
               min_e = window_min,
               max_e = window_max,
               balance_e = window_max,
               na.rm=TRUE, bstrap = TRUE, cband = FALSE, clustervars = "e.hhidpn") 

# summary(attet)
# ggdid(attet)
