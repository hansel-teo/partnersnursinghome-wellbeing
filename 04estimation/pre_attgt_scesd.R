### ESTIMATE PRE-TREATMENT EFFECTS
####################################
rm(attg_pre)
summary(attg_pre  <- att_gt(yname = "r.cesd",
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
                            data = dat %>%
                                  filter(et>=test_min)))
####################################