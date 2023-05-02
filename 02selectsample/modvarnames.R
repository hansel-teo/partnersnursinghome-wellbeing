### COVARIATES

rdemographics <- c("rafemale", 
                   "raracem", "ranonwhite", 
                   "raeduc", "racollege", "rahsdropout",
                   "ragey_i", "ragey_e", "ragey_grp", "ragey_65plus", "ragey.sq",
                   "racohbyr", 
                   "rmstat_grp",
                   "rcendiv", "rcenreg") 

rins <- c("rhiltc", "rgovmd", "riossdi")

rcarerecv <- c("rrscarehpw_i", "rrscarehpw_i.sq", 
               "rrscare_i", "rrccare_i", "rrrcare_i", "rrfcare_i",
               "rrnscare_i", "rrrfcare_i",
               "rrfrcare_i", "rhosp_e", "rhomcar_e")

radls <- c("rwalkra", "rdressa", "rbatha", "reata", "rbeda", "rtoilta", "rwalk1a", "rclim1a")

rcog <- c("rcogfunction_i", "rimpaired_i", "rdemented_i", "rcogtot27_imp", "rprxyscore_imp")
rdiag <- c("rmemrye_i", "rhearte", "rlunge", "rstroke", "rcancre", "rhibpe", "rdiabe", "rpsyche", "rarthre")
racute <- c("rheartprb", "rhrtatt", "rangin", "rconhrtf")

rhlth <- c("rshlt_grp", "rshlt_i", "roopmd_e_cens")

sdemographics <- c("safemale", 
                   "saracem",  "sanonwhite", 
                   "saeduc",  "sacollege",  "sahsdropout",
                   "sagey_i",  "sagey_e", "sagey_grp",  "sagey_65plus", "sagey.sq",
                   "slbrf_grp", "semployed")

shlth <- c("sshlt_grp", "sshlt_i")

hdemographics <- c("hlvwith_grp", "hspouseonly",
                   "hhhres", "hhhres.sq", "hhhres_grp",
                   "hchild", "hchild.sq", "hchild_grp") 

hfinancial <- c("haohous", 
                "hatotn_defl", "hatotf_defl", "hatoth_defl", "hitot_defl",
                "hatotn_defl_qtile", "hatotf_defl_qtile", "hatoth_defl_qtile", "hitot_defl_qtile",
                "hatotn_defl.pp", "hatotf_defl.pp", "hatoth_defl.pp", "hitot_defl.oecdeq")

rvars <- c(rdemographics, rins, rcarerecv, radls, rcog, rdiag, racute, rhlth, hdemographics, hfinancial)
svars <- c(sdemographics, shlth)
