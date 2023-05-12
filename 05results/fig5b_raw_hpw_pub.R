# LOAD REQUIRED PACKAGES
require(tidyverse)

# Estimation sample
load(paste0(dir_working, "03preanalysis/samp_event.rda"))

dat_plot <- dat_event %>%
  select(e.hhidpn, wave, treated, t0, et,
         r.cesd, rrscarehpw_qtile,
         rrscaredur1_pre, rrscaredur2_pre,
         rrscare_i_pre, rrscarehpw_i_pre)

### Define FD of CESD at admission wave  
dat_plot <- dat_plot %>%
  arrange(e.hhidpn, wave) %>%
  group_by(e.hhidpn) %>%
  mutate(r.cesd.fd = r.cesd - lag(r.cesd,1)) %>%
  ungroup()
###


dat_plot %>%
  filter(rrscare_i_pre==1) %>%
  group_by(treated) %>%
  summarise(n(), across(.cols = rrscarehpw_i_pre,
                   .fns = list(~quantile(.,0.2), ~quantile(.,0.4), ~quantile(.,0.6), ~quantile(.,0.8))))

dat_plot %>%
  filter(rrscare_i_pre==1) %>%
  group_by(treated) %>%
  summarise(n(), across(.cols = rrscarehpw_i_pre,
                        .fns = list(~quantile(.,0.1), ~quantile(.,0.2), ~quantile(.,0.3), 
                                    ~quantile(.,0.4), ~quantile(.,0.5), ~quantile(.,0.6))))

dat_plot <- dat_plot %>%
  mutate(hpw_grp = cut(rrscarehpw_i_pre, breaks = c(0,7,14,42,84,168), 
                       include.lowest = FALSE, right = TRUE, label = FALSE))


### Define mean rrscarehpw within treated x hpw_grp bin
dat_plot <- left_join(dat_plot,
                      dat_plot %>%
                        filter(rrscare_i_pre==1)  %>%
                        group_by(treated, rrscarehpw_qtile) %>%
                        summarise(binx = mean(rrscarehpw_i_pre)),
                      by = c("treated", "rrscarehpw_qtile"))

dat_plot %>%
  filter(wave==t0) %>%
  filter(rrscare_i_pre==1)  %>%
  select(binx, rrscarehpw_qtile, treated) %>% table()

### Define mean r.cesd.fd within treated x hpw_grp bin
dat_plot <- left_join(dat_plot,
                      dat_plot %>%
                        filter(et==0) %>%
                        filter(rrscare_i_pre==1)  %>%
                        group_by(treated, rrscarehpw_qtile) %>%
                        summarise(biny = mean(r.cesd.fd)),
                      by = c("treated", "rrscarehpw_qtile"))

### Plot binned-scatter
fig <- ggplot(data=dat_plot %>%
                filter(rrscare_i_pre==1), 
              aes(x=binx, y =biny,
                  linetype = treated,
                  shape = treated)) + geom_point(size=1.8)

fig  + 
  geom_smooth(formula = y~x,
              method="lm",
              se=FALSE, 
              color = "grey30",
              linewidth=0.25,
              size=0.6) +
  # scale_colour_viridis_d(end = 0.55, option = "D") + scale_fill_viridis_d(end = 0.55, option = "D") + 
  scale_linetype_manual(name =  "Group:", labels = c("Comparison", "Treated"), values = c("dashed", "solid")) +
  scale_shape_manual(name =  "Group:", labels = c("Comparison", "Treated"), values = c(1, 16)) +
  scale_y_continuous(#limits = c(-0.25,1.05)
    name  = "Change in CES-D score at NH admission wave",  
                     breaks = seq(-0.2,1,0.2), labels =seq(-0.2,1,0.2)) +
  scale_x_continuous(#limits = c(0,168),
                     name = "Hours of care per week",
                     breaks = seq(0,168,28), labels = seq(0,168,28)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 6), 
        axis.title = element_text(size = 6), 
        axis.line = element_line(linewidth=0.3),
        axis.ticks = element_line(linewidth=0.3),
        legend.position = "bottom",
        # legend.direction = "vertical",
        legend.margin  = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        legend.box.spacing = unit(6, units = "pt"),
        legend.key.size = unit(8, units = "pt"),
        legend.title =  element_text(size = 6), 
        legend.text = element_text(size = 6), 
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5, size = 9))


ggsave(path = paste0(dir_working, "05results/figures/"), 
       filename = "fig5b.jpg", 
       device = "jpg", dpi = 1200, units = "mm", width = 90, height = 60)  
######

rm(dat_plot)
rm(fig)
