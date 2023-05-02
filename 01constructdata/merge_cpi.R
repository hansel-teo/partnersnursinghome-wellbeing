# dir_input <- "/Volumes/GoogleDrive/My Drive/data/US/cpi/"
# dir_input <- "E:/My Drive/data/US/cpi/"

####################################
### CPI-U
cpi <- data.frame()
for(i in seq(1,4)){
  cpi <- bind_rows(cpi, 
                   read_excel(paste0("E:/My Drive/data/US/cpi/", "CUUR0", as.character(i), "00SA0.xlsx"), skip = 10) %>% 
                     select(-HALF1, -HALF2) %>%
                     mutate(region=i))
}

cpi_year <- cpi %>% select(region, Year, Annual)
cpi_year <- cpi_year %>% rename(cpi_reg_year = Annual)
CPI_year <- cpi_year

# cpi_month <- cpi %>% select(-Annual)
# colnames(cpi_month) <- c("Year", as.character(seq(1,12)), "region") 
# cpi_month <- cpi_month %>%
#   pivot_longer(cols = -c(region, Year),
#                names_to = "Month",
#                values_to = "cpi_reg_month")
# CPI_month <- cpi_month

rm(cpi, cpi_year)
####################################


### Load CPI data
# Change base year to 2012
base <- CPI_year[CPI_year$Year==2012,]
base <- base %>% 
  select(-Year) %>%
  rename(cpi_reg2012 = cpi_reg_year)

CPI_year <- left_join(CPI_year, base, by = "region")
CPI_year <- CPI_year %>%
  mutate(cpi_reg_year = cpi_reg_year/cpi_reg2012)

dat %>% select(rcenreg, riwendy, riwendm) %>% summary()

dat <- left_join(dat, 
                 CPI_year %>% select(Year, region, cpi_reg_year),
                 by = c("rcenreg" = "region", "riwendy" = "Year"))

rm(base, CPI_year)
