#
#load libraries
# intall packages plyr and gdata is required
library("openxlsx")
library("dplyr")
library("tidyr")

#get file list

fl <- dir(path = "/Volumes/FELLES/MATNAT/BIO/Ecological and Environmental Change/TransplantChina/ClimateData/OTCs", pattern = "xls$", recursive = TRUE, full.names = TRUE)

#loop over file list and extract data

otcc <- plyr::ldply(fl, function(f){
  print(f)
  col_names <-c("dateTime", "waterContent20", "Tsoil20", "waterContent5", "Tsoil5", "waterContent0", "Tsoil0", "RH", "Tair30", "PAR")
  # ot <- read_excel(f, sheet = 1, skip = 3,
  #                  col_types = c("date", rep("numeric", 9)),
  #                  col_names = col_names
  #       )
 ot <- gdata::read.xls(f, sheet = 1, header = FALSE, skip = 3, stringsAsFactors = FALSE, na.strings = "#N/A!", comment = "")
 names(ot) <- col_names
 ot$site <- gsub(".+YJG-([LMAH]).+", "\\1", f)
 ot$file <- basename(f)
 ot$dateTime <- as.POSIXct(ot$dateTime, format = "%Y-%m-%d %H:%M", tz = "Asia/Shanghai")
  ot
})


save(otcc, file = "climate/otcc.Rdata")
#load(file = "climate/otcc.Rdata")
summary(otcc)

#site names & file as factor
otcc$site <- factor(otcc$site, levels = c("H", "A", "M", "L"))
otcc$file <- reorder(otcc$file, otcc$date, min)


# Check spikes in each season
otcc %>%
  mutate(nummonth = month(as.POSIXlt(dateTime, format="%Y/%m/%d %H/%m/%s"))) %>% 
  mutate(season = ifelse(nummonth %in% c(12,1,2), "Winter",
                         ifelse(nummonth %in% c(3,4,5), "Spring", 
                                ifelse(nummonth %in% c(6,7,8), "Summer", "Autumn")))) %>% 
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) %>% 
  group_by(site, season, year(dateTime)) %>% summarise(min = min(Tair30, na.rm = TRUE), max = max(Tsoil0, na.rm = TRUE)) %>%
  print(n=60)


#clean 
otcc2 <- otcc %>% 
  # add season
  mutate(nummonth = month(as.POSIXlt(dateTime, format="%Y/%m/%d %H/%m/%s"))) %>% 
  mutate(season = ifelse(nummonth %in% c(12,1,2), "Winter",
                         ifelse(nummonth %in% c(3,4,5), "Spring", 
                                ifelse(nummonth %in% c(6,7,8), "Summer", "Autumn")))) %>% 
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) %>% 
  group_by(site, season, year(dateTime)) %>% summarise(min = min(Tair30, na.rm = TRUE), max = max(Tsoil0, na.rm = TRUE))
  #no real data before 2013
  filter(dateTime > "2013-01-01") %>%
  #max possible TAir30 == 40, min == -25
  mutate(Tair30 = ifelse(Tair30 < 40 & Tair30 > -25, Tair30, NA)) %>%
  mutate(Tair30 = ifelse(season %in% c("Summer", "Autumn") & Tsoil0 < -3, NA, Tsoil0)) %>%
  #relative humidity should have max 1 (0-1 scale) & move to percent
  mutate(RH = ifelse(RH < 1, RH * 100, NA)) %>%
  #tsoil0 threshold -10 removes most spikes
  mutate(Tsoil0 = ifelse(Tsoil0 > -10, Tsoil0, NA))  %>%
  #tsoil5 threshold -6 removes most spikes
  mutate(Tsoil5 = ifelse(Tsoil5 > -6, Tsoil5, NA)) %>%
  #tsoil20 threshold -6 removes most spikes
  mutate(Tsoil20 = ifelse(Tsoil20 > -6, Tsoil20, NA)) %>%
  #soil moisture > 0
  mutate(
    waterContent20 = ifelse(waterContent20 > 0, waterContent20, NA),
    waterContent5 = ifelse(waterContent5 > 0, waterContent5, NA),
    waterContent0 = ifelse(waterContent0 > 0, waterContent0, NA)
  ) %>%
  #remove duplicates
  distinct(site, dateTime, .keep_all = TRUE)

save(otcc, file = "climate/otcc_clean.Rdata")
####monthly OTC####

otc_month <- otcc %>%
  mutate(month = lubridate::ymd(format(dateTime, "%Y-%m-15"))) %>%
  select(-dateTime, -file) %>%
  gather(key = variable, value = value, -site, -month) %>%
  group_by(site, month, variable) %>%
  filter(!is.na(value)) %>%
  summarise(meanV = mean(value), sumV = sum(value), n = n()) %>%
  mutate(value = ifelse(variable == "rain", sumV, meanV)) %>%
  filter(n > 6 * 24 * 7 * 3) %>% #at least three weeks of data
  select(-meanV, -sumV, -n)

# add missing months
full_grid <- expand.grid(variable = unique(otc_month$variable), site = unique(otc_month$site), month = seq(min(otc_month$month), max(otc_month$month), by = "month"))

otc_month <- left_join(full_grid, otc_month) %>% tbl_df()

save(otc_month, file = "climate/otc_month.Rdata")


#some plots
library("ggplot2")
ggplot(otc_month, aes(x = month, y = value, colour = site)) + geom_path() + facet_wrap(~variable, scales = "free_y")

dateline <- c("2013-06-01 00:00:01", "2013-09-01 00:00:01", "2013-12-01 00:00:01", "2014-03-01 00:00:01", "2014-06-01 00:00:01", "2014-09-01 00:00:01", "2014-12-01 00:00:01", "2015-03-01 00:00:01", "2015-06-01 00:00:01", "2015-09-01 00:00:01", "2015-12-01 00:00:01", "2016-03-01 00:00:01", "2016-06-01 00:00:01", "2016-09-01 00:00:01")
ggplot(otcc2, aes(x = dateTime, y = Tair30)) + geom_path() + facet_wrap(~site) + geom_vline(xintercept = as.numeric(ymd_hms(dateline)), color = "red") 
ggplot(otcc, aes(x = dateTime, y = RH)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = Tsoil0)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = Tsoil5)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = Tsoil20)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = waterContent20)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = waterContent5)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = waterContent0)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = PAR)) + geom_path() + facet_wrap(~site)

ggplot(otcc, aes(x = dateTime, y = file, group = file)) + geom_path() + facet_wrap(~site, scale = "free_y")

#minute resolution
otcc %>% group_by(site, file) %>% mutate(d = c(NA, diff(dateTime)) == 1) %>% ggplot(aes(x = dateTime, y = file, colour = d, size = d)) + geom_point() + facet_wrap(~site)