#
#load libraries
# intall packages plyr and gdata is required
library("openxlsx")
library("tidyverse")

#get file list

fl <- dir(path = "climate/data_raw/WeatherStation/ClimateData/OTCs/", pattern = "xls$", recursive = TRUE, full.names = TRUE)

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

# safe copy
otcc2 <- otcc

save(otcc, file = "climate/otcc.Rdata")
#load(file = "climate/otcc.Rdata")
summary(otcc)


otcc <- otcc2
#site names & file as factor
otcc$site <- factor(otcc$site, levels = c("H", "A", "M", "L"))
otcc$file <- reorder(otcc$file, otcc$date, min)


#clean 
otcc <- otcc %>% 
  # add season to check remove spikes in different parts of the year
  mutate(nummonth = month(as.POSIXlt(dateTime, format="%Y/%m/%d %H/%m/%s"))) %>% 
  mutate(season = ifelse(nummonth %in% c(12,1,2), "Winter",
                         ifelse(nummonth %in% c(3,4,5), "Spring", 
                                ifelse(nummonth %in% c(6,7,8), "Summer", "Autumn")))) %>% 
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) %>% 
  #no real data before 2013
  filter(dateTime > "2013-01-01") %>%
  #max possible TAir30 == 40, min == -25
  mutate(Tair30 = ifelse(Tair30 < 40 & Tair30 > -25, Tair30, NA)) %>%
  mutate(Tair30 = ifelse(season == "Summer" & Tair30 < -4, NA, Tair30)) %>%
  mutate(Tair30 = ifelse(season == "Autumn" & Tair30 < -15, NA, Tair30)) %>%
  mutate(Tair30 = ifelse(season == "Spring" & site == "M" & Tair30 < -13, NA, Tair30)) %>%
  mutate(Tair30 = ifelse(season == "Winter" & site == "M" & Tair30 < -22, NA, Tair30)) %>%
  #relative humidity should have max 1 (0-1 scale) & move to percent
  mutate(RH = ifelse(RH < 1, RH * 100, NA)) %>%
  #tsoil0 threshold -10 removes most spikes
  mutate(Tsoil0 = ifelse(Tsoil0 > -10, Tsoil0, NA))  %>%
  mutate(Tsoil0 = ifelse(season == "Summer" & Tsoil0 < -3, NA, Tsoil0)) %>%
  mutate(Tsoil0 = ifelse(season == "Spring" & Tsoil0 < -7.8, NA, Tsoil0)) %>%
  mutate(Tsoil0 = ifelse(season == "Autumn" & site == "L" & Tsoil0 < 1.41, NA, Tsoil0)) %>%
  mutate(Tsoil0 = ifelse(season == "Winter" & site == "A" & Tsoil0 < -8.7, NA, Tsoil0)) %>%
  #tsoil5 threshold -6 removes most spikes
  mutate(Tsoil5 = ifelse(Tsoil5 > -6, Tsoil5, NA)) %>%
  mutate(Tsoil5 = ifelse(season == "Summer" & Tsoil5 < -3, NA, Tsoil5)) %>%
  mutate(Tsoil5 = ifelse(season == "Autumn" & site == "L" & Tsoil5 < 2, NA, Tsoil5)) %>%
  #tsoil20 threshold -6 removes most spikes
  mutate(Tsoil20 = ifelse(Tsoil20 > -6, Tsoil20, NA)) %>%
  mutate(Tsoil20 = ifelse(season == "Summer" & Tsoil20 < -2.5, NA, Tsoil20)) %>%
  mutate(Tsoil20 = ifelse(season == "Autumn" & Tsoil20 < -2, NA, Tsoil20)) %>%
  mutate(Tsoil20 = ifelse(dateTime == as.POSIXct("2015-12-02 11:20:00", tz = "Asia/Shanghai") & site == "H", NA, Tsoil20)) %>%
  mutate(Tsoil20 = ifelse(dateTime == as.POSIXct("2013-09-28 18:40:00", tz = "Asia/Shanghai") & site == "L", NA, Tsoil20)) %>%
  # waterContent5 > 0
  mutate(
    waterContent20 = ifelse(waterContent20 > 0, waterContent20, NA),
    waterContent5 = ifelse(waterContent5 > 0, waterContent5, NA),
    waterContent0 = ifelse(waterContent0 > 0, waterContent0, NA)
  ) %>%
  mutate(waterContent5 = ifelse(nummonth %in% c(1,2,3,11,12), NA, waterContent5)) %>% # remove waterContent5 in winter: 1.11 - 30.3
  mutate(waterContent5 = ifelse(site == "H" & waterContent5 < 0.2, NA, waterContent5)) %>%  # remove spikes
  mutate(waterContent5 = ifelse(site %in% c("A", "M") & waterContent5 < 0.15, NA, waterContent5)) %>%  # remove spikes 
  mutate(waterContent5 = ifelse(site == "L" & waterContent5 < 0.35, NA, waterContent5)) %>%   # remove spikes
  select(-nummonth, -season) %>% 
  #remove duplicates
  distinct(site, dateTime, .keep_all = TRUE)


save(otcc, file = "climate/otcc_clean.Rdata")
#load(file = "climate/otcc_clean.Rdata", verbose = TRUE)




# Some plots to check the data
# To add lines between the seasons
#dateline <- c("2013-06-01 00:00:01", "2013-09-01 00:00:01", "2013-12-01 00:00:01", "2014-03-01 00:00:01", "2014-06-01 00:00:01", "2014-09-01 00:00:01", "2014-12-01 00:00:01", "2015-03-01 00:00:01", "2015-06-01 00:00:01", "2015-09-01 00:00:01", "2015-12-01 00:00:01", "2016-03-01 00:00:01", "2016-06-01 00:00:01", "2016-09-01 00:00:01")
#+ geom_vline(xintercept = as.numeric(ymd_hms(dateline)), color = "red")
ggplot(otcc, aes(x = dateTime, y = Tair30)) + geom_path() + facet_wrap(~site)  
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

