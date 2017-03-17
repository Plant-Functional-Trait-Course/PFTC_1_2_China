#
#load libraries
# intall packages plyr and gdata is required
library("openxlsx")
library("tidyverse")

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

# safe copy
otcc2 <- otcc

save(otcc, file = "climate/otcc.Rdata")
#load(file = "climate/otcc.Rdata")
summary(otcc)

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
  #remove duplicates
  select(-nummonth, -season) %>% 
  distinct(site, dateTime, .keep_all = TRUE)

save(otcc, file = "climate/otcc_clean.Rdata")
#load(file = "climate/otcc_clean.Rdata", verbose = TRUE)

####monthly OTC####
# prepare data
otcc <- otcc %>% 
  select(-file)

otc_month <- CalcMonthlyData(otcc)

# add missing months
full_grid <- expand.grid(variable = unique(otc_month$variable), site = unique(otc_month$site), month = seq(min(otc_month$month), max(otc_month$month), by = "month"))

otc_month <- left_join(full_grid, otc_month) %>% tbl_df()

save(otc_month, file = "climate/otc_month.Rdata")


#### yearly OTC
otc_year <- CalcYearlyData(otc_month)


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

## ----HourlyData
otcc %>% 
  select(dateTime, site, Tair30, Tsoil0, Tsoil5) %>% 
  gather(key = variable, value = value, -dateTime, -site) %>% 
  ggplot(aes(x = dateTime, y = value, colour = site)) + geom_line() + facet_grid(site ~ variable)

## ----CorrelationsWithinSite
### Correlations
# within sites
CorrT30_5 <- ggplot(otcc, aes(x = Tair30, y = Tsoil0)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")
CorrT30_0 <- ggplot(otcc, aes(x = Tair30, y = Tsoil5)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")
CorrT0_5 <- ggplot(otcc, aes(x = Tsoil0, y = Tsoil5)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")

## ----VariationLoggerSite
# Variation between loggers within site
diff.logger <- otcc %>% 
  select(dateTime, site, Tair30, Tsoil0, Tsoil5) %>% 
  mutate(T30_0 = Tair30 - Tsoil0, T30_5 = Tair30 - Tsoil5, T0_5 = Tsoil0 - Tsoil5) 

ggplot(diff.logger, aes(x = dateTime, y = T30_0)) + geom_point() + facet_wrap(~site)
ggplot(diff.logger, aes(x = dateTime, y = T30_5)) + geom_point() + facet_wrap(~site)
ggplot(diff.logger, aes(x = dateTime, y = T0_5)) + geom_point() + facet_wrap(~site)


## ----DailyData
### Daily data
daily_dat <- otcc %>% 
  mutate(day = ymd(format(dateTime, "%Y-%m-%d"))) %>%
  select(day, site, Tair30, Tsoil0, Tsoil5) %>% 
  gather(key = variable, value = value, -day, -site) %>% 
  group_by(day, site, variable) %>% 
  summarise(mean = mean(value), min = min(value), max = max(value), diff = max - min)

ggplot(daily_dat, aes(x = day, y = mean, colour = site)) + geom_line() + facet_grid(site ~ variable)


## ----DailyDiffMinMax
ggplot(daily_dat, aes(x = day, y = diff, colour = site)) + geom_point() + facet_wrap(variable ~site)

## ----DailyVarBetweenSite
site.daily <- daily_dat %>% 
  select(day, variable, site, mean) %>% 
  spread(key = site, value = mean)
p1 <- ggplot(site.daily, aes(x = H, y = A, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p2 <- ggplot(site.daily, aes(x = H, y = M, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p3 <- ggplot(site.daily, aes(x = H, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p4 <- ggplot(site.daily, aes(x = A, y = M, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p5 <- ggplot(site.daily, aes(x = A, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p6 <- ggplot(site.daily, aes(x = M, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
cowplot::plot_grid(p1, p2, p3, p4, p5, p6, labels = c("a)", "b)", "c)", "d)", "e)", "f)"), nrow = 2, ncol = 3)


## ----DailyVarWithinSite
# Within sites
daily_dat %>% 
  select(day, variable, site, mean) %>% 
  spread(key = variable, value = mean) %>%
  ggplot(aes(x = Tair30, y = Tsoil0)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")

daily_dat %>% 
  select(day, variable, site, mean) %>% 
  spread(key = variable, value = mean) %>%
  ggplot(aes(x = Tair30, y = Tsoil5)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")

daily_dat %>% 
  select(day, variable, site, mean) %>% 
  spread(key = variable, value = mean) %>%
  ggplot(aes(x = Tsoil0, y = Tsoil5)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")






### Monthly data
otc_month$site <- factor(otc_month$site, levels = c("H", "A", "M", "L"))

## ----MonthlyData
otc_month %>% 
  filter(variable %in% c("Tair30", "Tsoil0", "Tsoil5")) %>% 
  ggplot(aes(x = month, y = value, colour = variable)) + geom_line() + facet_grid(~ site)

## ----MonthlyVarBetweenSite
# Between site
site.dat <- otc_month %>% 
  filter(variable %in% c("Tair30", "Tsoil0", "Tsoil5")) %>% 
  spread(key = site, value = value)
p1 <- ggplot(site.dat, aes(x = H, y = A, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p2 <- ggplot(site.dat, aes(x = H, y = M, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p3 <- ggplot(site.dat, aes(x = H, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p4 <- ggplot(site.dat, aes(x = A, y = M, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p5 <- ggplot(site.dat, aes(x = A, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p6 <- ggplot(site.dat, aes(x = M, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
cowplot::plot_grid(p1, p2, p3, p4, p5, p6, labels = c("a)", "b)", "c)", "d)", "e)", "f)"), nrow = 2, ncol = 3)

## ----MonthlyVarWithinSite
# Within sites
otc_month %>% 
  spread(key = variable, value = value) %>%
  ggplot(aes(x = Tair30, y = Tsoil0)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")

otc_month %>% 
  spread(key = variable, value = value) %>%
  ggplot(aes(x = Tair30, y = Tsoil5)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")

otc_month %>% 
  spread(key = variable, value = value) %>%
  ggplot(aes(x = Tsoil0, y = Tsoil5)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")


## ----MonthlyVarWithinSiteDiff
# same plot but as difference between loggers over time
otc_month %>% 
  spread(key = variable, value = value) %>%
  select(month, site, Tair30, Tsoil0, Tsoil5) %>% 
  mutate(T30_0 = Tair30 - Tsoil0, T30_5 = Tair30 - Tsoil5, T0_5 = Tsoil0 - Tsoil5) %>% 
  gather(key = diff, value = value, -month, -site, -Tair30, -Tsoil0, -Tsoil5) %>% 
  ggplot(aes(x = month, y = value, colour = diff)) + geom_point() + facet_grid(diff~site) + geom_hline(yintercept = 0)






