#
#load libraries

library("readxl")
library("openxlsx")
library("dplyr")
library("tidyr")

#get file list

fl <- dir(path = "climate/data/OTCs/", pattern = "xls$", recursive = TRUE, full.names = TRUE)

#loop over file list and extract data

otcc <- plyr::ldply(fl, function(f){
  print(f)
  col_names <-c("dateTime", "waterContent20", "Tsoil20", "waterContent5", "Tsoil5", "waterContent0", "Tsoil0", "RH", "Tair", "PAR")
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
summary(otcc)

#site names & file as factor
otcc$site <- factor(otcc$site, levels = c("H", "A", "M", "L"))
otcc$file <- reorder(otcc$file, otcc$date, min)


#clean 
otcc <- otcc %>% 
  #no real data before 2013
  filter(dateTime > "2013-01-01") %>%
  #max possible TAir == 50
  mutate(Tair = ifelse(Tair < 50, Tair, NA))%>%
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


#some plots
ggplot(otc_month, aes(x = month, y = value, colour = site)) + geom_path() + facet_wrap(~variable, scales = "free_y")

ggplot(otcc, aes(x = dateTime, y = Tair)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = RH)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = Tsoil0)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = Tsoil5)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = Tsoil20)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = waterContent20)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = waterContent5)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = waterContent0)) + geom_path() + facet_wrap(~site)
ggplot(otcc, aes(x = dateTime, y = PAR)) + geom_path() + facet_wrap(~site)

ggplot(otcc, aes(x = dateTime, y = file, group = file)) + geom_path() + facet_wrap(~site, scale = "free_y")
