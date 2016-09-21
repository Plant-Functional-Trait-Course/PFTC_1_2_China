#read climate data
#problems.
#1 time corrupted
#2 first data column concatenated onto datetime
#3 remaining columns out-by-one
#4 column names not consistant
#5 quaint colonial units
#6 overlapping times
#7 logger errors

#other fixes
#replaced missing comma in line 12464 of file YJG-H_2013.11.12_2014.04.30.csv (first two columns were merged - remaining columns shifted across)

#load libraries
library("plyr")
library("lubridate")

#list of files
fl <- dir("climate/data/csv", pattern = "csv$", recursive = TRUE, full.names = TRUE)


#fix column names by mapping names onto consistant pattern
#headers <- sapply(fl, read.table, sep = ",", header = FALSE, skip = 1, nrow = 1, comment = "", stringsAsFactors = FALSE)

#table(unlist(headers, recursive = TRUE))
#cbind(unique(unlist(headers, recursive = TRUE)), unique(unlist(headers, recursive = TRUE)))
varmap <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
'old new
"Date Time, GMT+08:00"   "dateTime"  
"PAR, uE"                "PAR"               
"Solar Radiation, W/m²"  "solarRadiation" 
"Rain, in"               "rain"              
"Tsoil,(F,-5cm)"         "Tsoil5"        
"Tsoil,(F,0cm)"          "Tsoil0"         
"Tsoil,(F,-20cm)"        "Tsoil20"       
"Water Content,-5cm)"    "waterContent5"   
"Water Content, -20cm)"  "waterContent20" 
"Wind Speed, mph"        "windSpeed"       
"Wind Direction, ø"      "windDirection"     
"Tair,(F)"               "Tair"              
"RH, %"                  "RH"                 
"UV, umolm-2s (LBL: UV)" "UV"
"Tair,F"                 "Tair"                
"RH, %(air)"             "RH"            
"notes"                  "notes"                 
"UV, umolm-2s"           "UV"          
"Batt, V"                "Batt"               
"RH, % (air)"            "RH"           
"Temp,(F,-5)"            "Tsoil5"           
"Temp(F,-20)"            "Tsoil20"           
"Temp(F,0cm)"            "Tsoil0"           
"Water Content,-20cm)"   "waterContent20"')

##
#step 1 combine files
weather <- ldply(fl, function(f){
  site <- gsub(".*YJG(?:-|_)([LMAH]).*", "\\1", f, perl = TRUE)
  stopifnot(!is.na(site))
  print(f)
  
  x <- read.table(f, header = TRUE, comment = "", stringsAsFactors = FALSE, skip = 1, fill = TRUE, sep = ",")
  x$X. <- NULL
  
  #unexpected names
  print(setdiff(names(x), make.names(varmap$old)))
  
  #make consistant names
  names(x) <- mapvalues(names(x), from = make.names(varmap$old), to = varmap$new, warn_missing = FALSE)
  
  #split datatime column into datatime and first data column
  #number after last ? is data column
  dataCol <- gsub(".*\\?(\\d*\\.*d*)", "\\1", x$dateTime)
  dataCol <- as.numeric(dataCol)
  
  #everything after the hour is junk
  datetime <- gsub("^(\\d{2}/\\d{2}/\\d{2} (下午|上午)\\d{2}).*", "\\1", x$dateTime)
  stopifnot(!any(is.na(datetime)))
  
  #estimate minutes
  #every ten minutes. assign to 00 10 20 30 40 50 
  #estimate number to start
  stopifnot(max(table(datetime)) == 6)
  start <- sum(datetime[1:6] == datetime[1])#number of ten minutes in first hour
  start <- 6 - start + 1
  mins <- c("00", "10", 20, 30, 40, 50)
  all_mins <- c(mins[start:6], rep(mins, ceiling(length(datetime)/6)))[1:length(datetime)]
  datetime <- paste(datetime, all_mins, sep = "")
  
  #convert (下午|上午) to am/pm 下午 == PM
  AM <- grepl("上午", datetime)
  datetime <-  gsub("上午", "", datetime)
  datetime <-  gsub("下午", "", datetime)
  
  datetime <-  paste(datetime, ifelse(AM, "am", "pm"))
  
  #switch locale so time conversion works
  loc <- Sys.getlocale(category = "LC_TIME")
  Sys.setlocale(category = "LC_TIME", locale = "en_GB.UTF-8")
  
  #convert time to posxit
  datetime <- mdy_hm(datetime, tz =  "Asia/Shanghai")
  if(any(is.na(datetime))){
    warning("unparsed datetime", f,  "row", which(is.na(datetime)))
  }
    
  #revert locale
  Sys.setlocale(category = "LC_TIME", locale = loc)
  
  #check time in order
  #stopifnot(all(diff(datetime) > 0))
  if(any(diff(datetime) <= 0, na.rm = TRUE))warning("Time inversion in ", f)
  x$dateTime <- datetime
  
  #shunt columns along
  #first check last column is empty
  x2 <- cbind(site, x[, 1, drop = FALSE], dataCol, x[, -1])
  
  if(all(is.na(x[, ncol(x)]))) {#last column all blank
    names(x2) <- c(names(x2)[-3], "delme")
    x2$delme <- NULL
  } else {
    names(x2) <- c(names(x2)[-3], "extra")
  } 
  x2$file <- basename(f)
  x2
})

#convert site to factor (to ensure plotting order)
weather$site <- factor(weather$site, levels = c("H", "A", "M", "L"))

##
#problems
weather[is.na(as.numeric(weather$Tair)),]
weather[is.na(as.numeric(weather$RH)),]
weather[is.na(as.numeric(weather$waterContent5)),]


#quick fix
weather$Tair <- as.numeric(weather$Tair)
weather$RH <- as.numeric((weather$RH))
weather$waterContent5 <- as.numeric((weather$waterContent5))

#extra columns
weather <- weather[, !grepl("X", names(weather))]

save(weather, file = "climate/weather_unclean.Rdata")

#unit conversion  - probably F to C
#clean climate

library("dplyr")
library("ggplot2")

F_C <- function(x)(x-32)/1.8

#assume files with median Tair >20 are in F, otherwise C
weather <- weather %>%
  group_by(file) %>%
  mutate(probableTempUnit = ifelse(median(Tair, na.rm = TRUE) > 20, "F", "C")) %>%
  mutate(Tair = ifelse(probableTempUnit == "F", F_C(Tair), Tair)) %>%
  mutate(Tsoil0 = ifelse(probableTempUnit == "F", F_C(Tsoil0), Tsoil0)) %>%
  mutate(Tsoil5 = ifelse(probableTempUnit == "F", F_C(Tsoil5), Tsoil5)) %>%
  mutate(Tsoil20 = ifelse(probableTempUnit == "F", F_C(Tsoil20), Tsoil20)) 


#zap impossible values
weather <- weather %>% 
  mutate(Tair = ifelse(Tair > 100 | Tair < -50, NA, Tair)) %>%
  mutate(Tsoil0 = ifelse(Tsoil0 > 60 | Tsoil0 < -50, NA, Tsoil0)) %>%
  mutate(Tsoil5 = ifelse(Tsoil5 > 50 | Tsoil5 < -5, NA, Tsoil5)) %>%
  mutate(Tsoil20 = ifelse(Tsoil20 > 50 | Tsoil20 < -5, NA, Tsoil20)) 

#remove minimal variance in Tsoil0 Toil5

weather %>% group_by(file) %>%
  mutate(sd = sd(soil5, na.rm = TRUE)) %>%
  summarise(sd = sd(Tsoil5, na.rm = TRUE)) %>% print(n = 100)

weather %>% group_by(file) %>%
#  mutate(sd = sd(soil0, na.rm = TRUE)) %>%
  summarise(sd = sd(Tsoil0, na.rm = TRUE)) %>% print(n = 100)

weather <- weather %>% 
  group_by(file) %>%
  mutate(sd = sd(Tsoil5, na.rm = TRUE)) %>% #Tsoil5
  mutate(Tsoil5 = ifelse(sd < 0.02, NA, Tsoil5)) %>%
  mutate(sd = sd(Tsoil0, na.rm = TRUE)) %>% #Tsoil0
  mutate(Tsoil0 = ifelse(sd < 0.1, NA, Tsoil0)) %>%
  select(-sd)

#zap zero variance solarRadiation variability (all in site A)
weather <- weather %>% 
  group_by(file) %>%
  mutate(sd = sd(solarRadiation, na.rm = TRUE)) %>% #Tsoil5
  mutate(solarRadiation = ifelse(sd < 0.02, NA, solarRadiation)) %>%
  select(-sd)

#soil moisture - zap impossible values
ggplot(weather, aes(x = dateTime, y = waterContent20, group = file, colour = site)) + geom_path() + facet_wrap(~site)

weather <- weather %>%
  mutate(waterContent5 = ifelse(waterContent5 < 0 | waterContent5 > 1, NA, waterContent5)) %>%
  mutate(waterContent20 = ifelse(waterContent20 < 0 | waterContent20 > 1, NA, waterContent20))

save(weather, file = "climate/weather.Rdata")

## deal with duplicates (from overlapping files)

#check variance is low amongst duplicates

weather %>% 
  group_by(dateTime, site) %>% 
   ungroup() %>%
    mutate(dateTime = ymd(format(dateTime, "%Y-%m-%d"))) %>%
   select(-notes, -Batt, -probableTempUnit, -extra) %>%
  select(file, dateTime, site, Tsoil5, Tsoil0, Tsoil20, waterContent5, waterContent20) %>%
  group_by(file, dateTime, site) %>%
  mutate(n = n()) %>% 
  filter(n == 6*24) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  select(-file, -n) %>%
  gather(key = variable, value = value, - dateTime, -site) %>% 
  group_by(dateTime, variable, site) %>% 
  summarise(delta = diff(range(value))) %>% 
  arrange(desc(delta))



weather %>% filter(dateTime == ymd_hms("2014-04-18 13:30:00", tz = "Asia/Shanghai"), site == "H") %>% select(dateTime, site, PAR, Tair)


weather %>% 
  ungroup() %>% 
  filter(site == "H", dateTime > ymd("2015-06-10", tz = "Asia/Shanghai") & dateTime < ymd("2015-06-20", tz = "Asia/Shanghai")) %>%
  select(-notes, -Batt, -probableTempUnit, -extra) %>%
  gather(key = variable, value = value, -dateTime,-file, -site) %>% 
  filter(!is.na(value)) %>%
  ggplot(aes(x = dateTime, y = value, colour = file)) + geom_path(size = .1, show.legend = FALSE) + facet_wrap(~variable, scales = "free_y") 



weather %>% filter(site == "H") %>% group_by(file) %>% summarise(cor = cor(rain, windSpeed))

weather %>%
  ggplot(aes(x = windSpeed, y = rain, colour = site)) + 
    geom_point() +
    facet_wrap(~site, scales = "free_y") + 
    xlim(c(NA, 26)) 

weather %>%
  ggplot(aes(x =rain, fill = site)) + 
    geom_histogram() +
  facet_wrap(~site, scales = "free")

weather %>% filter(site == "A") %>% ggplot(aes(x = dateTime, y = windDirection, colour = site, group = file)) + geom_path() + facet_wrap(~site)

weather %>% filter(site == "A") %>% ggplot(aes(x = dateTime, y = windSpeed, colour = site, group = file)) + geom_path() + facet_wrap(~site)

weather %>% filter(site == "A") %>% ggplot(aes(x = windSpeed, y = windDirection, colour = site, group = file)) + geom_point() + facet_wrap(~site)

#UV
ggplot(weather, aes(x = dateTime, y = UV, colour = site, group  = file)) + geom_path() + facet_wrap(~site)

## delete non-preciptation data from sites other than A, non-windDirection data from A
weather <- weather %>% 
  mutate(rain = ifelse(site == "A", rain, NA)) %>%
  mutate(windDirection = ifelse(site == "A", NA, windDirection))




#remove duplicates - start with last file 

distinct_weather <- weather %>% 
  ungroup() %>% 
  mutate(file  = reorder(file, dateTime, max, na.rm = TRUE)) %>%
  arrange(file) %>%
  distinct(site, dateTime, .keep_all = TRUE) %>%
  select(-file, -probableTempUnit, -notes, -Batt, -extra)


save(distinct_weather, file = "climate/clean_weather.Rdata")

#### make monthly climate ####
monthly <- distinct_weather %>% 
  mutate(month = ymd(format(dateTime, "%Y-%m-15"))) %>%
  select(-dateTime) %>%
  gather(key = variable, value = value, -site, -month) %>%
  group_by(site, month, variable) %>%
  filter(!is.na(value)) %>%
  summarise(meanV = mean(value), sumV = sum(value), n = n()) %>%
  mutate(value = ifelse(variable == "rain", sumV, meanV)) %>%
  filter(n > 6 * 24 * 7 * 3) %>% #at least three weeks of data
  select(-meanV, -sumV, -n)

#add missing months
full_grid <- expand.grid(variable = unique(monthly$variable), site = unique(monthly$site), month = seq(min(monthly$month), max(monthly$month), by = "month"))

monthly <- left_join(full_grid, monthly) %>%tbl_df()

save(monthly, file = "climate/monthly_climate.Rdata")

ggplot(monthly, aes(x = month, y = value, colour = site)) + geom_line() + facet_wrap(~variable, scale = "free_y")