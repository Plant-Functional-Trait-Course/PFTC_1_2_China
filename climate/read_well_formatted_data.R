#read 2016 data (and hopefully more when formatting fixed)
library("readr")

#get_list of files
fl <- dir(path = "/Volumes/FELLES/MATNAT/BIO/Ecological and Environmental Change/TransplantChina/ClimateData/WellFormatted", pattern = "csv$", full.names = TRUE)


plyr::ldply(fl, function(file){
  dat <- read_csv(file, skip = 1)
  sort(names(dat))#YUK
})


dat <- plyr::ldply(fl, function(file){
  print(file)
  
  #read file
  dat <- read_csv(file, skip = 1)
  
  #get site
  site <- gsub(".*YJG(?:-|_)([LMAH]).*", "\\1", file, perl = TRUE)
  stopifnot(!is.na(site))
  dat$site <- site

  #get file
  dat$file <- basename(file)
  
  #rename columns
  names(dat)[grep("Date Time", names(dat))] <- "dateTime"
  names(dat)[grep("Wind Speed", names(dat))] <- "windSpeed"
  names(dat)[grep("Wind Direction", names(dat))] <- "windDirection"
  names(dat)[grep("UV, umolm", names(dat))] <- "UV"
  names(dat)[grep("Solar Radiation", names(dat))] <- "solarRadiation"
  names(dat)[grep("RH", names(dat))] <- "RH"
  names(dat)[grep("Rain", names(dat))] <- "rain"
  names(dat)[grep("Pressure", names(dat))] <- "Pressure"
  names(dat)[grep("PAR", names(dat))] <- "PAR"  
  names(dat)[grep("Gust Speed", names(dat))] <- "gustSpeed"
  names(dat)[grep("Voltage", names(dat))] <- "Batt"

  #more complex for temperature and soil moisture
  names(dat)[grepl("Temp, °C", names(dat)) & grepl("T-20", names(dat))] <- "Tsoil20"
  names(dat)[grepl("Temp, °C", names(dat)) & grepl("T-5", names(dat))] <- "Tsoil5"
  names(dat)[grepl("Temp, °C", names(dat)) & grepl("T-0", names(dat))] <- "Tsoil0"
  names(dat)[grepl("Temp, °C", names(dat))] <- "Tair"  
  
  names(dat)[grepl("Water Content", names(dat)) & grepl("-20)$", names(dat))] <- "waterContent20"
  names(dat)[grepl("Water Content", names(dat)) & grepl("-5)$", names(dat))] <- "waterContent5"
  
  #remove # column
  dat <- dat[, !names(dat) == "#"]
  
  #dateTime
  dat$dateTime <- gsub("上午", "AM", dat$dateTime)
  dat$dateTime <- gsub("下午", "PM", dat$dateTime)
  
  dat$dateTime <- as.POSIXct(dat$dateTime, tz = "Asia/Chongqing", format = "%m/%d/%y %p%I时%M分%S秒")
  
  
  dat
})

