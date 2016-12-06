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
weather <- plyr::ldply(fl, function(f){
  site <- gsub(".*YJG(?:-|_)([LMAH]).*", "\\1", f, perl = TRUE)
  stopifnot(!is.na(site))
  print(f)
  
  x <- read.table(f, header = TRUE, comment = "", stringsAsFactors = FALSE, skip = 1, fill = TRUE, sep = ",")
  x$X. <- NULL
  
  #unexpected names
  print(setdiff(names(x), make.names(varmap$old)))
  
  #make consistant names
  names(x) <- plyr::mapvalues(names(x), from = make.names(varmap$old), to = varmap$new, warn_missing = FALSE)
  
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

## delete bad data
weather <- weather %>% 
  #non-preciptation data from sites other than A
  mutate(rain = ifelse(site == "A", rain, NA)) %>%
  #non-windDirection data from A
  mutate(windDirection = ifelse(site == "A", NA, windDirection)) %>%
  # delete bad UV from H
  mutate(UV = ifelse(site == "H", NA, UV))

