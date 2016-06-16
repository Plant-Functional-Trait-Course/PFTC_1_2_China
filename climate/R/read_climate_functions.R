####read.climate####

read.climate<-function(file){
  x <- read.csv(file, skip = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
  x$X. <- NULL # row numbers

  names(x)[1] <- "datetime"
  #get AM/PM
  x$datetime <- gsub("上午", "AM", x$datetime)
  x$datetime <- gsub("下午", "PM", x$datetime)
  
  x$datetime <- as.POSIXct(x$datetime, tz = "Asia/Chongqing", format = "%m/%d/%y %p%I时%M分%S秒")
  
#  UV=UV, 
  UV <- grep(pattern = "UV", names(x))
  if (length(UV) == 1) {
    UV <- x[, UV]
  } else if (length(UV) == 0) {
    UV <- NA
  } else{
    stop("UV")
  }
  
#  PAR=PAR, 
  PAR <- grep(pattern = "PAR", names(x))
  stopifnot(length(PAR) == 1)
  PAR <- x[, PAR]
  
  #  solar=solar, 
  solar <- grep(pattern = "Solar", names(x))
  stopifnot(length(solar) == 1)
  solar <- x[, solar]
  
#  RH
  RH <- grep(pattern = "RH", names(x))
  stopifnot(length(RH) == 1)
  RH <- x[, RH]
  
#  pressure=pressure, 
  pressure <- grep(pattern = "Pressure", names(x))
  stopifnot(length(pressure) == 1)
  pressure <- x[, pressure]
  if (length(grep("in.Hg", names(x))) == 1) {#convert to mbar
    pressure <- pressure * 33.8638
  }
  
#  windDirection=windDirection
  windDirection <- grep(pattern = "Wind.Direction", names(x))
  stopifnot(length(windDirection) == 1)
  windDirection <- x[, windDirection]
  
# windSpeed=windSpeed
  windSpeed <- grep(pattern = "Wind.Speed", names(x))
  stopifnot(length(windSpeed) == 1)
  windSpeed <- x[, windSpeed]
  if (length(grep("Wind.Speed..mph", names(x))) == 1) {#convert to m/s
    windSpeed <- windSpeed * 0.44704
  }
  
  
# gustSpeed=gustSpeed, 
  gustSpeed <- grep(pattern = "Gust.Speed", names(x))
  stopifnot(length(gustSpeed) == 1)
  gustSpeed <- x[, gustSpeed]
  if (length(grep("Gust.Speed..mph", names(x))) == 1) {#convert to m/s
    gustSpeed <- gustSpeed * 0.44704
  }
  
  
#  rain=rain, 
  rain <- grep(pattern = "Rain", names(x))
  stopifnot(length(rain) == 1)
  rain <- x[, rain]
  if (length(grep("Rain..in", names(x))) == 1) {#convert to mm
    rain <- rain * 25.4
  }
  
  #  waterContent5=waterContent5, 
  waterContent5 <-
    grepl(pattern = "Water.Content", names(x)) &
    grepl(pattern = "S.[MT].5", names(x))
  stopifnot(sum(waterContent5) == 1)
  waterContent5 <- x[, waterContent5]
  
  
#  waterContent20=waterContent20, 
  waterContent20 <-
    grepl(pattern = "Water.Content", names(x)) &
    grepl(pattern = "S.[MT].20", names(x))
  stopifnot(sum(waterContent20) == 1)
  waterContent20 <- x[, waterContent20]
  
  
#  temp=temp
  temp <-
    grepl(pattern = "Temp", names(x)) & !grepl(pattern = "S.T", names(x))
  stopifnot(sum(temp) == 1)
  temp <- x[, temp]
  if (class(temp) != "numeric") {
    temp2 <- as.numeric(temp)
    print(temp[is.na(temp2)])
    temp <- temp2
  }
  if (any(grepl(pattern = "Temp...F", names(x)))) {#convert to F to C
    temp <- (temp - 32) / 1.8
  }
  
#  soilTemp0=soilTemp0
  soilTemp0 <- grepl(pattern = "Temp", names(x)) & grepl(pattern = "S.T.0", names(x))
  stopifnot(sum(soilTemp0) == 1)
  soilTemp0 <- x[, soilTemp0]
  if (any(grepl(pattern = "Temp...F", names(x)))) {#convert to F to C
    soilTemp0 <- (soilTemp0 - 32) / 1.8
  }
  
#  soilTemp5=soilTemp5,
  soilTemp5 <- grepl(pattern = "Temp", names(x)) & grepl(pattern = "S.T.5", names(x))
  stopifnot(sum(soilTemp5) == 1)
  soilTemp5 <- x[, soilTemp5]
  if (any(grepl(pattern = "Temp...F", names(x)))) {#convert to F to C
    soilTemp5 <- (soilTemp5 - 32) / 1.8
  }
  
#  soilTemp20=soilTemp20
  soilTemp20 <- grepl(pattern = "Temp", names(x)) & grepl(pattern = "S.T.20", names(x))
  stopifnot(sum(soilTemp20) == 1)
  soilTemp20 <- x[, soilTemp20]
  if (any(grepl(pattern = "Temp...F", names(x)))) { #convert to F to C
    soilTemp20 <- (soilTemp20 - 32) / 1.8
  }
  
  res <- data.frame(
    datetime = x$datetime,
    UV = as.numeric(UV),
    PAR = as.numeric(PAR),
    solar = solar,
    RH = RH,
    pressure = pressure,
    windDirection = windDirection,
    windSpeed = windSpeed,
    gustSpeed = gustSpeed,
    rain = rain,
    waterContent5 = waterContent5,
    waterContent20 = waterContent20,
    temp = temp,
    soilTemp0 = soilTemp0,
    soilTemp5 = soilTemp5,
    soilTemp20 = soilTemp20
  )
  res[res == -888.88] <- NA #missing value code?
  res
}

####clean_climate####
clean_climate <- function(x){
  x <- within(
    x, {
    PAR[PAR < -800] <- NA
    solar[solar < -800] <- NA
 
    windSpeed[windSpeed == 64.21000] <- NA
    gustSpeed[gustSpeed == 64.21000] <- NA
    waterContent5[waterContent5 > 4] <- NA
    temp[temp < -80] <- NA
    soilTemp0[soilTemp0 < -80] <- NA    
    soilTemp0[soilTemp0 > 53] <- NA
    soilTemp5[soilTemp5 < -80] <- NA
    soilTemp20[soilTemp20 < -80] <- NA
})
  
  x
}

