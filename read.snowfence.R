# read libraries
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("readxl")


# Function to read in snowfence data
ReadExcelSheets <- function(sheet){
  dat <- read_excel("/Volumes/SILVER/transplant_climate/data/Snowfence/SoilTempratureMoisture_Gongga.xlsx", sheet = sheet, col_names = TRUE, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text")) # read excel file
  dat$distance <- substr(colnames(dat)[2],1,2) # grab first two characters
  names(dat) <- c("dateTime", "Tsoil0", "Tsoil5", "Tsoil20", "waterContent0", "waterContent5", "waterContent20", "Notes", "distance")
  dat
}

# Import
snowfence <- plyr::ldply(1:6, ReadExcelSheets)

# rename
snowfence <- snowfence %>% 
  mutate(distance = plyr::mapvalues(distance, c("0-", "2.", "5-", "10", "20", "CK"), c("0m", "2.5m", "5m", "10m", "20m", "Control"))) %>%  # rename
  mutate(distance = factor(distance, levels = c("0m", "2.5m", "5m", "10m", "20m", "Control"))) %>%  # order factor
  mutate(dateTime = ymd_hms(dateTime, tz = "Asia/Shanghai"))

# Data cleaning, remove spikes
snowfence2 <- snowfence %>% 
  mutate(Tsoil20 = ifelse(Tsoil20 > 25 | Tsoil20 < -10, NA, Tsoil20)) %>% 
  mutate(Tsoil5 = ifelse(Tsoil5 > 25 | Tsoil5 < -10, NA, Tsoil5)) %>% 
  mutate(Tsoil0 = ifelse(Tsoil0 > 25 | Tsoil0 < -10, NA, Tsoil0))
  
#summary(snowfence2)  
ggplot(snowfence2, aes(x = dateTime, y = Tsoil0)) + geom_line() + facet_wrap(~distance) + geom_vline(xintercept = as.numeric(ymd_hms("2016-03-01 00:00:01")), color = "red") + geom_vline(xintercept = as.numeric(ymd_hms("2016-05-01 00:00:01")), color = "blue")

head(snowfence)
dim(snowfence)


### SNOWDEPTH DATA
snowdepth <- read_excel("/Volumes/SILVER/transplant_climate/data/Snowfence/SnowDepth_Gongga.xlsx", col_names = TRUE, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "date"))
snowdepth$dateTime <- ymd_hms(snowdepth, tz = "Asia/Shanghai"))
head(snowdepth)
summary(snowdepth)

ggplot(snowdepth, aes(x = TIMESTAMP, y = Air_Temp)) + geom_line()
+ ylim(-0.25,0.5) + scale_x_datetime(date_breaks = "month") + theme(axis.text.x = element_text(angle = 90))



