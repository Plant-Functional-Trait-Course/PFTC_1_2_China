#load packages
library("ggplot2")
library("lubridate")
library("tidyr")
library("ggplot2")

#load functions
source("climate/R/read_climate_functions.R")

####################################################################################
#load climate
siteL <- rbind(
    read.climate("climate/data/YJG-L_2014.11.05_2015.04.23.csv"),
    read.climate("climate/data/YJG-L_2015.01.31_2015.07.18.csv")
  )
siteH <- rbind(
    read.climate("climate/data/YJG-H_2014.11.06_2015.04.01.csv"),
    read.climate("climate/data/YJG-H_2015.03.11_2015.08.27.csv")
  )
siteM <- read.climate("climate/data/9_YJG-M_2015.04.23_2015.07.19.csv")
siteA <- read.climate("climate/data/YJG-A_2015.04.23_2015.08.15.csv")

siteA$site <- "A"
siteH$site <- "H"
siteM$site <- "M"
siteL$site <- "L"


#clean climate
summary(rbind(siteA, siteM, siteL, siteH))
x <- gather(rbind(siteA, siteM, siteL, siteH), key = "variable", value = value, -datetime, -site)
ggplot(x, aes(x = value)) + geom_histogram() + geom_rug() + facet_wrap(~variable, scales = "free_x")

#soilTemp0 problems
ggplot(x%>%filter(variable == "soilTemp0"), aes(x = datetime, y = value, colour = site)) + geom_path()

ggplot(x%>%filter(variable == "soilTemp0"), aes(x = datetime, y = value, colour = site)) + geom_path() + scale_y_continuous(limits = c(NA, 54)) + geom_point()



siteA <- clean_climate(siteA)
siteH <- clean_climate(siteH)
siteM <- clean_climate(siteM)
siteL <- clean_climate(siteL)



#remove overlap
#difference in precision between duplicates
siteL[siteL$datetime == siteL$datetime[duplicated(siteL$datetime)][1], ]

remove_duplicates <- function(x) {
  #check for non-identical rows after rounding
  x2 <- cbind(x$datetime, plyr::numcolwise(round)(x, digits = 2))
  x2 <- unique(x2)
  s2 <- sum(duplicated(x2$datetime))

  if (s2 != 0)
    stop(paste(s2, "duplicate datestamps"))
  
  x[!duplicated(x$datetime),]
}

siteH <- remove_duplicates(siteH)
siteA <- remove_duplicates(siteA)
siteM <- remove_duplicates(siteM)
siteL <- remove_duplicates(siteL)

## combine data

klima <- rbind(siteH, siteA, siteM, siteL)
klima <- gather(klima, key = "variable", value = "value", -site, -datetime)
head(klima)

#calculate monthly means
threshold <- 30 * 24 * 6 / 4 #~ one week. Minimum accepted

mklima <- klima %>%
  filter(!is.na(value)) %>%
  mutate(month = dmy(paste0("15-",format(datetime, "%b.%Y")))) %>%
  group_by(site, month, variable) %>%
  summarise(n = n(),  mean = mean(value), sum = sum(value)) %>%
  filter(n > threshold) %>%
  mutate(value =  ifelse(variable == "rain", sum, mean)) %>%
  select(-mean, -sum)


monthlyClimate <- mklima %>%
  select(-n, -month) %>%  
  spread(key = variable, value = value)  


save(klima, file = "climate.Rdata")
save(monthlyClimate, mklima, file = "monthlyclimate.Rdata")
