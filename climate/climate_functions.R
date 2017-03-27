#### CLIMATE DATA FUNCTIONS #####

#### CALCULATE MONTHLY VALUES FROM HOURLY DATA ####
# for wide data frame!
# dd = data, should only contain
# dateTime
# climate variables (name does not matter, except rain needs to be called "rain")
# site

CalcMonthlyData<-function(dd){ 
  dd <- dd %>%
    mutate(month = lubridate::ymd(format(dateTime, "%Y-%m-15"))) %>% 
    select(-dateTime) %>%
    gather(key = variable, value = value, -site, -month) %>%
    group_by(site, month, variable) %>%
    filter(!is.na(value)) %>%
    summarise(meanV = mean(value), sumV = sum(value), n = n()) %>%
    mutate(value = ifelse(variable == "rain", sumV, meanV)) %>%
    filter(n > 6 * 24 * 7 * 3) %>% #at least three weeks of data
    select(-meanV, -sumV, -n)
  
  return(dd)
}


#### CALCULATE YEARLY VALUES FROM MONTHLY DATA ####
# dd = data, should only contain
# month
# site
# variables (names of climate variables does not matter, except rain needs to be called "rain")
# value

CalcYearlyData<-function(dd){ 
  dd <- dd %>%
    mutate(year = year(month)) %>% 
    group_by(site, year, variable) %>%
    filter(!is.na(value)) %>%
    summarise(meanV = mean(value), sumV = sum(value), n = n()) %>%
    mutate(value = ifelse(variable == "rain", sumV, meanV)) %>%
    filter(n > 11) %>% #only if data from every month available
    select(-meanV, -sumV, -n)
  
  return(dd)
}
  


#### ZOOM INN PLOT FUNCTION ####
ddd <- climate %>% 
  filter(variable == "Tsoil5")



ZoomIntoPlot <- function(data){
    data %>% 
      filter(dateTime > as.POSIXct("2016-07-01 01:00:00", tz = "Asia/Shanghai")) %>%
      filter(logger == "otc") %>% 
      filter(site == "M") %>% 
      ggplot(aes(x = dateTime, y = value, colour = logger)) + 
      geom_line()
  }
ZoomIntoPlot(data = ddd)  
  
  
ZoomIntoPlot <- function(data = climate, Site, start_date, end_date, Variable, Logger){
  data %>% 
    filter(dateTime > as.POSIXct(ymd(start_date)), dateTime < as.POSIXct(ymd(end_date))) %>%
    filter(site == Site) %>% 
    filter (variable == Variable) %>% 
    ggplot(aes(x = dateTime, y = Variable, colour = Site)) + 
    geom_line() +
    facet_wrap(~ Site)
    #scale_x_datetime(date_breaks = breaks) +
    #theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0)) +
    #ggtitle(label = SITE)
}

ZoomIntoPlot(Site = "H", start_date = "2013-03-01 01:50:00", end_date = "2016-08-20 01:50:00", Logger = "gradient")
ZoomIntoPlot(climate, SITE = "M", VARIABLE = "Tsoil5")
