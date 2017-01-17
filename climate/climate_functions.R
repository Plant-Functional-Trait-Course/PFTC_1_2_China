#### CLIMATE DATA FUNCTIONS #####

#### CALCULATE MONTHLY VALUES FROM HOURLY DATA ####
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
  
otc_yearly <- CalcYearlyData(otc_month)

