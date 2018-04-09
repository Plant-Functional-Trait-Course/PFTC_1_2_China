#### Appendix for iButton data ####

library("tidyverse")
library("lubridate")

load(file = "Temperature_monthlyiButton.RData")

Sys.setlocale("LC_ALL", "en_GB")

head(monthlyiButton)
# create Transplant values (take controls from one step down)
WarmAirT <- monthlyiButton %>% 
  filter(depth == "air", treatment == "C", site != "H") %>%
  mutate(treatment = "Transplant") %>%
  rename(origSite = site) %>% 
  mutate(destSite = origSite) %>% 
  mutate(destSite = plyr::mapvalues(destSite, c("A", "M", "L"), c("H", "A", "M")))

monthlyiButton %>% 
  filter(depth == "air") %>% 
  mutate(destSite = site) %>% 
  rename(origSite = site) %>% 
  bind_rows(WarmAirT) %>%
  gather(key = Temperature, value = value, Tmean, Tmin, Tmax) %>%
  mutate(destSite = factor(destSite, levels = c("H", "A", "M", "L"))) %>% 
  ggplot(aes(x = month, y = value, color = treatment)) +
  geom_line() +
  scale_color_manual(name = "Treatment", values = c("grey", "purple", "orange")) +
  labs(x = "", y = "Mean monthly temperature °C") +
  facet_grid(Temperature ~ destSite, scales = "free") +
  theme_minimal()

monthlyTemps <- monthlyiButton %>% 
  filter(depth == "air") %>% 
  mutate(destSite = site) %>% 
  rename(origSite = site) %>% 
  bind_rows(WarmAirT) %>% 
  select(-Tmin, -Tmax, -origSite, -depth) %>%
  unite(united, Tmean, Tse, sep = "_") %>% 
  spread(key = treatment, value = united) %>% 
  separate(col = C, into = c("Control_mean", "Control_se"), sep = "_", convert = TRUE) %>%
  separate(col = OTC, into = c("OTC_mean", "OTC_se"), sep = "_", convert = TRUE) %>%
  mutate(Transplant = ifelse(is.na(Transplant), "NA_NA", Transplant)) %>% 
  separate(col = Transplant, into = c("Transplant_mean", "Transplant_se"), sep = "_", convert = TRUE) %>%
  mutate(OTC_mean = OTC_mean -  Control_mean, Transplant_mean = Transplant_mean - Control_mean) %>% 
  mutate(OTC_se = sqrt(Control_se^2 + OTC_se^2), Transplant_se = sqrt(Control_se^2 + Transplant_se^2)) %>% 
  select(-Control_mean, -Control_se) %>% 
  mutate(destSite = factor(destSite, levels = c("H", "A", "M", "L")))
write.csv(monthlyTemps, "monthlyTemps.csv")



# create Transplant values (take controls from one step down)
WarmAirTDayily <- dailyiButton %>% 
  filter(depth == "air", treatment == "C", site != "H") %>%
  mutate(treatment = "Transplant") %>%
  rename(origSite = site) %>% 
  mutate(destSite = origSite) %>% 
  mutate(destSite = plyr::mapvalues(destSite, c("A", "M", "L"), c("H", "A", "M")))


dailyiButton %>% 
  filter(depth == "air") %>% 
  mutate(destSite = site) %>% 
  rename(origSite = site) %>% 
  bind_rows(WarmAirTDayily) %>%
  mutate(diff = max - min) %>% 
  gather(key = Temperature, value = value, mean, min, max, diff) %>%
  mutate(destSite = factor(destSite, levels = c("H", "A", "M", "L"))) %>% 
  ggplot(aes(x = date, y = value, color = treatment)) +
  geom_line() +
  scale_color_manual(name = "Treatment", values = c("grey", "purple", "orange")) +
  labs(x = "", y = "Daily temperature °C") +
  facet_grid(Temperature ~ destSite, scales = "free") +
  theme_minimal()
  
  
  
