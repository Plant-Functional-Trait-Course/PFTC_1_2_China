#### Appendix for iButton data ####

library("tidyverse")
library("lubridate")

load(file = "Temperature_monthlyiButton.RData")

Sys.setlocale("LC_ALL", "en_GB")

head(monthlyiButton)
WarmAirT <- monthlyiButton %>% 
  filter(depth == "air", treatment == "C", site != "H") %>%
  mutate(treatment = "Warm") %>%
  rename(origSite = site) %>% 
  mutate(destSite = origSite) %>% 
  mutate(destSite = plyr::mapvalues(destSite, c("A", "M", "L"), c("H", "A", "M")))

monthlyiButton %>% 
  filter(depth == "air") %>% 
  mutate(destSite = site) %>% 
  rename(origSite = site) %>% 
  bind_rows(WarmAirT) %>%
  mutate(destSite = factor(destSite, levels = c("H", "A", "M", "L"))) %>% 
  ggplot(aes(x = month, y = Tmean, color = treatment)) +
  geom_line() +
  scale_color_manual(name = "Treatment", values = c("grey", "red", "purple")) +
  labs(x = "", y = "Mean monthly temperature °C") +
  facet_grid(~ destSite) +
  theme_minimal()
