#litter

#libraries
library("tidyverse")
library("readxl")

litter_file <- "community/data/Litter Mass_2013-2016_OTCs.xlsx"
litter <- map_df(excel_sheets(litter_file), function(sheet){
  read_excel(litter_file, sheet = sheet) %>%
    mutate(
      turfID = gsub("(.*)-\\d{4}", "\\1", `Plot#`),
      TTtreat = gsub("[LMAH]\\d-(.*)", "\\1", turfID),
      site = substring(turfID, 1, 1), 
      year = as.numeric(sheet), 
      drymass = `drymass(g/25*25cm2)` * 16
      ) %>% 
    select(site, turfID, TTtreat, year, drymass) 
})

litter <- litter %>% mutate(
  site = factor(site, levels = c("H", "A", "M", "L")),
  TTtreat = recode(TTtreat, C = "Control")
  )

litter %>%
  ggplot(aes(x = year, y = drymass, colour = TTtreat, group = turfID)) + 
  geom_line(alpha = 0.2) + 
  stat_summary(mapping = aes(group = TTtreat), fun.y = mean, geom = "line") +
  stat_summary(mapping = aes(group = TTtreat), fun.data = mean_se, geom = "linerange", position = position_dodge(width = 0.1)) +
  facet_wrap(~ site) +
  labs(x = "Year", y = expression(Litter~biomass~gm^-2), colour = "Treatment")
