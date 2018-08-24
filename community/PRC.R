# load libraries
library("vegan")
library("ggvegan")
library("gridExtra")
#example(prc)

source("community/start_here.R")
source("community/prcPlotwithoutSP.R")

## functional groups
fun_gp <- tbl(con, "taxon") %>% 
  select(species, functionalGroup) %>% 
  collect()

set.seed(2)

## prep data for ordination
cover_fat <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  left_join(fun_gp) %>% 
  filter(functionalGroup == "forb") %>% 
  select(-functionalGroup) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("local", "control", "warm1", "OTC")) %>%
  # remove rare species
  group_by(species) %>%
  filter(n() > 3) %>% 
  spread(key = species, value = cover, fill = 0) %>%
  mutate(TTtreat = droplevels(TTtreat)) %>% 
  mutate(newTT = plyr::mapvalues(TTtreat, c("warm1", "control", "local", "OTC"), c("warm1", "control", "control", "OTC")))


# COMMUNITY CHANGE AWAY FROM ORIGIN CONTROL (OTC and warm1)
# Transplant
cover_fat %>% 
  filter(newTT != "OTC", originSiteID != "L") %>% 
  # make year and treatment a factor, sort levels, so that control comes first
  mutate(year = factor(year), newTT = factor(newTT, levels = c("control", "warm1"))) %>% 
  group_by(originSiteID) %>% 
  do({
    # run prc
    mod = prc(response = (.) %>% select(-(originSiteID:TTtreat), -year, -newTT), treatment = .$newTT, time = .$year)
    
    # explained variance by treatment and p value
    data_frame(VarianceTreat = mod$CCA$tot.chi/mod$tot.chi,
               Pvalue = anova(mod)$'Pr(>F)'[1])
    # plot prc
    #plot(mod, species = FALSE)
  })



# OTC
cover_fat %>% 
  filter(TTtreat %in% c("OTC", "control")) %>% 
  mutate(TTtreat = factor(TTtreat), year = factor(year)) %>% 
  group_by(originSiteID) %>% 
  do({
    # run prc
    mod = prc(response = (.) %>% select(-(originSiteID:TTtreat), -year, -newTT), treatment = .$TTtreat, time = .$year)
    
    # explained variance by treatment and p value
    data_frame(VarianceTreat = mod$CCA$tot.chi/mod$tot.chi,
               Pvalue = anova(mod)$'Pr(>F)'[1])
    # plot prc
    #plot(mod, species = FALSE)
  })



# Species scores
SpScoreTransplant <- cover_fat %>% 
  filter(newTT != "OTC", originSiteID != "L") %>% 
  # make year and treatment a factor, sort levels, so that control comes first
  mutate(year = factor(year), newTT = factor(newTT, levels = c("control", "warm1"))) %>% 
  group_by(originSiteID) %>% 
  do({
    # run prc
    mod = prc(response = (.) %>% select(-(originSiteID:TTtreat), -year, -newTT), treatment = .$newTT, time = .$year)
    
    data_frame(species = names(summary(mod)$sp),
               SPscore = summary(mod)$sp,
               treatment = "Transplant")
  })

SpScoreOTC <- cover_fat %>% 
  filter(TTtreat %in% c("OTC", "control")) %>% 
  mutate(TTtreat = factor(TTtreat), year = factor(year)) %>% 
  group_by(originSiteID) %>% 
  do({
    # run prc
    mod = prc(response = (.) %>% select(-(originSiteID:TTtreat), -year, -newTT), treatment = .$TTtreat, time = .$year)
    
    data_frame(species = names(summary(mod)$sp),
               SPscore = summary(mod)$sp,
               treatment = "OTC")
  })


sp_name <- tbl(con, "taxon") %>% 
  select(species, speciesName, family) %>% 
  collect()

SpScore <- SpScoreTransplant %>% 
  rbind(SpScoreOTC) %>% 
  filter(SPscore > 0.5 | SPscore < -0.5) %>% 
  mutate(SPscore = round(SPscore, 3)) %>% 
  unite(originSiteID, treatment, col = "SiteTreat", sep = "_") %>% 
  spread(key = SiteTreat, value = SPscore) %>% 
  left_join(sp_name) %>% 
  select(speciesName, family, H_OTC, H_Transplant, A_OTC, A_Transplant, M_OTC, M_Transplant, L_OTC) %>% 
  # multiply with -1 if wrong direction
  mutate(M_Transplant = (-1) * M_Transplant,
         A_OTC = (-1) * A_OTC,
         M_OTC = (-1) * M_OTC,
         L_OTC = (-1) * L_OTC)

writexl::write_xlsx(x = SpScore, path = "community/FinalFigures/SpScore.xlsx")



# DIRECTIONAL COMMUNITY CHANGE AWAY FROM ORIGIN AND TOWARDS DESTINATION CONTROL
# H-A
coverFatHA <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  left_join(fun_gp) %>% 
  filter(functionalGroup == "forb") %>% 
  select(-functionalGroup) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("local", "control", "warm1", "OTC")) %>%
  mutate(newTT = plyr::mapvalues(TTtreat, c("warm1", "control", "local", "OTC"), c("warm1", "control", "control", "OTC"))) %>% 
  filter(originSiteID == "H" | originSiteID == "A" & newTT == "control") %>% 
  # remove rare species
  group_by(species) %>%
  filter(n() > 3) %>% 
  spread(key = species, value = cover, fill = 0) %>% 
  mutate(newTT = as.character(newTT)) %>% 
  mutate(newTT = ifelse(newTT == "control" & originSiteID == "A", "controlA", newTT)) %>% 
  mutate(year = factor(year), newTT = factor(newTT, levels = c("control", "warm1", "OTC", "controlA")))

communitydataHA <- coverFatHA %>% select(-(originSiteID:year), -newTT)
# select common species
spselectHA <- colSums(communitydataHA) > 50

fitHA <- prc(response = communitydataHA, treatment = coverFatHA$newTT, time = coverFatHA$year)
pHA <- plot(fitHA, species = FALSE, col = c("orange", "purple", "grey30"), xlab = "")

ggvegan:::autoplot.prc(fitHA, xlab = "", legend.position = "top", species = FALSE) +
  scale_colour_manual(values = c("orange", "purple", "grey30"))



# A-M
coverFatAM <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  left_join(fun_gp) %>% 
  filter(functionalGroup == "forb") %>% 
  select(-functionalGroup) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("local", "control", "warm1", "OTC")) %>%
  mutate(newTT = plyr::mapvalues(TTtreat, c("warm1", "control", "local", "OTC"), c("warm1", "control", "control", "OTC"))) %>% 
  filter(originSiteID == "A" | originSiteID == "M" & newTT == "control") %>% 
  # remove rare species
  group_by(species) %>%
  filter(n() > 3) %>% #distinct(species)
  spread(key = species, value = cover, fill = 0) %>% 
  mutate(newTT = as.character(newTT)) %>% 
  mutate(newTT = ifelse(newTT == "control" & originSiteID == "M", "controlM", newTT)) %>% 
  mutate(year = factor(year), newTT = factor(newTT, levels = c("control", "warm1", "OTC", "controlM")))

communitydataAM <- coverFatAM %>% select(-(originSiteID:year), -newTT)
# select common species
spselectAM <- colSums(communitydataAM) > 50

fitAM <- prc(response = communitydataAM, treatment = coverFatAM$newTT, time = coverFatAM$year)
pAM <- plot(fitAM, species = FALSE, col = c("orange", "purple", "grey30"), xlab = "")


# M-L
coverFatML <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  left_join(fun_gp) %>% 
  filter(functionalGroup == "forb") %>% 
  select(-functionalGroup) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("local", "control", "warm1", "OTC")) %>%
  mutate(newTT = plyr::mapvalues(TTtreat, c("warm1", "control", "local", "OTC"), c("warm1", "control", "control", "OTC"))) %>% 
  filter(originSiteID == "M" | originSiteID == "L" & newTT == "control") %>% 
  # remove rare species
  group_by(species) %>%
  filter(n() > 3) %>% #distinct(species)
  spread(key = species, value = cover, fill = 0) %>% 
  mutate(newTT = as.character(newTT)) %>% 
  mutate(newTT = ifelse(newTT == "control" & originSiteID == "L", "controlL", newTT)) %>% 
  mutate(year = factor(year), newTT = factor(newTT, levels = c("control", "warm1", "OTC", "controlL")))

communitydataML <- coverFatML %>% select(-(originSiteID:year), -newTT)
# select common species
spselectML <- colSums(communitydataML) > 50

fitML <- prc(response = communitydataML, treatment = coverFatML$newTT, time = coverFatML$year)
plot(fitML, species = FALSE, col = c("orange", "purple", "grey30"), xlab = "")
#, legpos = NA


# Make Fig 2
pHA <- autoplot.prcWithoutSP(fitHA, xlab = "", ylab = "Effect") +
  scale_colour_manual(values = c("orange", "purple", "grey30")) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  ggtitle("High alpine - Alpine") +
  guides(colour = FALSE, linetype = FALSE) +
  theme_light()

pAM <- autoplot.prcWithoutSP(fitAM, xlab = "", ylab = "Effect") +
  scale_colour_manual(values = c("orange", "purple", "grey30")) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  scale_y_reverse() +
  ggtitle("Alpine - Middle") +
  guides(colour = FALSE, linetype = FALSE) +
  theme_light()

pML <- autoplot.prcWithoutSP(fitML, xlab = "Year", ylab = "Effect") +
  scale_colour_manual(values = c("orange", "purple", "grey30"), labels = c("Transplant", "OTC", "Destination control")) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  ggtitle("Middle - Lowland") +
  guides(colour = FALSE, linetype = FALSE) +
  theme_light()

pp <- autoplot.prcWithoutSP(fitML, xlab = "", ylab = "Effect") +
  scale_colour_manual(values = c("orange", "purple", "grey30"), labels = c("Transplant", "OTC", "Destination control")) +
  scale_linetype_manual(values = c("solid", "solid", "dashed"), labels = c("Transplant", "OTC", "Destination control")) +
  ggtitle("Middle - Lowland") +
  theme(legend.position = "top")

prcLegend <- cowplot::get_legend(pp)

prcPlot <- grid.arrange(prcLegend, pHA, pAM, pML, 
             layout_matrix = rbind(c(1),c(2), c(2), c(2),c(3), c(3), c(3),c(4), c(4), c(4)))

ggsave(prcPlot, filename = "community/FinalFigures/prcPlot.jpg", height = 8, width = 6, dpi = 300)
ggsave(prcPlot, filename = "community/FinalFigures/prcPlot.pdf", height = 8, width = 6, dpi = 300)
