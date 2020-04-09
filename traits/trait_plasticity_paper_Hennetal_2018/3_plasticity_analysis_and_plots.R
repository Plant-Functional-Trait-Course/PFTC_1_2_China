library(tidyverse)
library(dplyr)

####load and organize data####
load("traits/data/china_intra_trait.RData")
#comm.trait.data <- read.csv("china_community_traits.csv")
comm.trait.data <- read.csv("traits/data/China_moments_site_level_for_Jon_9_13_2018.csv")
cont_dist <- read.csv("traits/data/cont_trait_dist.csv") %>% 
  mutate(Taxon.x = Taxon) %>% 
  select(-Taxon, -X)

###graph of community means with 95%ci (Figure 2) ####
comm.trait.data2 <- comm.trait.data %>%
  filter(variable == "area_mean" | variable == "sla_mean" | variable == "ldmc_mean" | variable == "thickness_mean" | variable == "c_pct_mean" | variable == "cn_ratio_mean" | variable == "n_pct_mean" | variable == "dc13_pct_mean" | variable == "p_pct_mean" | variable == "dn15_pct_mean" | variable == "np_ratio_mean")

comm.trait.data3 <- comm.trait.data2 %>% 
  mutate(site = factor(site, levels(site)[c(3, 4, 1, 2)])) %>% 
  mutate(elevation = plyr::mapvalues(site, from = c("L", "M", "A", "H"), to = c("3000", "3500", "3750", "4100"))) %>% 
  mutate(variable = plyr::mapvalues(variable, from = c("sla_mean", "ldmc_mean", "area_mean", "thickness_mean", "n_pct_mean", "c_pct_mean", "p_pct_mean", "cn_ratio_mean", "dc13_pct_mean", "dn15_pct_mean", "np_ratio_mean"), to = c("SLA", "LDMC", "LA", "LT", "'%'*'N'", "'%'*'C'", "'%'*'P'", "'C'*':'*'N'", "paste(delta^13, 'C')", "'N'*':'*'P'", "paste(delta^15, 'N')"))) %>% 
  mutate(variable = as.factor(variable)) %>% 
  droplevels(.)

comm.trait.data3$variable <- factor(comm.trait.data3$variable,levels(comm.trait.data3$variable)[c(1, 11, 6, 10, 2, 7, 9, 3, 8, 4, 5)])

comm.trait.data3 %>%   
  ggplot(aes(x = elevation, y = mean_val)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_95_ci, ymax = upper_95_ci, width = 0), size = 1) +
  facet_wrap(~variable, scales = "free_y", labeller = label_parsed) +
  theme_bw() +
  theme(text = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle = 90)) + 
  ylab("Mean Trait Value (+/- 95% CI)") +
  xlab("Elevation (m)")

#### prepare data for figures 3 and 4 along with associated analysis ####
#extract and rename useful variables from community distribution data
comm.trait.data1 <- comm.trait.data %>%
  filter(variable == "area_mean" | variable == "sla_mean" | variable == "ldmc_mean" | variable == "thickness_mean" | variable == "c_pct_mean" | variable == "cn_ratio_mean" | variable == "n_pct_mean" | variable == "dc13_pct_mean" | variable == "dn15_pct_mean" | variable == "p_pct_mean" | variable == "np_ratio_mean") %>% 
  select(site, variable, mean_val)  %>% 
  mutate(variable = plyr::mapvalues(variable, from = c("area_mean", "sla_mean", "ldmc_mean", "thickness_mean", "c_pct_mean", "cn_ratio_mean", "n_pct_mean", "dc13_pct_mean", "dn15_pct_mean", "p_pct_mean", "np_ratio_mean"), to = c("mean.LA", "mean.SLA", "mean.LDMC", "mean.LT", "mean.C", "mean.CN", "mean.N", "mean.C13", "mean.N15", "mean.P", "mean.NP")))

#make trait data wide
trait.data <- intra.trait.data %>% 
  group_by(trait) %>% 
  spread(key = trait, value = mean.trait) %>% 
  select( -n) %>% 
  mutate(t.LA = log(mean.LA)) %>% 
  mutate(t.SLA = log(mean.SLA)) %>% 
  mutate(t.P = log(mean.P)) %>% 
  mutate(mean.NP = mean.N/mean.P) %>% 
  gather(key = Trait, value = mean.trait, -Project, -origin, -Taxon, -rep) %>% 
  spread(key = Project, value = mean.trait)

#made comm data wide
comm.data <- comm.trait.data1 %>% 
  group_by(variable) %>% 
  spread(key = variable, value = mean_val) %>% 
  mutate(Taxon = "Community") %>% 
  mutate(rep = 1) %>% 
  mutate(Project = "Cont") %>% 
  mutate(t.LA = log(mean.LA)) %>% 
  mutate(t.SLA = log(mean.SLA)) %>% 
  mutate(t.P = log(mean.P)) %>% 
  gather(key = variable, value = mean_val, -Project, -site, -Taxon, -rep)

#get comm and species datasets to match structure for combining them.
colnames(comm.data)[1] <- "origin"


#These blocks of code combine the community mean trait values with the individual trait values for transplanted plants. There are two joins, which are slightly different to line up traits of transplanted individuals with either the mean community trait values of their home or their destination site.
comm.data$comp <- rep(c("H", " ", "M", "A"), 14)
warm.data.all <- trait.data %>% 
  #this first join links the traits of the transplanted individuals to the community mean value at their destination site
  left_join(comm.data, by = c("origin" = "comp", "Trait" = "variable")) %>% 
  #this first join links the traits of the transplanted individuals to the community mean value at their origin site
  left_join(comm.data, by = c("origin" = "origin", "Trait" = "variable")) %>%
  select(origin, Taxon.x, Trait, rep.x, Control, warm1, cool1, mean_val.x, mean_val.y) %>% 
  #this is to clariy what each column means. dest = the destination community mean trait value, home = home community mean trait value. Control = individual trait values for each species in their home site, warm1 = individual trait values for each species in warming tranplants, cool1 = individual trait values for each species in cooling transplants.
  rename(dest = mean_val.x, home = mean_val.y) %>% 
  mutate(abs.diff = abs(abs(Control - warm1)/Control)) %>% 
  mutate(diff = (Control - warm1)/Control) %>% 
  mutate(comm.diff = abs(Control - dest)) %>% 
  mutate(closer = ifelse(abs(Control - home) < abs(Control -dest), "home", "dest")) %>% 
  mutate(toward = ifelse(abs(Control - dest) > abs(warm1 - dest), "yes", "no")) %>% 
  mutate(type = "warm")


#full data for cooling treatment, see notes for above code, this is the same but for cooling transplants
comm.data$comp <- rep(c("M", "A", " ", "L"), 14)
cool.data.all <- trait.data %>% 
  left_join(comm.data, by = c("origin" = "comp", "Trait" = "variable")) %>% 
  left_join(comm.data, by = c("origin" = "origin", "Trait" = "variable")) %>%
  select(origin, Taxon.x, Trait, rep.x, Control, warm1, cool1, mean_val.x, mean_val.y) %>% 
  rename(dest = mean_val.x, home = mean_val.y) %>% 
  mutate(abs.diff = abs(abs(Control - cool1)/Control)) %>% 
  mutate(diff = (Control - cool1)/Control) %>% 
  mutate(comm.diff = abs(Control - dest)) %>% 
  mutate(closer = ifelse(abs(Control - home) < abs(Control -dest), "home", "dest")) %>% 
  mutate(toward = ifelse(abs(Control - dest) > abs(cool1 - dest), "yes", "no")) %>% 
  mutate(type = "cool")

#combine the warm and cool data together and reorder factors for plotting.
comp.data.all <- rbind(warm.data.all, cool.data.all)
comp.data.all$origin <- as.factor(comp.data.all$origin)
comp.data.all$origin = factor(comp.data.all$origin, levels(comp.data.all$origin)[c(3, 4, 1, 2)])
comp.data.all$type <- as.factor(comp.data.all$type)
comp.data.all$type = factor(comp.data.all$type, levels(comp.data.all$type)[c(2, 1)])

#combine comparison data with trait distributions to determine whether transplant-related changes in traits are "significant" based on trait distributions from a species in it's home.
c.comp.data.all <- comp.data.all %>% 
  filter(Trait != "t.SLA" & Trait != "t.LA" & Trait != "t.P") %>% 
  left_join(cont_dist) %>% 
  filter(var > 0 & !is.na(var)) %>% 
  mutate(error = qt(0.995,df = n-1)*sqrt(var)/sqrt(n)) %>% 
  mutate(sig = ifelse(type == "warm", ifelse(abs(warm1 - Control) > error, "Y", "N"), ifelse(abs(cool1 - Control) > error, "Y", "N")))

#some exploratory plots
c.comp.data.all %>% 
  ggplot(aes(y = log(abs.diff), x = type)) +
  geom_boxplot() +
  facet_grid(~Trait, scales = "free")

c.comp.data.all %>% 
  ggplot(aes(x = Trait, y = abs.diff)) +
  geom_boxplot() 

c.comp.data.all %>% 
  ggplot(aes(x = log(comm.diff/Control), y = log(abs.diff), color = Taxon.x)) +
  geom_point() +
  facet_wrap(~Trait, scales = "free")

#look at mean plasticity for each trait
c.comp.data.all %>%
  group_by(Trait) %>% 
  summarize(mean.diff = mean(abs.diff, na.rm = T))


#change order and name of factors for plotting
p.comp.data.all <- c.comp.data.all %>% 
  mutate(Trait = plyr::mapvalues(Trait, from = c("mean.SLA", "mean.LDMC", "mean.LA", "mean.LT", "mean.N", "mean.C", "mean.P", "mean.CN", "mean.C13","mean.NP", "mean.N15"), to = c("SLA", "LDMC", "LA", "LT", "'%'*'N'", "'%'*'C'", "'%'*'P'", "'C'*':'*'N'", "paste(delta^13, 'C')", "'N'*':'*'P'", "paste(delta^15, 'N')"))) %>% 
  mutate(Trait = as.factor(Trait)) %>% 
  mutate(type = plyr::mapvalues(type, from = c("warm", "cool"), to = c("Warming", "Cooling"))) %>% 
  mutate(toward = as.factor(plyr::mapvalues(toward, from = c("yes", "no"), to = c("Converging", "Diverging")))) %>% 
  mutate(closer = plyr::mapvalues(closer, from = c("home", "dest"), to = c("Home", "Destination"))) %>% 
  mutate(all = "All") %>% 
  mutate(toward2 = as.factor(ifelse(sig == "N", "Not Significant", as.character(toward)))) %>% 
  mutate(closer2 = as.factor(ifelse(sig == "N", "Not Significant", as.character(closer))))

abs.diff.order <- p.comp.data.all %>% 
  group_by(Trait) %>% 
  summarize(mean = mean(log(abs.diff), na.rm = T)) %>% 
  arrange(-mean)

#Change order of factors
p.comp.data.all$Trait <- factor(p.comp.data.all$Trait,levels = as.vector(abs.diff.order$Trait))
p.comp.data.all$toward <- factor(p.comp.data.all$toward,levels(p.comp.data.all$toward)[c(2, 1)])
p.comp.data.all$toward2 <- factor(p.comp.data.all$toward2,levels(p.comp.data.all$toward2)[c(2, 3, 1)])

#### Figure 3 ####
p.comp.data.all %>% 
  ggplot(aes(y = log(abs.diff), x = type)) +
  geom_boxplot(aes(fill = type)) +
  facet_grid(~Trait, scales = "free", switch = "both", labeller = label_parsed) +
  theme_bw() +
  theme(text = element_text(size = 12),
        axis.text.x  = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        strip.text = element_text(size = 8)) +
  ylab("log(Relative Plasticity)") +
  scale_fill_discrete(name = "Transplant\nType",
                      labels = c("Warming", "Cooling")) +
  annotate("text", x = 1.5, y = 1.5, label = c("","*", "", "", "", "**", "", "", "**", "",""), size = 10)

#### Figure 4 ####
p.comp.data.all %>% 
  filter(!is.na(toward2)) %>% 
  ggplot(aes(x = type, fill = toward2)) +
  geom_bar(position = "fill") +
  coord_flip() +
  facet_wrap(~closer) +
  theme_bw() +
  labs(y = "Proportion") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        text = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        plot.title = element_text(size = 12),
        strip.text.y = element_text(size = 8, angle = 180)) +
  scale_fill_manual(values = c("yellowgreen", "gray90", "coral"), 
                    guide = guide_legend(reverse = T))

#### Analysis ####
#which traits are most plastic?
library(lmerTest)
plast <- lmer(log(abs.diff) ~ Trait*type + (1 |Taxon.x) + (1|origin), data = c.comp.data.all)
summary(plast)
rand(plast)

#Generate counts of comparisons for reporting
#first is the split between convering, diverging, and not significant
table(p.comp.data.all$toward2)

#second is the split between closer to home or closer to destination in control plots
table(p.comp.data.all$closer[!is.na(p.comp.data.all$toward)])

#test whether converging and diverging proportions are independent of transplant type and of distance from destination community mean
library(DescTools)

type <- GTest(p.comp.data.all$toward2, c.comp.data.all$type)
type
closer <- GTest(p.comp.data.all$toward2, c.comp.data.all$closer)
closer

#### Supplemental graph ####
#supplemental material graph, converging/diverging patterns by treatment type (cooling or warming) and whether individuals started closer to home trait values or destination trait values.

#the following 4 blocks of code generate tables to figure out the correct order in which to graph each type of situation
converging.wh <- p.comp.data.all %>% 
  group_by(Trait, closer, type) %>% 
  filter(!is.na(toward2)) %>% 
  summarize(n = length(toward2), con = sum(toward2 == "Converging"), prop = con/n) %>% 
  mutate(key = paste(closer, type, sep = "")) %>% 
  ungroup() %>% 
  select(-n, -con, -closer, -type) %>% 
  spread(key = key, value = prop) %>% 
  arrange(-HomeWarming)

converging.wd <- p.comp.data.all %>% 
  group_by(Trait, closer, type) %>% 
  filter(!is.na(toward2)) %>% 
  summarize(n = length(toward2), con = sum(toward2 == "Converging"), prop = con/n) %>% 
  mutate(key = paste(closer, type, sep = "")) %>% 
  ungroup() %>% 
  select(-n, -con, -closer, -type) %>% 
  spread(key = key, value = prop) %>% 
  arrange(-DestinationWarming)

converging.ch <- p.comp.data.all %>% 
  group_by(Trait, closer, type) %>% 
  filter(!is.na(toward2)) %>% 
  summarize(n = length(toward2), con = sum(toward2 == "Converging"), prop = con/n) %>% 
  mutate(key = paste(closer, type, sep = "")) %>% 
  ungroup() %>% 
  select(-n, -con, -closer, -type) %>% 
  spread(key = key, value = prop) %>% 
  arrange(-HomeCooling)

converging.cd <- p.comp.data.all %>% 
  group_by(Trait, closer, type) %>% 
  filter(!is.na(toward2)) %>% 
  summarize(n = length(toward2), con = sum(toward2 == "Converging"), prop = con/n) %>% 
  mutate(key = paste(closer, type, sep = "")) %>% 
  ungroup() %>% 
  select(-n, -con, -closer, -type) %>% 
  spread(key = key, value = prop) %>% 
  arrange(-DestinationCooling)

p.comp.data.all$c <- "c"

#The following blocks create 4 ggplots for each possible situation then combines them into one graph with appropriate labels.
p.comp.data.all$Trait <- factor(p.comp.data.all$Trait,levels = as.vector(converging.wh$Trait))
wh <- p.comp.data.all %>% 
  filter(type == "Warming" & closer == "Home", !is.na(toward2)) %>% 
  ggplot(aes(x = c, fill = toward2)) +
  geom_bar(position = "fill") +
  coord_flip() +
  facet_wrap(~Trait, labeller = label_parsed, ncol = 1, switch = "y") +
  ylab("Proportion") +
  xlab("Warming") +
  labs(title = "Home") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text.y = element_text(size = 8, angle = 180)) +
  scale_fill_manual(values = c("yellowgreen", "gray90", "coral"), 
                    guide = guide_legend(reverse = T))

p.comp.data.all$Trait <- factor(p.comp.data.all$Trait,levels = as.vector(converging.wd$Trait))
wd <- p.comp.data.all %>% 
  filter(type == "Warming" & closer == "Destination", !is.na(toward2)) %>% 
  ggplot(aes(x = c, fill = toward2)) +
  geom_bar(position = "fill") +
  coord_flip() +
  facet_wrap(~Trait, labeller = label_parsed, ncol = 1, switch = "y") +
  ylab("Proportion") +
  xlab("Transplant Type") +
  labs(title = "Destination") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 8),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text.y = element_text(size = 8, angle = 180)) +
  scale_fill_manual(values = c("yellowgreen", "gray90", "coral"), 
                    guide = guide_legend(reverse = T))

p.comp.data.all$Trait <- factor(p.comp.data.all$Trait,levels = as.vector(converging.ch$Trait))
ch <- p.comp.data.all %>% 
  filter(type == "Cooling" & closer == "Home", !is.na(toward2)) %>% 
  ggplot(aes(x = c, fill = toward2)) +
  geom_bar(position = "fill") +
  coord_flip() +
  facet_wrap(~Trait, labeller = label_parsed, ncol = 1, switch = "y") +
  ylab("Proportion") +
  xlab("Cooling") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 8, angle = 90),
        strip.text.y = element_text(size = 8, angle = 180)) +
  scale_fill_manual(values = c("yellowgreen", "gray90", "coral"), 
                    guide = guide_legend(reverse = T))

p.comp.data.all$Trait <- factor(p.comp.data.all$Trait,levels = as.vector(converging.cd$Trait))
cd <- p.comp.data.all %>% 
  filter(type == "Cooling" & closer == "Destination", !is.na(toward2)) %>% 
  ggplot(aes(x = c, fill = toward2)) +
  geom_bar(position = "fill") +
  coord_flip() +
  facet_wrap(~Trait, labeller = label_parsed, ncol = 1, switch = "y") +
  ylab("Proportion") +
  xlab("Transplant Type") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 8),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 8, angle = 90),
        strip.text.y = element_text(size = 8, angle = 180)) +
  scale_fill_manual(values = c("yellowgreen", "gray90", "coral"), 
                    guide = guide_legend(reverse = T))


library(ggpubr)
ggarrange(wh, wd, ch, cd, ncol = 2, nrow = 2, common.legend = TRUE, legend = "top", label.x = "Proportion")
