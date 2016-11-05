
library("readxl")
library("dplyr")
library("ggplot2")

#read excel file
biomass <- plyr::ldply(1:4, read_excel, path = "biomass/data/biomass2015.xls")
names(biomass) <- make.names(names(biomass))
head(biomass)
biomass$site <- factor(biomass$site, levels = c("H", "A", "M", "L"))
biomass <- biomass %>% 
  select(site, plot, species, H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,cover.,weight)

# make plots
ggplot(biomass, aes(x = cover., y = weight, color = species)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ site) +ylim(0, 100)

biomass[which.max(biomass$weight),]




# split authority from name
spNames <- strsplit(taxonomy$speciesName, " ")
nameAuthority <-ldply(spNames, function(x){
  if(any(grepl("var.", x, fixed = TRUE))){
    speciesName <- paste(x[1:4], collapse = " ")
    authority <- paste(x[-(1:4)], collapse = " ")
  } else {
    speciesName <- paste(x[1:2], collapse = " ")  
    authority <- paste(x[-(1:2)], collapse = " ")
  }
  if(is.na(authority)) authority <- ""
  data.frame(speciesName, authority, stringsAsFactors = FALSE)
})