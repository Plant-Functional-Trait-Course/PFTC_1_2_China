##ordinations##

#packages
library("vegan")
library("ggvegan")#devtools::install_github("gavinsimpson/ggvegan")

#load data
if(interactive()){
  source("community/start_here.R")
} else {
  source("start_here.R")
}

#ordination of controls
control_cover <- cover[cover_meta$TTtreat %in% c("control", "local"), ]
control_cover <- control_cover[, colSums(control_cover > 0) > 1]

#decorana
decorana(sqrt(control_cover)) # long gradient

# CA
CA <- cca(sqrt(control_cover))
plot(CA)

fortify(CA)
autoplot(CA)

fCA <- fortify(CA, display = "sites")
fCA <- cbind(fCA, cover_meta[cover_meta$TTtreat %in% c("control", "local"), ])

sites <- c("H", "A", "M", "L")
treat_colours <- c("black", "grey50", "pink", "lightblue", "red", "blue", "green")

g <-ggplot(fCA, aes(x = Dim1, y = Dim2, shape = originSiteID, colour = TTtreat, group = originPlotID, fill = TTtreat)) +
  geom_point(aes(size = ifelse(year == min(year), 2, 1))) +
  geom_path() + 
  coord_fixed(ratio = 1) +
  scale_size(range = c(1, 3), guide = "none") +
  scale_colour_manual(limits = levels(cover_meta$TTtreat), values = treat_colours) +
  scale_fill_manual(limits = levels(cover_meta$TTtreat), values = treat_colours) +
  scale_shape_manual(breaks = sites, limits = sites, values = c(24, 22, 23, 25)) +
  guides(shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(x = "CA1", y = "CA2", colour = "Treatment", fill = "Treatment", shape = "Site")
g
  
#Hogsete plots.

hogsete_plot <- function(site, base, ord = cca, dest = TRUE, OTC = TRUE, transplant = TRUE) {
  if(dest){
    use <- cover_meta$destSiteID == site
  } else {
    use <- cover_meta$originSiteID == site
  }
  if(!OTC) use <- use & cover_meta$TTtreat != "OTC"
  if(!transplant) use <- use & cover_meta$TTtreat %in% c("OTC", "control")
  S_cover <- cover[use,]
  S_cover <- S_cover[, colSums(S_cover > 0) > 1]
  decorana(sqrt(S_cover))
  
  # CA
  SCA <- ord(sqrt(S_cover))
  
  fSCA <- fortify(SCA, display = "sites")
  fSCA <- cbind(fSCA, cover_meta[use, ])
  
  base %+% fSCA
}

hogsete_plot(site = "L", base = g)
hogsete_plot(site = "M", base = g)
hogsete_plot(site = "A", base = g)
hogsete_plot(site = "H", base = g)  

hogsete_plot(site = "L", base = g, dest = FALSE)
hogsete_plot(site = "M", base = g, dest = FALSE)
hogsete_plot(site = "A", base = g, dest = FALSE)
hogsete_plot(site = "H", base = g, dest = FALSE)  

hogsete_plot(site = "L", base = g, transplant = FALSE)
hogsete_plot(site = "M", base = g, transplant = FALSE)
hogsete_plot(site = "A", base = g, transplant = FALSE)
hogsete_plot(site = "H", base = g, transplant = FALSE)  


#### ---- differentWarming

keep <- cover_meta$TTtreat %in% c("warm1", "control", "local", "OTC")
ord_comm <- cover[keep, ]
ord_comm <- ord_comm[, colSums(ord_comm > 0) > 1]

ord <- cca(ord_comm)
ford <- fortify(ord, display = "sites")
ford <- cbind(ford, cover_meta[keep, ])

g %+% ford
