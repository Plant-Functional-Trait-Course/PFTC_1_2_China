##ordinations##

#packages
library("vegan")
library("ggvegan")#devtools::install_github("gavinsimpson/ggvegan")

#load data
source("community/start_here.R")

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
?ggvegan::autoplot.cca
fCA <- fortify(CA, display = "sites")
fCA <- cbind(fCA, cover_meta[cover_meta$TTtreat %in% c("control", "local"), ])

ggplot(fCA, aes(x = Dim1, y = Dim2, col = originSiteID, shape = TTtreat, group = originPlotID, linetype = TTtreat)) +
  geom_point() +
  geom_line() + 
  coord_fixed(ratio = 1) +
  labs(x = "CA1", y = "CA2", colour = "Site", shape = "Treatment", linetype = "Treatment")


  