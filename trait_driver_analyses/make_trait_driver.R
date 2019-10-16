#load packages
library(drake)


#Build the right things
r_make(source = "trait_driver_analyses/trait_driver_drake.R")
failed()

#view dependency graph
r_vis_drake_graph(source = "trait_driver_analyses/trait_driver_drake.R", targets_only = TRUE)
