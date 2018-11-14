library("drake")
library("tidyverse")
library("vegan")
library("ggvegan")

pkgconfig::set_config("drake::strings_in_dots" = "literals")

source("trait_distributions/r_scripts/trait_ordinations_fun.R")

setting_plan <- drake_plan(
  treat_colours = c("black", "grey50", "pink", "lightblue", "red", "green", "orange")
  )


cwm_datasets_plan <- drake_plan(
  all_taxa = read_cwm("trait_distributions/data/China_pftc_cwm.rds"),
  no_graminoids = read_cwm("trait_distributions/data/China_pftc_cwm_nongraminoids.rds")
)


methods_plan <- drake_plan(
  AH = twoSites(dataset__, low = "A", high = "H", treat_colours = treat_colours),
  MA = twoSites(dataset__, low = "M", high = "A", treat_colours = treat_colours),
  LM = twoSites(dataset__, low = "L", high = "M", treat_colours = treat_colours),
  LH = twoSites(dataset__, low = "L", high = "H", treat_colours = treat_colours)
)

cwm_analyses <- evaluate_plan(
  methods_plan, 
  wildcard = "dataset__",
  values = cwm_datasets_plan$target
)

cwm_report <- drake_plan(
  report = rmarkdown::render(input = knitr_in("trait_distributions/Rmd/trait_ordinations.Rmd"))
)

my_plan <- bind_rows(cwm_report, cwm_datasets_plan, cwm_analyses, setting_plan)

conf <- drake_config(my_plan)
make(my_plan)

vis_drake_graph(conf, targets_only = TRUE)

system("evince trait_distributions/Rmd/trait_ordinations.pdf", wait = FALSE)

