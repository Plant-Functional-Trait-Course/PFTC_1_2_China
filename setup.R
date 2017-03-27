####
#Run this script first to check required packages installed
#and to download data from dropbox
#don't forget to repeat the download occasionally
####

#Check recent version of R installed
if(getRversion() < "3.3.2") {
  stop("##########\nOld version of R\nPlease install latest version\n##########")
}

#Check recent version of Rstudio installed
if(RStudio.Version()$version < "1.0.0"){
  stop("##########\nOld version of Rstudio\nPlease install latest version\n##########")
}

#Check git is installed
if(Sys.which("git") == ""){
  warning("##########\ngit not installed\n##########", immediate. = TRUE)
}


#Check libraries installed
packages_needed <- read.table(header = TRUE, text = 
  "package CRAN github
   tidyverse TRUE NA #this includes dplyr, ggplot, tidyr etc
   rdrop2 TRUE NA
   vegan  TRUE NA
   RSQLite TRUE NA
   ggvegan FALSE gavinsimpson
   assertthat TRUE NA
   plyr TRUE NA
   devtools TRUE NA
   raster TRUE NA
   cowplot TRUE NA
   gridExtra TRUE NA
   rmarkdown TRUE NA
  ")

#check against currently installed packages
packages_needed <- packages_needed[!packages_needed$package %in% .packages(all.available = TRUE), , drop = FALSE]

#download missing CRAN packages
if(nrow(packages_needed[packages_needed$CRAN, ]) > 0){
  install.packages(packages_needed$package[packages_needed$CRAN])
}

#download missing github packages
pn <- packages_needed[!packages_needed$CRAN, , drop = FALSE]
if(nrow(pn) > 0){
  devtools::install_github(paste(pn$package, pn$github, sep = "/"))
}

#check all packages downloaded - if this line doesn't work - assertthat didn't install!
assertthat::assert_that(all(packages_needed$package %in% .packages(all.available = TRUE)))

#clean-up
rm(packages_needed, pn)

## install/sync data from dropbox
source("R/sync_from_dropbox.R")

#download community data - repeat this to check you have latest available data
sync_from_dropbox(drop_path_file = "transplant/communityDatabase/transplant.sqlite", 
                  local_path_file = "community/data/transplant.sqlite")


#download climate data
sync_from_dropbox(drop_path_file = "climate/climate.Rdata", 
                  local_path_file = "climate/climate.Rdata")

# monthly climate data
sync_from_dropbox(drop_path_file = "climate/climate_month.Rdata", 
                  local_path_file = "climate/climate_month.Rdata")