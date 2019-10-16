#trait driver theory

library("drake")
library("tidyverse")
library("rjt.misc")
library("BIEN")
library("traitstrap")

#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

#set up parallel processing for drake
options(future.fork.enable = TRUE)
future::plan(future::multiprocess) 

#drake plan
trait_plan <- drake_plan(
  ##TODO - move import code to use database/csv files
  
  #import community
  community = get(load("trait_driver_analyses/data/Community.Rdata")), 
  
  #import trait data
  traits0 = get(load("trait_driver_analyses/data/traits.Rdata")),
  traits = traits0 %>% 
    select(-Full_Envelope_Name, -Envelope_Name_Corrected, -Date) %>% 
    gather(key = trait, value = value, -(Elevation:Leaf_number)) %>% 
    filter(!is.na(value)),
  #TODO clean impossible trait values using BIEN
  #calculate derived traits
  #transform
  
  #import environmental data
  env  = get(load("trait_driver_analyses/data/climate_month.Rdata")),
  
  #impute traits
  imputed_traits = trait_impute(comm = community, traits = traits, scale_hierarchy = c("Site", "Location"), taxon_col = "Taxon", value_col = "value", other_col = "Elevation"),

  #traits moments 
  bootstrapped_trait_moments  = trait_np_bootstrap(imputed_traits, nrep = 100)  
  
  #space/time R2 relationship
  
  #means
  
  #variance
  
  #skewness
  
  #kurtois
  
)

#configure and make drake plan
trait_config <- drake_config(plan = trait_plan, jobs = 2, parallelism = "future", keep_going = TRUE)
trait_config

