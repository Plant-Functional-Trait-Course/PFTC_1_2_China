#trait driver theory

library("drake")
library("tidyverse")
library("rjt.misc")
library("BIEN", quietly = TRUE)
library("traitstrap")

#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

#set up parallel processing for drake
#options(future.fork.enable = TRUE)
future::plan(future::multiprocess) 

#drake plan
trait_plan <- drake_plan(
  ##TODO - move import code to use database/csv files
  
  #import community
  community = get(load("trait_driver_analyses/data/Community.Rdata")), 
  
  #import trait data
  traits0 = get(load("trait_driver_analyses/data/traits.Rdata")),
  
  traits = traits0 %>% 
    select(-Full_Envelope_Name, -Envelope_Name_Corrected, -Date, -Elevation, -P_FILE_NAME, -matches("Flag$"), -allComments, -FileName, -Taxon_written_on_envelopes, -CN_FILE_NAME , -StoichLabel, -P_Std_Dev, -P_Co_Var, -LeafID) %>% 
    select(-matches("Leaf_Thickness_\\d_mm")) %>% 
    pivot_longer(cols = -(Site:Leaf_number), names_to = "trait", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    mutate(Site = factor(Site, levels = levels(community$originSiteID))),
  #TODO clean impossible trait values using BIEN
  #calculate derived traits
  #transform
  
  #Histograms - shows need for cleaning
  trait_histograms = traits %>% 
    ggplot(aes(x = value, fill = Site)) + 
    geom_histogram() + 
    facet_wrap(~trait, scales = "free") +
    labs(title = "Clean me"),
  
  #import environmental data
  env  = get(load("trait_driver_analyses/data/climate_month.Rdata")),
  
  #impute traits for control and pre-transplant
  imputed_traits = {
    imputed_traits_home = community %>%
      filter(year == min(year) | TTtreat %in% c("control", "local", "OTC")) %>% 
      select(Site = originSiteID, Location = originBlockID, turfID, year, TTtreat, Taxon = speciesName, cover) %>% 
      trait_impute(traits = traits, scale_hierarchy = c("Site", "Location"), taxon_col = "Taxon", value_col = "value", abundance_col = "cover", other_col = c("TTtreat", "year", "turfID"))
      
    imputed_traits_transplant = community %>%
      filter(year > min(year), !TTtreat %in% c("control", "local", "OTC")) %>%
      select(Site = destSiteID, Location = destBlockID, turfID, year, TTtreat, Taxon = speciesName, cover) %>% 
      trait_impute(traits = traits, scale_hierarchy = c("Site", "Location"), taxon_col = "Taxon", value_col = "value", abundance_col = "cover", other_col = c("year", "TTtreat", "turfID"))
    
    
    x <- bind_rows(imputed_traits_home, imputed_traits_transplant)
    attributes(x) <- attributes(imputed_traits_home)
    x
  },

  #traits moments 
  bootstrapped_trait_moments  = trait_np_bootstrap(imputed_traits, nrep = 100),  
  
  #space/time R2 relationship
  
  #means
  mean_plot = bootstrapped_trait_moments %>% ggplot(aes(x = Site, y = mean)) +
    geom_boxplot() +
    facet_wrap(~trait, scales = "free_y"),
  
  #bootstrapped_trait_moments %>% filter(trait == "Leaf_Thickness_Ave_mm") %>% 
  #  ggplot(aes(x = year, y = value, fill = TTtreat)) + facet_wrap(~Site)
  
  #variance
  
  #skewness
  
  #kurtois
  
  
  moments_plot = bootstrapped_trait_moments %>% ggplot(aes(x = Site, y = mean)) +
    geom_boxplot() +
    facet_wrap(~trait, scales = "free_y")
  
)

#configure and make drake plan
trait_config <- drake_config(plan = trait_plan, jobs = 2, parallelism = "future", keep_going = TRUE)
trait_config

