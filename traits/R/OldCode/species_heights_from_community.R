## load species heights from 2016 community data

library("tidyverse")
library("readxl")
library("assertthat")

originDestination <- read.table(header = TRUE, stringsAsFactors = FALSE, text = "
  origin, destination
                                HOTC HOTC
                                AOTC AOTC
                                MOTC MOTC
                                LOTC LOTC
                                
                                HC HC
                                AC AC
                                MC MC
                                LC LC
                                
                                HO HO
                                AO AO
                                MO MO
                                LO LO
                                
                                H1 A1
                                A1 M1
                                M1 L1
                                
                                A2 H2
                                M2 A2
                                L2 M2
                                
                                H3 L3
                                
                                L4 H4
                                ")  

#read taxonomy file
taxonomy <- readr::read_csv("community/databaseSetup/data/transplant_taxonomy.csv")
taxonomy <- taxonomy %>% 
  filter(!is.na(fullName)) # I hate excel

#sanity checks on taxonomy file
dups <- taxonomy %>% filter(is.na(keep)) %>% group_by(newCode) %>% filter(n() > 1) %>% arrange(newCode)
assert_that(nrow(dups) == 0)

taxonomy <- taxonomy %>% 
  select(oldCode, newCode)

#loop over files
#loop over sheets
#read sheet
#fill turfid down
#filter subsurf has H\\d
#select

files <- dir("community/databaseSetup/data/commXLSx/comm_2016/", pattern= "xlsx$", full.names = TRUE)

heights <- map_df(files, .f = function(fname){
  sheets <- excel_sheets(fname)
  map_df(sheets, .f = function(sheet){ print(paste(fname, sheet))
    dat <- read_excel(path = fname, sheet = sheet)  
    dat <- dat %>% 
      fill(DestinationSite, year) %>% 
      filter(grepl("^H\\d$", subPlot)) %>% 

      #fix metadata
      mutate(
        meta = DestinationSite, #NB DestinationSite is the OriginSite!
        originPlotID = meta,
        turfID = meta,
        TTtreat = substr(meta, 4, nchar(meta)),
    
        #find destination site/block/plot
        sitetreat =  gsub("[[:digit:]]-", "", meta), #remove block info
        destSitetreat = plyr::mapvalues(sitetreat, from = originDestination$origin, to = originDestination$destination, warn_missing = FALSE),
        destinationPlotID = paste0(substr(destSitetreat, 1, 1), substr(meta,2, 3), TTtreat),
        DestinationSite = substr(destSitetreat, 1, 1),
        DestinationBlock = substr(destinationPlotID, 1, 2)#assuming transplant from block1 go to block 1
      ) %>% 
      mutate(DestinationSite = factor(DestinationSite, levels = c("H", "A", "M", "L"))) %>% 
      select(-meta, -sitetreat, -destSitetreat) %>% 
      
      select(-DestinationBlock,	-originPlotID,	-destinationPlotID,	-RTtreat, -GRtreat, -date, -Measure, -recorder) %>%
      gather(key = taxon, value = height, -DestinationSite, -turfID, -TTtreat, -subPlot, -year) %>%
      filter(!is.na(height))
    dat
  })
})  

#fix taxonomy - step 1
heights <- heights %>% mutate(
  taxon = make.names(taxon),
  taxon = plyr::mapvalues(taxon, from = taxonomy$oldCode, to = taxonomy$newCode, warn_missing = FALSE) 
) %>% 
  filter(!taxon %in% c("litter", "moss", "lichen"))

#fix taxonomy step 2
global <- read_csv("community/databaseSetup/data/globalCorrections.csv")
taxonomy <- tbl(con, "taxon") %>% select(species, speciesName) %>% collect()
global <- global %>% mutate(
  new = trimws(new),
  #convert names to code
  old = plyr::mapvalues(old, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE),
  new = plyr::mapvalues(new, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE)
  
  )

heights <- heights %>% left_join(global,  by = c("taxon" = "old")) %>% 
  mutate(taxon = if_else(is.na(new), taxon, new)) %>% 
  select(-new)


#fix taxonomy step 3
local <- read_csv("community/databaseSetup/data/localDatacorrections_plots_China.csv")
local <- local %>% 
  filter(year == 2016, !is.na(new)) %>% 
  select(turfID, year, new, old) %>% 
  distinct(turfID, year, new, old)


heights <- heights %>% left_join(
  local %>% filter(year == 2016, !is.na(new)) %>% select(turfID, year, new, old),
  by = c("turfID" = "turfID", "year" = "year", "taxon" = "old")
) %>% 
  mutate(taxon = if_else(is.na(new), taxon, new)) %>% 
  select(-new)

test <- heights %>% left_join(cover_thin, by = c("turfID" = "turfID", "year" = "year", "taxon" = "species"))



ggplot(heights, aes(x = DestinationSite, y = height, fill = TTtreat)) +
  geom_violin()

heights %>% filter(height > 50)
