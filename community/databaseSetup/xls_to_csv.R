#packages
library("readxl")
library("assertthat")
library("dplyr")

#list of xls files
flist <- dir("community/databaseSetup/data", pattern = "*.xls$", full.names = TRUE, recursive = TRUE)

#what do we have
sapply(flist, excel_sheets)

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
taxonomy <- read_excel("community/databaseSetup/data/Full name and code.xlsx", sheet = "Sheet1")
taxonomy <- taxonomy[, names(taxonomy) != ""]#zap blank columns
taxonomy <- taxonomy %>% 
  filter(!is.na(fullName)) %>% # I hate excel
  select(oldCode, newCode)
                                
#import data, process and export to CSV
allsites <- plyr::ldply(flist, function(fl){
  print(fl)
  onesite <- plyr::ldply(excel_sheets(fl), function(sheet){
    print(sheet)
    dat <- read_excel(fl, sheet = sheet)

    #fix metadata
    dat <- dat[!is.na(dat$Measure), ]
    meta <- dat$DestinationSite #NB DestinationSite is the OriginSite!
    meta[is.na(meta)] <- meta[which(is.na(meta))-1]#fill blanks in meta with row above   
    dat$originPlotID <-meta
    dat$turfID <- meta
    dat$TTtreat <- substr(meta, 4, nchar(meta))
  
    

#    dat$destinationPlotID <- meta    

    #find destination site/block/plot
    sitetreat <-  gsub("[[:digit:]]-", "", meta) #remove block info
    destSitetreat <- plyr::mapvalues(sitetreat, from = originDestination$origin, to = originDestination$destination, warn_missing = FALSE)
    dat$destinationPlotID <- paste0(substr(destSitetreat, 1, 1), substr(meta,2, 3), dat$TTtreat)
    dat$DestinationSite <- substr(destSitetreat, 1, 1)
    dat$DestinationBlock <- substr(dat$destinationPlotID, 1, 2)#assuming transplant from block1 go to block 1
    

    #delete empty columns
    to_delete <- which(colSums(!is.na(dat)) == 0)
    to_delete <- to_delete[names(to_delete) != "comment"]
    dat <- dat[, -to_delete] # currently also removes GRtreat and RTtreat
    
    #year
    dat$year[is.na(dat$year)] <- dat$year[which(is.na(dat$year)) - 1]#fill blanks in year with row above
    #recorder
    dat$recorder[is.na(dat$recorder)] <- dat$recorder[which(is.na(dat$recorder)) - 1]#fill blanks in year with row above
    
    #taxonomy
    names(dat) <- trimws(names(dat))#zap trailing white space
    names(dat) <- make.names(names(dat))
    extras <- setdiff(names(dat), taxonomy$oldCode)
    taxonomy <- rbind(taxonomy, cbind(oldCode = extras, newCode = extras))
    names(dat) <- plyr::mapvalues(names(dat), from = taxonomy$oldCode, to = taxonomy$newCode, warn_missing = FALSE) 
  
    #deal with multiple columns
    multiple <- plyr::count(names(dat))
    multiple <- multiple$x[multiple$freq > 1]
    if(length(multiple) > 0) {
      sppX <- lapply(multiple, function(sp) {
        rowSums(dat[, names(dat) == sp], na.rm = TRUE)
      })
      sppX <- setNames(as.data.frame(sppX), multiple)
      dat <- cbind(dat[!names(dat) %in% multiple], sppX)
    }
    assert_that(all(table(names(dat)) == 1))#check no duplicates
    
    #assume correction file if no comments
    if(is.null(dat$comment)){
      dat$comment <- "correction"
    }
    
    #check date in date format
    if(class(dat$date)[1] == "numeric"){
      dat$date <- as.POSIXct(dat$date, origin = "1900-1-01")
    }
      
    dat
  })
  onesite
})

assert_that(!any(is.na(allsites$turfID)))# check for missing values

#sort columns so all species together
metaNames <- c("DestinationSite", "DestinationBlock", "originPlotID", "TTtreat", "RTtreat", "destinationPlotID", "turfID", "subPlot", "year", "date", "Measure", "recorder")

envNames <- c("moss", "lichen", "litter", "soil", "rock", "totalVascular", "totalBryophytes", "totalLichen", "vegetationHeight", "mossHeight", "litterThickness", "comment")

sppNames <- names(allsites)[!names(allsites) %in% c(metaNames, envNames)]
sppNames <- sort(sppNames)
sppNames

allsites <- allsites[,  c(metaNames, sppNames, envNames)]

names(allsites)

#check for odditities
allsites[, sppNames] %>% 
  tidyr::gather(key = spp, value = cover) %>%
  mutate(cover = as.character(cover)) %>% 
  filter(!is.na(cover)) %>% 
  mutate(cover = trimws(cover)) %>% 
  filter(!grepl("^\\d+(?:\\.\\d+)?$", cover))

#trim anywhite space
allsites <- plyr::colwise(trimws)(allsites)
#ensure numeric
allsites[, sppNames] <- plyr::colwise(as.numeric)(allsites[, sppNames])

##write data to csv file
write.csv(allsites, file = "community/databaseSetup/data/allsites.csv", row.names = FALSE)
