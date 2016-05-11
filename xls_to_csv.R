#packages
library("readxl")
library("plyr")

#list of xls files
flist <- dir("./data", pattern = "*.xls", full.names = TRUE)

#what do we have
sapply(flist, excel_sheets)

originDestination <- read.table(header = TRUE, sep = ",", stringsAsFactors = FALSE, text = "
  origin, destination
  HOTC, HOTC
  AOTC, AOTC
  MOTC, MOTC
  LOTC, LOTC

  HC, HC
  AC, AC
  MC, MC
  LC, LC

  HO, HO
  AO, AO
  MO, MO
  LO, LO

  H1, A1
  A1, M1
  M1, L1

  A2, H2
  M2, A2
  L2, M2

  H3, L3

  L4, H4
")  
                                
                                
#import data, process and export to CSV
allsites <- lapply(flist, function(fl){
  onesite <- lapply(excel_sheets(fl), function(sheet){
    dat <- read_excel(fl, sheet = sheet)

    #fix metadata
    dat <- dat[!is.na(dat$Measure), ]
    meta <- dat$DestinationSite
    meta[is.na(meta)] <- meta[which(is.na(meta))-1]#fill blanks in meta with row above
    dat$DestinationSite <- substr(meta, 1, 1)
    dat$DestinationBlock <- substr(meta, 1, 2)
    dat$TTtreat <- substr(meta, 4, nchar(meta))
    dat$destinationPlotID <- meta    
    dat$turfID <- meta

    #find originplot
    sitetreat <-  gsub("[[:digit:]]-", "",meta) #remove block info
    originSitetreat <- mapvalues(sitetreat, from = originDestination$destination, to = originDestination$origin, warn_missing = FALSE )
    dat$originPlotID <- paste0(substr(originSitetreat, 1, 1), substr(meta,2, 3), substr(originSitetreat, 2, nchar(originSitetreat)))

    names(dat) <- trimws(names(dat))#zap trailing white space
    
    dat
  })
  do.call(rioja::Merge, args = onesite)
  
})
allsites <- do.call(rioja::Merge, args = allsites)

originalNames <- names(allsites)
#make save names
names(allsites) <- make.names(names(allsites)) #make legal names

#sort columns so all species together
metaNames <- c("DestinationSite", "DestinationBlock", "originPlotID", "TTtreat", "destinationPlotID", "turfID", "RTtreat", "GRtreat",           "subPlot", "year", "date", "Measure", "recorder")

envNames <- c("moss", "lichen", "litter", "soil", "rock", "totalVascular", "totalBryophytes", "totalLichen", "vegetationHeight", "mossHeight", "litterThickness", "comment")

sppNames <- names(allsites)[!names(allsites) %in% c(metaNames, envNames)]
sppNames <- sort(sppNames)
sppNames

allsites <- allsites[,  c(metaNames, sppNames, envNames)]


names(allsites)

##write data to csv file
