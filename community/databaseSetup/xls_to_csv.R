#packages
library("readxl")
library("plyr")

#list of xls files
flist <- dir("community/databaseSetup/data/", pattern = "*.xls$", full.names = TRUE)

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
taxonomy <- taxonomy[, c("oldCode", "newCode")]
                                
#import data, process and export to CSV
allsites <- lapply(flist, function(fl){
  print(fl)
  onesite <- lapply(excel_sheets(fl), function(sheet){
    print(sheet)
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
    originSitetreat <- mapvalues(sitetreat, from = originDestination$destination, to = originDestination$origin, warn_missing = FALSE)
    dat$originPlotID <- paste0(substr(originSitetreat, 1, 1), substr(meta,2, 3), substr(originSitetreat, 2, nchar(originSitetreat)))

    #delete empty columns
    dat <- dat[, colSums(!is.na(dat))>0] # currently also removes GRtreat and RTtreat
    
    #taxonomy
    
    names(dat) <- trimws(names(dat))#zap trailing white space
    names(dat) <- make.names(names(dat))
    extras <- setdiff(names(dat), taxonomy$oldCode)
    taxonomy <- rbind(taxonomy, cbind(oldCode = extras, newCode = extras))
    names(dat) <- mapvalues(names(dat), from = taxonomy$oldCode, to = taxonomy$newCode, warn_missing = FALSE) 
    
    #deal with multiple columns
    multiple <- count(names(dat))
    multiple <- multiple$x[multiple$freq > 1]
    if(length(multiple) > 0) {
      sppX <- lapply(multiple, function(sp) {
        rowSums(dat[, names(dat) == sp], na.rm = TRUE)
      })
      sppX <- setNames(as.data.frame(sppX), multiple)
      dat <- cbind(dat[!names(dat) %in% multiple], sppX)
    }
    stopifnot(all(table(names(dat)) == 1))
    dat
  })
  do.call(rioja::Merge, args = onesite)
  
})
allsites <- do.call(rioja::Merge, args = allsites)


#sort columns so all species together
metaNames <- c("DestinationSite", "DestinationBlock", "originPlotID", "TTtreat", "destinationPlotID", "turfID", "subPlot", "year", "date", "Measure", "recorder")

envNames <- c("moss", "lichen", "litter", "soil", "rock", "totalVascular", "totalBryophytes", "totalLichen", "vegetationHeight", "mossHeight", "litterThickness", "comment")

sppNames <- names(allsites)[!names(allsites) %in% c(metaNames, envNames)]
sppNames <- sort(sppNames)
sppNames

allsites <- allsites[,  c(metaNames, sppNames, envNames)]

names(allsites)

##write data to csv file
write.csv(allsites, file = "community/databaseSetup/data/allsites.csv", row.names = FALSE)
