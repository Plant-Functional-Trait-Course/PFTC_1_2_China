#packages
library("readxl")

#list of xls files
flist <- dir("./data", pattern = "*.xls", full.names = TRUE)

#what do we have
sapply(flist, excel_sheets)

originDestination <- read.table(header = TRUE, sep = ",", text = "
origin, destination
HOTC, HOTC
AOTC, AOTC
MOTC, MOTC
LOTC, LOTC

HC, HC
AC, AC
MC, MC
LC, LC

H1,X

H2,X

H3,X

H4,X

")  
                                
                                
                    


#import data, process and export to CSV
lapply(flist, function(fl){
  lapply(excel_sheets(fl), function(sheet){
    dat <- read_excel(fl, sheet = sheet)
    #testonly
      dat <- read_excel(flist[1], sheet = excel_sheets(flist[1])[1])#test only
    #testonly
    dat <- dat[!is.na(dat$Measure), ]
    meta <- dat$DestinationSite
    meta[is.na(meta)] <- meta[which(is.na(meta))-1]#fill blanks in meta with row above
    dat$DestinationSite <- substr(meta, 1, 1)
    dat$DestinationBlock <- substr(meta, 1, 2)
    dat$originPlotID <- mapvalues()
    dat$TTtreat <- substr(meta, 4, nchar(meta))
    dat$destinationPlotID <- meta
  })

  
})