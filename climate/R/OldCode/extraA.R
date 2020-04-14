#read extra site A data (another site in the vicinity of site A)

#import data
#f <- "/Volumes/FELLES/MATNAT/BIO/Ecological and Environmental Change/TransplantChina/ClimateData/extraSiteA/climate data_3800m_YJG.xls"
f <- "climate/data/extraSiteA/climate data_3800m_YJG.xls"

col_names <-c("dateTime", "waterContent20", "Tsoil20", "waterContent10", "Tsoil10", "RH", "Tair25", "PAR")

extraA <- gdata::read.xls(f, sheet = 1, header = FALSE, skip = 4, stringsAsFactors = FALSE, na.strings = "#N/A!", comment = "")
names(extraA) <- col_names
extraA$dateTime <- as.POSIXct(extraA$dateTime, format = "%Y-%m-%d %I:%M %p", tz = "Asia/Shanghai")

#remove NA columns

extraA <- extraA[, !is.na(names(extraA))] 

##some plots - look OK (except for initial gap in +25 measurements)

ggplot(extraA, aes(x = dateTime, y = Tair25)) + geom_path() 
ggplot(extraA, aes(x = dateTime, y = RH)) + geom_path() 
ggplot(extraA, aes(x = dateTime, y = Tsoil20)) + geom_path() 
ggplot(extraA, aes(x = dateTime, y = Tsoil10)) + geom_path() 

ggplot(extraA, aes(x = dateTime, y = waterContent20)) + geom_path() 
ggplot(extraA, aes(x = dateTime, y = waterContent10)) + geom_path() 

save(extraA, file = "extraA_climate.Rdata")