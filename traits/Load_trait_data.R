#######################################################
###########   China Leaf Trait  2015      ##############
###########                               ##############
#######################################################
library(lubridate)

#####Data by Site
trait.site<- read.csv(file="traits/data/ChinaLeafTraitData20151102Site.csv", stringsAsFactors = FALSE)
trait.site$Taxon_TNRS_corrected <- gsub("\xa0", "", trait.site$Taxon_TNRS_corrected)
head(trait.site)
str(trait.site)

trait.site$Date <- ymd(trait.site$Date) # convert date
trait.site$Leaf_Thickness_Ave_mm <- rowMeans(trait.site[,c("Leaf_Thickness_1_mm", "Leaf_Thickness_2_mm", "Leaf_Thickness_3_mm","Leaf_Thickness_4_mm","Leaf_Thickness_5_mm","Leaf_Thickness_6_mm")], na.rm = TRUE) # calculate average leaf thickness




taxa <- dbGetQuery(con, "SELECT * FROM taxon")

sp.comparison <- plyr::ldply(unique(trait.site$Taxon_TNRS_corrected), function(x){
  code <- taxa$species[grep(x, taxa$speciesName, ignore.case = TRUE)]
  data.frame(traitName = x, commCode = ifelse(length(code) == 1, code, NA))
})

ggplot(data.frame(noccur = colSums(cover > 0), inTraits = names(cover) %in% sp.comparison$commCode), aes(x = inTraits, y = noccur)) + geom_boxplot()

names(cover) %in% sp.comparison$commCode
sum(!is.na(sp.comparison$commCode))
sum(is.na(sp.comparison$commCode))
sort(colSums(cover > 0))
