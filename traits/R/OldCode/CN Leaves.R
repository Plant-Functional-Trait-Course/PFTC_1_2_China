library(readxl)
CNLeafs <- leafarea2015 <- read_csv(file = "CNAnalysis.csv")

Annjeanette <- read_excel(path = "ChinaLeafTraitData_senttogroup.xlsx")
Annjeanette <- Annjeanette %>% 
  select(Full_Envelope_Name, `stoich vial label`)

ddd <- CNLeafs %>% 
  left_join(Annjeanette, by = c("Full_Envelope_Name")) %>% 
  sort(`stoich vial label`)

write_csv(ddd, "CNAnalysis2.csv")
