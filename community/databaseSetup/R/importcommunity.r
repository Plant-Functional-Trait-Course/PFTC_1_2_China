####################
#todo############
#check species in taxon table before inserting
#merge subturf taxa
####################

import.data<-function(dat, mergedictionary, flags){#dat is data.frame from the correctly formatted csv file loaded into R
  require("assertthat")

    dat <- dat[!is.na(dat$originPlotID),]
    head(dat)
    names(dat)

  print(max(nchar(as.character(dat$comment)))) #how long is longest comment)
    
    #extract turf data
    turf <- dat[,c("turfID", "TTtreat", "originPlotID", "destinationPlotID")]
    turf <- unique(turf)
    turf$TTtreat <- trimws(turf$TTtreat) #  trim white spaces
  
    turf
    names(turf)
    
    alreadyIn <- dbGetQuery(con,"select turfID from turfs")$turfID
    newTurfs <- turf[!as.character(turf$turfID) %in% alreadyIn,] #find which turfs IDs are not already in database
    
    if(nrow(newTurfs) > 0) {
      dbPadWriteTable(con, "turfs", newTurfs)
    }
    nrow(turf)
    nrow(newTurfs)
    
    message("done turfs")                                  
    
    #subTurf env
    subturfEnv <- dat %>% 
      filter(Measure != "cover%", comment != "Yans correction") %>% 
      select(turfID, subTurf = subPlot, year, moss, lichen, litter, soil, rock, comment) %>%
      mutate(subTurf = as.numeric(subTurf)) %>%
      filter(is.na(comment) | comment != "correction")
            
    assert_that(all(dat$turfID %in% subturfEnv$turfID))
             
      if(!is.null(dat$missing)){
         bad = dat$missing[dat$Measure != "cover%"]
         bad[is.na(bad)] <- ""
        subturfEnv$bad <- bad
      } else{
        subturfEnv$bad <-  ""    
      }
    subturfEnv 
    dbPadWriteTable(con, "subTurfEnvironment", subturfEnv)
    nrow(subturfEnv)
    
    #TurfEnv    
    turfEnv <- dat %>% 
    filter(Measure == "cover%", comment != "Yans correction") %>% 
      select(turfID, year, totalVascular, vegetationHeight, mossHeight, comment, recorder, date) %>%
      filter(is.na(comment) | comment != "correction") %>% 
      # fix unrealistic outliers
      mutate(mossHeight = case_when(turfID == "A4-OTC" & year == 2014 ~ 3.7,
                                    turfID == "H7-1" & year == 2014 ~ 2.1,
                                    turfID == "L7-4" & year == 2013 ~ 1.2,
                                    turfID == "L2-2" & year == 2013 ~ 1.7,
                                    TRUE ~ mossHeight))

    assert_that(!any(nchar(as.character(turfEnv$comment[!is.na(turfEnv$comment)])) > 255))
    dbPadWriteTable(con, "turfEnvironment", turfEnv)
  nrow(turfEnv)   
  
    #TurfCommunity  
  spp <- dat %>% 
    as_tibble() %>% 
    filter(Measure == "cover%") %>% 
    select(turfID, year, (which(names(dat) == "recorder") + 1) : (which(names (dat) == "moss") - 1), comment) %>% 
    gather(key = species, value = cover, -turfID, -year, -comment) %>% 
    filter(!is.na(cover)) %>% #remove absent taxa
    mutate(cf = grepl("cf", cover, ignore.case = TRUE),
           cover = gsub("cf", "", cover, ignore.case = TRUE) #move any CF to new column
           ) 
    
  #oddity search
  spp %>% filter(is.na(as.numeric(cover)))  %>% count(cover)
  
  
mergedictionary  <- spp %>% 
  distinct(species) %>% 
  select(oldID = species) %>% 
  anti_join(mergedictionary) %>% #get any taxa not in mergedictionary
  mutate(newID = oldID) %>% 
  bind_rows(mergedictionary)



#merge synonyms
spp <- spp %>% 
  left_join(mergedictionary, by = c("species" = "oldID")) %>% 
  select(turfID, year, comment, species = newID, cover) %>% 
  group_by(year, turfID, comment, species) %>% 
  mutate(cover = as.numeric(cover)) %>% 
  summarise(cover = sum(cover)) %>% #aggregate taxa
  filter(cover > 0) %>% 
  ungroup()

#flag Yans corrections
spp <- spp  %>% 
  left_join(flags %>% 
              filter(subPlot == "cover%") %>% 
              select(-subPlot)
            ) %>% 
  mutate(
    flag = if_else(comment == "Yans correction", "Yans imputation", flag ),
    flag = if_else(is.na(flag), "", flag)
    ) %>% 
  select(-comment)

  #inject
  initNrowTurfCommunity <- dbGetQuery(con, "select count(*) as n from turfCommunity")
  dbPadWriteTable(con, "turfCommunity", spp)
  finalNrowTurfCommunity <- dbGetQuery(con, "select count(*) as n from turfCommunity")
  
  #check correct number rows
  assert_that(nrow(spp) == finalNrowTurfCommunity - initNrowTurfCommunity)

  
  #subTurfCommunity  
  message("subturfcommunity")  
  subspp <- dat %>% 
    as_tibble() %>% 
    filter(dat$Measure != "cover%") %>% 
    select(turfID, year, subTurf = subPlot, (which(names(dat) == "recorder") + 1) : (which(names(dat) == "moss") -1), comment) %>%
    mutate(subTurf = as.integer(subTurf)) %>% 
    gather(key = species, value = presence, -turfID, -year, -subTurf, -comment) %>% 
    filter(!is.na(presence)) %>% 
    filter(presence != 0) %>% #for seedclim use code similar to turf community code above for cf to pull of adult etc. Do it before the merge
    ungroup()

  #Find oddities in dataset
  subspp %>% count(presence)

  #merge synonyms
  subspp <- subspp %>% 
    left_join(mergedictionary, by = c("species" = "oldID")) %>% 
    select(turfID, year, subTurf, comment, species = newID, presence) %>% 
    group_by(year, turfID, subTurf, comment, species) %>% 
    summarise(adult = sign(sum(presence))) %>% #will be much more complex for seedclim
    filter(adult > 0) %>% 
    ungroup()
  
  #flag Yans corrections
  subspp <- subspp %>%
    left_join(
      flags %>%
        filter(subPlot != "cover%") %>% 
        rename(subTurf = subPlot) %>% 
        mutate(subTurf = as.numeric(subTurf))
      ) %>% 
    mutate(flag = if_else(comment == "Yans correction", "Yans imputation", flag ),
           flag = if_else(is.na(flag), "", flag)
           ) %>% 
    select(-comment)
  
  #inject
    initNrowSTurfCommunity <- dbGetQuery(con, "select count(*) as n from subTurfCommunity")
    dbPadWriteTable(con, "subTurfCommunity", subspp)
    finalNrowSTurfCommunity <- dbGetQuery(con, "select count(*) as n from subTurfCommunity")
    
    #check correct number rows
    assert_that(nrow(subspp) == finalNrowSTurfCommunity - initNrowSTurfCommunity)

}


# Codes for deleting tables:
wipe <- function(){
  dbGetQuery(con, "Delete FROM subTurfCommunity")                            
  dbGetQuery(con, "Delete FROM subTurfEnvironment")
  dbGetQuery(con, "Delete FROM turfCommunity")
  dbGetQuery(con, "Delete FROM turfEnvironment")
   #dbGetQuery(con, "Delete * FROM turfs")
  message("Database wiped. Hope you really wanted to do that!")
}

 
#replace mytable with a table you want to clean
#duplicate lines as necessary to clean all tables
#delete tables in the correct order or it won't work
#Then just run wipe() to clean the database
