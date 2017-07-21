### CALCULATE LEAF AREA
library(LeafArea)


# Auds fantastic function
new.folder <- "~/Desktop/Temp"
list.of.files <- dir(path = paste0("~/Desktop/TestLeaf"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)

list.of.files <- ToDo$full[201:390]

loop.files <-  function(files){
  
  file.copy(files, new.folder)
  if(grepl("-NA$", files)){
    newfile <- basename(files)
    file.rename(paste0(new.folder, "/", newfile), paste0(new.folder,
                                                         "/", gsub("-NA$", "", newfile)))
  }
  print(files)
  area <- try(run.ij(set.directory = "~/Desktop/Temp", distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.005, save.image = TRUE))
  if(inherits(area, "try-error")){
    return(data.frame(File_Name =files, LeafArea = NA))
  }
  file.copy(dir(new.folder, full.names = TRUE, pattern = "\\.tif"), "~/Desktop/Output")
  Sys.sleep(0.1)
  if(any(!file.remove(dir(new.folder, full.names = TRUE) ))) stop()
  res <- data.frame(File_Name = names(unlist(area[[2]])), LeafArea = (unlist(area[[2]])))
  return(res)
}

LeafAreaSecond <- plyr::ldply(list.of.files, loop.files)

LeafAreaSecond_1 <- LeafAreaSecond
LeafAreaSecond_2 <- LeafAreaSecond
LeafAreaSecond_3 <- LeafAreaSecond

LeafArea2 <- rbind(LeafAreaSecond_1, LeafAreaSecond_2, LeafAreaSecond_3)
LeafArea2 <- as_tibble(LeafArea2)
write_csv(LeafArea2, "LeafArea2.csv")


# Sean leaf areas without loop

file.list.sean <- list.files(path = "C:/Users/cpo082/Desktop/leaf
data/SEAN_cropped")

sean_area <- run.ij (set.directory = "C:/Users/cpo082/Desktop/leaf
data/SEAN_cropped", distance.pixel = 237, known.distance = 2, log =
                       TRUE, save.image = TRUE, low.size = 0.05)

sean_cropped_LA_new <- data.frame(File_Name = names(unlist(sean_area
                                                           [[2]])), LeafArea = (unlist(sean_area[[2]])))

save(sean_cropped_LA_new, file = "C:/Users/cpo082/Desktop/leaf
data/sean_cropped_LA_new.Rdata")



# Auds fantastic function
current.folder <-
  "C:/Users/cpo082/Desktop/leaf_data/2015_scans/Elevation_leaf_images_without_folders_cleaned"
new.folder <- "C:/Users/cpo082/Desktop/leaf_data/2015_scans/temp"
list.of.files <- list.files(current.folder, full.names = TRUE)

loop.files <-  function(files){
  
  file.copy(files, new.folder)
  if(grepl("-NA$", files)){
    newfile <- basename(files)
    file.rename(paste0(new.folder, "/", newfile), paste0(new.folder,
                                                         "/", gsub("-NA$", "", newfile)))
  }
  area <- try(run.ij(set.directory =
                       "C:/Users/cpo082/Desktop/leaf_data/2015_scans/temp", distance.pixel =
                       237, known.distance = 2, log = TRUE, low.size = 0.005, save.image = TRUE))
  if(inherits(area, "try-error")){
    return(data.frame(File_Name =files, LeafArea = NA))
  }
  file.copy(dir(new.folder, full.names = TRUE, pattern = "\\.tif"),
            "C:/Users/cpo082/Desktop/leaf_data/2015_scans/output/")
  Sys.sleep(0.1)
  if(any(!file.remove(dir(new.folder, full.names = TRUE) ))) stop()
  res <- data.frame(File_Name = names(unlist(area[[2]])), LeafArea =
                      (unlist(area[[2]])))
  return(res)
}

Leaf.Area2015 <- plyr::ldply(list.of.files, loop.files)
