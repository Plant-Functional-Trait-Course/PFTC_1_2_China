
sync_from_dropbox <- function(drop_path, drop_file, local_path){
  
  if(!require("rdrop2")){
    message("Install rdrop2 with devtools::install_github('karthik/rdrop2') or from CRAN")
    return()
  }
  browser()
  drop_acc()# get account - will ask for login details
  #drop_auth()
  
  
  
  
  #download
  local_path <- "community/data/transplant.sqlite"
  drop_path <-"/transplant/community/data"
  
  if(file.exists(local_path)){
    load(paste0(local_path, ".Rdata"))#load previously stored deltas
    d_delta <- drop_delta(cursor = dropbox_delta$cursor, path_prefix = drop_path)#check if files changed
    if(nrow(d_delta$entries) > 0){#zero rows if no change
      downloadneeded <- TRUE
    } else{
      downloadneeded <- FALSE
    }
  } else{
    downloadneeded <- TRUE
  }
  
  
  if(needdownload){
    drop_get(path = paste0(drop_path, drop_file) , local_file = local_path, overwrite = TRUE)
    
    #get deltas so can check for updates later
    dropbox_delta <- drop_delta(path_prefix = drop_path)
    save(dropbox_delta, file = paste0(local_path, ".Rdata"))
  }
}

sync_from_dropbox(drop_path = "test/", drop_file = "test.txt", local_path = "community/test.txt")
