## ToDo
#make delta test specific to one file
#test
# check behaviour if directory is missing
# remember user between sessions?

sync_from_dropbox <- function(drop_path, drop_file, local_path_file){
  
  if(!require("rdrop2")){
    stop("Need to install rdrop2 with devtools::install_github('karthik/rdrop2') or from CRAN")
  }
  browser()
  drop_acc()# get account - will ask for login details
  #drop_auth()
  
  if(file.exists(local_path_file)){
    load(paste0(local_path_file, ".Rdata"))#load previously stored deltas
    d_delta <- drop_delta(cursor = dropbox_delta$cursor, path_prefix = drop_path)#check if files changed
    if(nrow(d_delta$entries) > 0){#zero rows if no changes
      downloadneeded <- TRUE
    } else{#local copy up to date
      downloadneeded <- FALSE
    }
  } else{#no local copy
    downloadneeded <- TRUE
  }
  
  
  if(downloadneeded){
    drop_get(path = paste0(drop_path, drop_file) , local_file = local_path_file, overwrite = TRUE)
    
    #get deltas so can check for updates later
    dropbox_delta <- drop_delta(path_prefix = drop_path)
    save(dropbox_delta, file = paste0(local_path_file, ".Rdata"))
  }
}

sync_from_dropbox(drop_path = "test/", drop_file = "test.txt", local_path_file = "community/test.txt")
