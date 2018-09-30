
sync_from_dropbox <- function(drop_path_file, local_path_file, force = FALSE){
  
  if(!require("rdrop2")){
    stop("Need to install rdrop2 with devtools::install_github('karthik/rdrop2') or from CRAN")
  }
 # browser()
  drop_acc()# get account - will ask for login details
  #drop_auth()
  sdrop_path_file <- paste0("/", dirname(drop_path_file))
  
  if(file.exists(local_path_file)){
    chk_file <- paste0(local_path_file, ".Rdata")
    if(!file.exists(chk_file)){
      stop("Data file present but no file with synchronisation info/nrun with force = TRUE to force download")
    }
    load(chk_file)#load previously stored deltas
    d_delta <- drop_get_metadata(path = sdrop_path_file)#check if files changed
    changed <- dropbox_delta$content_hash != d_delta$content_hash
    if(isTRUE(changed)){#zero rows if no changes
      downloadneeded <- TRUE
    } else{#local copy up to date
      downloadneeded <- FALSE
      message("All files are up to date")
    }
  } else{#no local copy
    downloadneeded <- TRUE
  }
  
  
  if(downloadneeded || force){
    #check directory exits
    local_dir <- dirname(local_path_file)
    if(!dir.exists(local_dir)){
      dir.create(local_dir)
    }
    
    #download
    drop_download(path = drop_path_file, local_path = local_path_file, overwrite = TRUE)
    
    #get deltas so can check for updates later
    dropbox_delta <- drop_get_metadata(path = sdrop_path_file)
    save(dropbox_delta, file = paste0(local_path_file, ".Rdata"))
  }
}

#test

#sync_from_dropbox(drop_path = "test/", drop_file = "test.txt", local_path_file = "community/test.txt")
