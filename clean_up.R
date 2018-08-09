clean_script <- function(htmls){
  fdir = "temp"
  
  allscripts = lapply(list.files("temp",pattern="script.*js",full.names = T),readLines)
  
  names(allscripts) <- list.files("temp",pattern="script.*js")
  
  for (afile in htmls){
    
    # print(fdir)
    tmp <- readLines(afile)
    # print(tmp)
    # get ids
    idx_start <-grep("<script>",tmp)
    # print(idx_start)LS
    
    idx_end <- grep("</script>",tmp)
    idx_end2 <- sapply(idx_start, function(x) {min(idx_end[which(idx_end>= x)])})
    
    if (length(idx_start) != length(idx_end2)) next;
    
    for (i in 1:length(idx_start)){
      # print(idx_start[i],idx_end2[i])
      tmpscript <- tmp[idx_start[i]:idx_end2[i]]
      
      tmp_dummy <- character(length=length(tmpscript))
      
      id_found = -1
      
      if (length(allscripts) > 0){
        for (j in 1:length(allscripts)){
          if (paste0(tmpscript,collapse = "") == paste0(allscripts[[j]],collapse = "")){
            id_found = j
            break;
          }
        }
      }
      
      ## update html file
      
      if (id_found > 0) {
        tmp_dummy[1] <- paste0("<script src='script-",id_found,".js'></script>")
      }
      else {
        tmp_dummy[1] <- paste0("<script src='script-",length(allscripts)+1,".js'></script>")
        allscripts[[length(allscripts)+1]] <- tmpscript;
      }
      
      tmp[idx_start[i]:idx_end2[i]] <- tmp_dummy
      
      
    }
    writeLines(tmp,paste0(fdir,"/",basename(afile)))
  }
  
 
  
  # write scripts
  if (length(allscripts) >0){
    for (i in 1:length(allscripts)){
      writeLines(allscripts[[i]], paste0(fdir,"/script-",i,".js"))
    }
    
  }

}



clean_css <- function(htmls){
  fdir = "temp"
  
  allscripts = lapply(list.files("temp",pattern="style-.*css",full.names = T),readLines)
  
  names(allscripts) <- list.files("temp",pattern="style-.*css")
  
  for (afile in htmls){
    
    tmp <- readLines(afile)
    # get ids
    idx_start <-grep('<style type.*=.*"text/css">',tmp)
    idx_end <- grep('</style>',tmp)
    idx_end2 <- sapply(idx_start, function(x) {min(idx_end[which(idx_end>= x)])})
    
    for (i in 1:length(idx_start)){
      
      tmpscript <- tmp[idx_start[i]:idx_end2[i]]
      tmp_dummy <- character(length=length(tmpscript))
      
      id_found = -1
      
      if (length(allscripts) > 0){
        for (j in 1:length(allscripts)){
          if (paste0(tmpscript,collapse = "") == paste0(allscripts[[j]],collapse = "")){
            id_found = j
            break;
          }
        }
      }
      
      ## update html file
      
      if (id_found > 0) {
        tmp_dummy[1]<- paste0("<link rel='stylesheet' href='style-",id_found,".css' />")
        # tmp_dummy[1] <- paste0("<script src='script-",id_found,".js'></script>")
      }
      else {
        # tmp_dummy[1] <- paste0("<script src='script-",length(allscripts)+1,".js'></script>")
        tmp_dummy[1]<- paste0("<link rel='stylesheet' href='style-",length(allscripts)+1,".css' />")
        allscripts[[length(allscripts)+1]] <- tmpscript;
      }
      
      tmp[idx_start[i]:idx_end2[i]] <- tmp_dummy
      
      
    }
    writeLines(tmp,paste0(fdir,"/",basename(afile)))
    
  }
  
  
  
  # write scripts
  if (length(allscripts) > 0){
    for (i in 1:length(allscripts)){
      writeLines(allscripts[[i]], paste0(fdir,"/style-",i,".css"))
    }
    
  }

}

# main

if (file.exists("./temp")){
  system("rm -rf temp")
}

dir.create("./temp")

htmls <- list.files(pattern= "*.html",full.names = T)



 
clean_script(htmls)

clean_css(htmls)