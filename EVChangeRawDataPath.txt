#' Change the raw data file path for multiple EV files
#'
#' @param filesetName Character vector representing the name of the fileset
#' whose data files should be removed and then re-added. For this function
#' to complete successfully on multiple EV files, the filesetName must be
#' the same for each EV file.
#'
#' @return
#' Nothing is returned. The function is called for the effect of altering
#' the path to raw data files.
#' @export
#' @details 
#' This function uses folders identified by the user to remove all
#' raw data files in a list of EV files and re-add the same data 
#' files from a new location.
EVChangeRawDataPath <- function (filesetName) {
  datapath <-
    choose.dir(caption = "Choose folder where transect subfolders of DT4 files are located.")
  evpath <-
    choose.dir(caption = "Select folder where EV files are located")
  all_dt4_files <-
    list.files(
      datapath,
      pattern = "dt4$",
      recursive = TRUE,
      full.names = TRUE
    )
  #split above so that you just have the name of the dt4 file
  my.filenames <-
    data.frame(filename = matrix(unlist(all_dt4_files), byrow = T),
               stringsAsFactors = FALSE) #this creates
  #data.frame with filename as a field
  my.filenames.split <-
    strsplit(my.filenames$filename, "/") # this creates a list of parts of the path
  transects <-
    (data.frame(transect = do.call(
      "rbind", lapply(my.filenames.split, "[[", 2)
    ), stringsAsFactors = FALSE)) 
  transects$transect <- as.character(transects$transect)
  #this creates data.frame that has transect
  #name as a field
  tran.and.file <-
    data.frame(transect = as.character(transects$transect),
               filename = as.character(my.filenames$filename),
               stringsAsFactors = FALSE)
  trans.names <- unique(transects)
  all.evfiles <-
    list.files(
      evpath,
      pattern = "EV|ev",
      full.names = TRUE,
      recursive = TRUE
    )
  scrap.files <-
    grep("evwx|evb|csv|evi|CAL|cal|evw|backup", all.evfiles, value = TRUE)
  #now we have a list of all ev files.
  evfiles <- setdiff(all.evfiles, scrap.files)
  
  #uniqueTransect <- as.character(unique(tran.and.file$transect))
  i=12
  EVAppObj <- COMCreate('EchoviewCom.EvApplication')
  for (i in 1:length(evfiles)) {
    Sys.sleep(1)
    EVFile <- evfiles[i]
    try(EVFile <- EVAppObj$OpenFile(EVFile))
    filesetName <- filesetName
    fileset <- EVFindFilesetByName(EVFile, filesetName)$fileset
    #get number of raw data files currently in "fileset.name" fileset
    nbr.of.raw.in.fileset.pre <- fileset[["DataFiles"]]$Count()
    msgA <-
      paste(Sys.time(),
            ": Original # of data files = ",
            nbr.of.raw.in.fileset.pre,
            sep = '')
    message(msgA)
    
    fileset[["DataFiles"]]$RemoveAll()
    msgB <-
      paste(Sys.time(),
            ": Removed ",
            nbr.of.raw.in.fileset.pre,
            " data files",
            sep = '')
    message(msgB)
    
    # now we add the removed data files from a different path
    dataFiles <-
      tran.and.file$filename[tran.and.file$transect[i] ==
                                            evfiles[i]]
    
    msgB <-
      paste(Sys.time(),
            ' : Adding data files to EV file ',
            uniqueTransect[i],
            sep = '')
    message(msgB)
    for (j in 1:length(dataFiles)) {
      dataFiles <- subset(tran.and.file, tran.and.file$transect == transects[i])
        tran.and.file$filename[tran.and.file$transect[i] ==
                                 evfiles[i]]
      fileset[["DataFiles"]]$Add(dataFiles[j])
      
    }
    nbr.of.raw.in.fileset <- fileset[["DataFiles"]]$Count()
    if ((nbr.of.raw.in.fileset - nbr.of.raw.in.fileset.pre) != length(dataFiles)) {
      msg  <-
        paste(Sys.time(),
              ' : Number of candidate to number of added file mismatch',
              sep = '')
      #msgV <- c(msgV, msg)
      warning(msg)
    }
    EVSaveFile(EVFile = EVFile)
    Sys.sleep(1)
    EVCloseFile(EVFile = EVFile)
    Sys.sleep(1)
    
  }
  QuitEchoview(EVAppObj)
}
