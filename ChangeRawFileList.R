library(EchoviewR)
options(stringsAsFactors = FALSE)

the.ev.dir <- choose.dir(caption = "Choose the parent directory where EV file subdirectories are located")
newdatapath <-
  choose.dir(caption = "Choose folder where transect subfolders of DT4 files are located.")

all.evfiles <- list.files(the.ev.dir, pattern="EV|ev", full.names = TRUE,
                          recursive = TRUE)
scrap.files <- grep("evwx|evb|csv|evi|CAL|cal|evw|backup|template|GTB", all.evfiles, value=TRUE)

#now we have a list of all ev files.
evfiles <- setdiff(all.evfiles, scrap.files)

all_ecs_files <- list.files(the.ev.dir, pattern = "ecs$", recursive=TRUE,full.names = TRUE)

file.list <- evfiles

all_raw_files <-
  data.frame(raw.file.names = list.files(
    newdatapath,
    pattern = "dt4$|raw$",
    recursive = TRUE,
    full.names = TRUE
  ), basenam = basename(list.files(
    newdatapath,
    pattern = "dt4$|raw$",
    recursive = TRUE,
    full.names = TRUE
  )))


EVAppObj <- COMCreate('EchoviewCom.EvApplication')
#' The function for getting raw data file paths
ChangeRawFileList <- function (file.list, filesetName)
{
  # now a for loop to get raw data file list for each EV file in all_EV_files nested in a loop to
  # open each of the EV files in all_EV_files and then close each EV file
  
  # create an empty list in which to store the k data frames
  klist <- list()
  
  for (k in 1:length(file.list)) {
    try(
      EVFile <- EVAppObj$OpenFile(file.list[k]) #Selects the kth EV file in the list and opens it.
    )
    Sys.sleep(1)   #This slows R down to wait for Echoview.  Generally also fails without this.
    filesetName <- filesetName
    try(
      fileset.loc <- EVFindFilesetByName(EVFile, filesetName)$filesetObj #select the fileset COM obj for the named fileset
    )
    nbr.of.raw.in.fileset <- fileset.loc[["DataFiles"]]$Count() # identify # of raw data files in the kth EV file/fileset
    raw.file.names <- "c" #create file.names object, will be overwritten with character vector of all the data file paths
    # loop gets file.path for each DT4 file associated with the kth EV file.
    for (j in 0:(nbr.of.raw.in.fileset - 1)) {
      raw.file.names[j + 1] <- as.character(file.path(fileset.loc[["DataFiles"]]$Item(j)$FileName()))   #Loop through the j raw data files
    } # close inner loop
    # grabbing their full names and creating a character vector of the full names
    files <- data.frame(raw.file.names = raw.file.names, basenam = as.character(basename(raw.file.names)),
                        stringsAsFactors = FALSE) # convert the character vector of file names for the kth EV file
    files$EV.file <- file.list[k]
    klist[[k]] <- files
    fileset <- EVFindFilesetByName(EVFile, filesetName)$fileset
    fileset[["DataFiles"]]$RemoveAll()
    msgB <-
      paste(Sys.time(),
            ": Removed ",
            nbr.of.raw.in.fileset,
            " data files",
            sep = '')
    message(msgB)
    
    
    # now we add the removed data files from a different path
    dataFiles <-
      all_raw_files$raw.file.names[all_raw_files$basenam %in%
                               files$basenam]
    msgB <-
      paste(Sys.time(),
            ' : Adding data files to EV file ',
            uniqueTransect[i],
            sep = '')
    message(msgB)
    for (j in 1:length(dataFiles)) {
      # now we add the removed data files from a different path


      fileset[["DataFiles"]]$Add(dataFiles[j])
      
    }
    nbr.of.raw.in.fileset.post <- fileset[["DataFiles"]]$Count()
    if (nbr.of.raw.in.fileset.post - nbr.of.raw.in.fileset != 0) {
      msg  <-
        paste(Sys.time(),
              ' : Number of candidate to number of added file mismatch',
              sep = '')
      #msgV <- c(msgV, msg)
      warning(msg)
    }
    try(EVSaveFile(EVFile = EVFile))
    # Now change ecs file location

    new.ecs.df <- data.frame(ecs.path = grep("Sturgeon|Steelhead|Baird|LTBB", all_ecs_files, 
                         value = TRUE), ecs.basename = 
                  basename(grep("Sturgeon|Steelhead|Baird|LTBB", all_ecs_files, 
                                                                     value = TRUE)))
    

    
    ecs.file.orig <- basename(EVGetCalibrationFileName(EVFile = EVFile, filesetName = filesetName))
    ecs.file.new <- new.ecs.df$ecs.path[new.ecs.df$ecs.basename == ecs.file.orig ]
    SetCalibrationFile(string Name)
    fileset[["DataFiles"]]$Count()
    fileset$SetCalibrationFile(ecs.file.new)
    try(
      EVCloseFile(EVFile = EVFile) #close the EV file so you don't have a ton of EV files open.
    )
    Sys.sleep(1)
  } #Close outer loop
  
  # combine all of the k data frames into one big data frame
  #onebigdf <- dplyr::bind_rows(klist)
  
  #write.csv(onebigdf, 'LM_rawfiles.csv', row.names=FALSE )  #Save a csv file that includes the 
  # the full path of the raw data files for each EV file, the full name of that EV file, and the name of the ECS file used 
  # by each EV file.
} #End function

