library(EchoviewR)
library(dplyr)

filesetName <- "Primary fileset"
#Get list of dt4 files with full path....this path should be to the location of the folder containing subfolders that represent
#individual transect and the folder names are used as transect names
path <- choose.dir(caption="Input folder where transect subfolders of DT4 files are located.")
evpath <-
  choose.dir(caption = "Select folder where EV files are located")

all_dt4_files <- list.files(path, pattern = "dt4$", recursive=TRUE,full.names = TRUE)
all.evfiles <-
  list.files(
    evpath,
    pattern = "EV|ev",
    full.names = TRUE,
    recursive = TRUE
  )
scrap.files <-
  grep("evwx|evb|csv|evi|CAL|cal|evw|backup|template", all.evfiles, value = TRUE)
#now we have a list of all ev files.
evfiles <- setdiff(all.evfiles, scrap.files)
#split above so that you just have the name of the dt4 file
my.filenames <- data.frame(filename=matrix(unlist(all_dt4_files),byrow=T),stringsAsFactors=FALSE) #this creates 
#data.frame with filename as a field
my.filenames.split<-strsplit(my.filenames$filename, "/") # this creates a list of parts of the path

transects<- data.frame(transect=do.call("rbind", lapply(my.filenames.split, "[[", 2)), stringsAsFactors = FALSE)
#name as a field
tran.and.file<-data.frame(transect=as.character(transects$transect), filename=as.character(my.filenames$filename),
stringsAsFactors = FALSE)
trans.names<-unique(transects)

tran.and.file.w.nums <- left_join(tran.and.file, trans.names, by='transect') %>% group_by(transect)%>%
  summarise(N.files = n())
sum(tran.and.file.w.nums$N.files)
head(tran.and.file.w.nums)
uniqueTransect <- as.character(unique(tran.and.file$transect))
#####
#y19fix_for_sturgeon 
#uniqueTransect  <- uniqueTransect[11:14]
EVAppObj <- COMCreate('EchoviewCom.EvApplication')
i=12
for(i in 1:length(uniqueTransect)) {
  
  Sys.sleep(1) #this is present to slow R down to a speed Echoview can handle
  EVFile <- evfiles[i]
  try(EVFile <- EVAppObj$OpenFile(EVFile))
  filesetName <- filesetName
  fileset <- EVFindFilesetByName(EVFile, filesetName)$fileset
  #get number of raw data files currently in "fileset.name" fileset
  nbr.of.raw.in.fileset.pre <- fileset[["DataFiles"]]$Count()
  fileset.loc <- EVFindFilesetByName(EVFile, filesetName)$filesetObj #select the fileset COM obj for the named fileset
  nbr.of.raw.in.fileset.pre <- fileset[["DataFiles"]]$Count()
  raw.file.names <- 0 #create file.names object, will be overwritten with character vector of all the data file paths
  # loop gets file.path for each DT4 file associated with the kth EV file.
  for (j in 0:(nbr.of.raw.in.fileset.pre - 1)) {
    raw.file.names[j + 1] <- file.path(fileset.loc[["DataFiles"]]$Item(j)$FileName())   #Loop through the j raw data files
  } # close inner loop
  # grabbing their full names and creating a character vector of the full names
  files <- data.frame(raw.file.names = as.character(raw.file.names), 
      basenam =basename(as.character(files$raw.file.names)),stringsAsFactors = FALSE)

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
  
  # Now add the data files from a different location
  dataFiles <- as.character(tran.and.file$filename[tran.and.file$transect==strsplit(basename(evfiles[i]), "\\.")[[1]][1]])
  dataFiles <- as.character(files$raw.file.names[files$basenam == ])
  EVAddRawData(EVFile = EVFile, filesetName = filesetName, 
               dataFiles = dataFiles)
  for (j in 1:length(dataFiles)) {
  fileset[["DataFiles"]]$Add(dataFiles[j])
    msg <- paste(Sys.time(), ' : Adding ', dataFiles[i], ' to fileset name ', 
                 filesetName, sep = '')
    message(msg)
  }
  EVSaveFile(EVFile=EVFile)
  Sys.sleep(1)
  #get number of raw data files currently in "fileset.name" fileset
  nbr.of.raw.in.fileset.post <- fileset[["DataFiles"]]$Count()
  msgB <-
    paste(Sys.time(),
          ": New # of data files = ",
          nbr.of.raw.in.fileset.post,
          sep = '')
  message(msgB)
  diff <- nbr.of.raw.in.fileset.post - nbr.of.raw.in.fileset.pre
  msgC <-
    paste(Sys.time(),
          ": The difference in the # of data files is ",
          diff,
          sep = '')
  message(msgC)
  EVCloseFile(EVFile = EVFile)
  Sys.sleep(1)
}