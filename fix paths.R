fileset.loc <- EVFindFilesetByName(EVFile, filesetName)$filesetObj #select the fileset COM obj for the named fileset
nbr.of.raw.in.fileset.pre <- fileset[["DataFiles"]]$Count()
raw.file.names <- 0 #create file.names object, will be overwritten with character vector of all the data file paths
# loop gets file.path for each DT4 file associated with the kth EV file.
for (j in 0:(nbr.of.raw.in.fileset.pre - 1)) {
  raw.file.names[j + 1] <- file.path(fileset.loc[["DataFiles"]]$Item(j)$FileName())   #Loop through the j raw data files
} # close inner loop
# grabbing their full names and creating a character vector of the full names
files <- data.frame(raw.file.names) # convert the character vector of file names for the kth EV file
files$EV.file <- file.list[k]
files$ECS.file <- EVGetCalibrationFileName(EVFile = EVFile, filesetName = filesetName) #Call of first function defined above
klist[[k]] <- files