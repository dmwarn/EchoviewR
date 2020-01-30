#' Title
#'
#' @param file.list list of files
#' @param filesetName name of fileset
#' @param UpdateECS Logical stating whether or not to update the ECS file
#' @return nothing
#' @export
#'

UpdateDataPathAndECS <- function (file.list, filesetName = "Primary Fileset", UpdateECS = TRUE)
{
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

filesetName <- "Primary fileset"



EVAppObj <- COMCreate('EchoviewCom.EvApplication')

for (i in 1:length(file.list)) {
EVFile <- EVAppObj$OpenFile(file.list[i])
fileset <- EVFindFilesetByName(EVFile, filesetName)$fileset
EVFile[["Properties"]][["DataPaths"]]$Clear()

newdatafullpath <- paste0(newdatapath, "/", strsplit(basename(file.list[i]), split = "\\.")[[1]][1])

EVFile[["Properties"]][["DataPaths"]]$Add(newdatafullpath)
if(UpdateECS ==TRUE) {
  new.ecs.df <- data.frame(ecs.path = grep("Sturgeon|Steelhead|Baird|LTBB", all_ecs_files, 
                                           value = TRUE), ecs.basename = 
                             basename(grep("Sturgeon|Steelhead|Baird|LTBB", all_ecs_files, 
                                           value = TRUE)))
  ecs.file.orig <- basename(EVGetCalibrationFileName(EVFile = EVFile, filesetName = filesetName))
  ecs.file.new <- new.ecs.df$ecs.path[new.ecs.df$ecs.basename == ecs.file.orig ]
  
  fileset$SetCalibrationFile(ecs.file.new) 
}


try(EVSaveFile(EVFile = EVFile))

try(
  EVCloseFile(EVFile = EVFile) #close the EV file so you don't have a ton of EV files open.
)
}
QuitEchoview(EVAppObj)
}

