#' Export Sv and TS distribution by region-cell from a folder of EV files.
#'
#' This function runs region-by cell exports of Sv and TS frequency distribution 
#' for EV files identified in a single folder selected by the user and exports them to 
#' the output directory selected by the user. Can of course be used to process
#' multiple folders of data with additional code.
#' @param dat.dir 
#' @param exp.dir 
#' @param SvacoVarName 
#' @param TSacoVarName 
#' @param regionClassName 
#' @return
#' This function generates csv files of Sv and TS frequency distribution data with the result 
#' being one of these files for each EV file processed. 
#' @keywords Echoview COM scripting
#' @export
#' @references \url{http://support.echoview.com/WebHelp/Echoview.htm/}

ExportSvTSdistRegionbyCell <-
  function(dat.dir,
           exp.dir,
           SvacoVarName,
           TSacoVarName,
           regionClassName) {
    dat.dir <- paste0(choose.dir(), "\\EVfiles*")
    all.ev.dirs <- Sys.glob(file.path(dat.dir), dirmark = TRUE)
    all.evfiles <-
      list.files(all.ev.dirs, pattern = "EV|ev", full.names = TRUE)
    scrap.files <-
      grep("evwx|evb|csv|evi|CAL|evw|backup", all.evfiles, value = TRUE)
    
    #now we have a list of all ev files.
    evfiles <- setdiff(all.evfiles, scrap.files)
    
    #This is the base path of the EchoNet2Fish folder where you want exports to go.
    exp.dir <- choose.dir()
    
    EVAppObj <- COMCreate('EchoviewCom.EvApplication')
    for (i in 1:length(evfiles)) {
      try(EVFile <- EVAppObj$OpenFile(evfiles[i]))
      Sys.sleep(1)
      EVname <- evfiles[i]
      exp.path <- strsplit(EVname, "[\\]")
      trans.name <- strsplit(basename(evfiles[i]), "[.]") 
      exp.fname.sv <-
        paste0(exp.dir,
               "\\",
               "MI",
               exp.path[[1]][5],
               "\\Sv\\",
               "Sv_",
               trans.name[[1]][[1]],
               ".csv")
      exp.fname.ts <-
        paste0(exp.dir,
               "\\",
               "MI",
               exp.path[[1]][5],
               "\\TS\\" ,
               "TS_",
               trans.name[[1]][[1]],
               ".csv")
      
      Sys.sleep(1)
      try (EVIntegrationByRegionsByCellsExport(
        EVFile,
        SvacoVarName,
        regionClassName,
        exportFn = exp.fname.sv,
        dataThreshold = NULL
      ))
      try(EVFreqDistRegionsByCellsExport(EVFile, TSacoVarName, regionClassName, exp.fname.ts))
      Sys.sleep(1)
      EVCloseFile(EVFile = EVFile)
    }
  }
