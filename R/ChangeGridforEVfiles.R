#' Modify acoustic variable grid height and distance for both
#' Sv and single target variables.
#'
#' @param EVFile 
#' EV file to be altered. In this function it will generally be one of many and the user
#' will have this entered by code.
#' @param SvacoVarName 
#' Character. The name for the Sv variable.
#' @param TSacoVarName 
#' Character. The name for the single target variable.
#' @param timeDistanceGridType (numeric) specifying the along track grid type. 0 = no grid, 1 = time (minutes), 2 = GPS distance (NMi), 3 = Vessel Log Distance (NMi), 4 = Pings, 5 = GPS distance (m), 6 = Vessel Log Distance (m). 
#' @param depthGridType (numeric) 0 = no grid, 1 = depth grid, 2 = use reference line.
#' @param timeDistanceGridDistance (numeric) vertical grid line spacing. Not needed if verticalType = 0. 
#' @param depthGridDistance (numeric) horizontal grid line spacing. Ignored if horizontalType = 0. 
#' @return
#' Modified EV files.
#' @export
#'

ChangeGridforEVfiles <-
  function(EVFile,
           SvacoVarName,
           TSacoVarName,
           timeDistanceGridType,
           depthGridType,
           timeDistanceGridDistance,
           depthGridDistance) {
    dat.dir <- paste0(choose.dir(), "\\EVfiles*")
    all.ev.dirs <- Sys.glob(file.path(dat.dir), dirmark = TRUE)
    all.evfiles <-
      list.files(all.ev.dirs, pattern = "EV|ev", full.names = TRUE)
    scrap.files <-
      grep("evwx|evb|csv|evi|CAL|evw|backup", all.evfiles, value = TRUE)
    
    #now we have a list of all ev files.
    evfiles <- setdiff(all.evfiles, scrap.files)
    EVAppObj <- COMCreate('EchoviewCom.EvApplication')
    for (i in 1:length(evfiles)) {
      try(EVFile <- EVAppObj$OpenFile(evfiles[i]))
      Sys.sleep(1)
      
      Sys.sleep(1)
      EVChangeVariableGrid(EVFile, SvacoVarName, timeDistanceGridType = 5,
                           depthGridType = 1, timeDistanceGridDistance = 3000,
                           depthGridDistance = 10, EVLineName = "surface", showenum = FALSE)
      Sys.sleep(1)
      EVChangeVariableGrid(EVFile, TSacoVarName, timeDistanceGridType = 5,
                           depthGridType = 1, timeDistanceGridDistance = 3000,
                           depthGridDistance = 10, EVLineName = "surface", showenum = FALSE)
      
      EVCloseFile(EVFile = EVFile)
    }
    QuitEchoview(EVAppObj)
  }