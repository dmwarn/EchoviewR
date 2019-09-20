#' Function to export TS frequency distribution.
#' 
#' 
#' This function runs region-by cell exports of TS frequency distribution 
#' for EV files identified in the data directory selected and exports them to 
#' the output directory selected. Can be called on its own but is also used in 
#' the function ExportSvSingleTargetsRegionbyCell.
#' @param EVFile 
#' @param TSacoVarName 
#' @param regionClassName 
#' @param exportFn
#'
#' @return
#' This function exports single target by region-cells. The result is
#' a file with a row per interval-layer with targets meeting defined 
#' parameters, with the mean, minimum, and maximum TS for that cell.
#' @keywords Echoview COM scripting
#' @export
#' @references \url{http://support.echoview.com/WebHelp/Echoview.htm/}
#' @export
#' 


EVSingleTargetsRegionsByCells <- function (EVFile, TSacoVarName, regionClassName, exportFn) {
  #EVFile <- EVAppObj$OpenFile(uniqueTransect[i])
  EVVar <- EVFile[["Variables"]]$FindByName(TSacoVarName)
  acoVarObj <- EVAcoVarNameFinder(EVFile = EVFile, acoVarName = TSacoVarName)
  msgV <- acoVarObj$msg
  acoVarObj <- acoVarObj$EVVar
  EVRC <- EVRegionClassFinder(EVFile = EVFile, regionClassName = regionClassName)
  msgV <- c(msgV, EVRC$msg)
  RC <- EVRC$regionClass
  success <- acoVarObj$ExportSingleTargetsByRegionsByCells(exportFn, RC)
  if (success) {
    msg <- paste(Sys.time(), " : Successful export of single targets",
                 regionClassName, sep = "")
    message(msg)
    msgV <- c(msgV, msg)
  }
  else {
    msg <- paste(Sys.time(), " : Failed to or export ",
                 regionClassName, sep = "")
    warning(msg)
    msgV <- c(msgV, msg)
  }
}