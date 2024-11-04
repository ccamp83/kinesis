#' Get Data Column Names for Kinesis
#' 
#' Returns a data frame containing the default and current data column names used
#' in kinesis data processing.
#' 
#' @return A data frame with 2 rows ("DEFAULT" and "CURRENT") containing the standard
#'         column names and any custom column names defined in kinesis_parameters.
#'         The default columns are:
#'         - subjName: Subject identifier
#'         - frameN: Frame number
#'         - time: Timestamp
#'         - deltaTime: Time difference between frames
#'         - trialN: Trial number
#'         Additional columns are pulled from the dataCols parameter in kinesis_parameters.
#' 
#' @export
kin.getDataCols <- function()
{
  output <- as.data.frame(
  matrix(c("subjName","frameN","time","deltaTime","trialN",
         get("dataCols", kinesis_parameters)), nrow = 2, byrow = T)
  )

  colnames(output) <- NULL
  rownames(output) <- c("DEFAULT","CURRENT")

  return(output)
}
