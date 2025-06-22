#' Set Standard Data Column Names for Kinesis Analysis
#' 
#' @description
#' Sets the standard column names used for kinesis data analysis. These columns
#' typically include subject identifier, frame number, time, time difference, and
#' trial number.
#' 
#' @param subjName Character string specifying the column name for subject identifier.
#'                Default is "subjName".
#' @param frameN Character string specifying the column name for frame number.
#'               Default is "frameN".
#' @param time Character string specifying the column name for time measurements.
#'             Default is "time".
#' @param deltaTime Character string specifying the column name for time differences.
#'                 Default is "deltaTime".
#' @param trialN Character string specifying the column name for trial number.
#'               Default is "trialN".
#'
#' @return None (invisibly). Updates the internal kinesis_parameters environment.
#' 
#' @examples
#' # Use default column names
#' kin.setDataCols()
#' 
#' # Customize column names
#' kin.setDataCols(subjName = "participant", 
#'                 frameN = "frame", 
#'                 time = "timestamp",
#'                 deltaTime = "dt",
#'                 trialN = "trial")
#'
#' @export
kin.setDataCols <- function(subjName = "subjName", frameN = "frameN", time = "time", deltaTime = "deltaTime", trialN = "trialN")
{
  .kinesis_env$dataCols <- c(subjName,frameN,time,deltaTime,trialN)
}