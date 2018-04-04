#' @export
kin.setDataCols <- function(subjName = "subjName", frameN = "frameN", time = "time", deltaTime = "deltaTime", trialN = "trialN")
{
  kinesis_parameters$dataCols <- c(subjName,frameN,time,deltaTime,trialN)
}
