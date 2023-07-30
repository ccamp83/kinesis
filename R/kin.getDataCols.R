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
