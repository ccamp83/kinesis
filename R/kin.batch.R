#' Process multiple trials in parallel
#' 
#' @param data Data frame containing all trials
#' @param start_positions List of start positions for each trial
#' @param end_positions List of end positions for each trial
#' @param refresh_rate Sampling rate in seconds
#' @param n_cores Number of cores to use for parallel processing
#' @param progress Show progress bar
#' @return List of processed trials
#' @export
kin.batch.process <- function(data, start_positions, end_positions, 
                            refresh_rate = 1/120, n_cores = NULL, 
                            progress = TRUE) {
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package 'parallel' is required for this function")
  }
  
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
  }
  
  # Split data into trials
  trials <- split(data, data$trialN)
  
  # Create cluster
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl))
  
  # Process trials in parallel
  results <- parallel::parLapply(cl, seq_along(trials), function(i) {
    trial_data <- trials[[i]]
    start_pos <- start_positions[[i]]
    end_pos <- end_positions[[i]]
    
    # Process single trial
    kin.signal.analysis(trial_data, 
                       start_pos = start_pos,
                       end_pos = end_pos,
                       refresh_rate = refresh_rate)
  })
  
  return(results)
}

#' Export results to various formats
#' 
#' @param results List of processed trials
#' @param format Output format ("csv", "json", or "excel")
#' @param file_prefix Prefix for output files
#' @return NULL
#' @export
kin.export.results <- function(results, format = "csv", file_prefix = "kinesis_results") {
  if (!is.list(results)) {
    stop("Results must be a list")
  }
  
  format <- tolower(format)
  
  switch(format,
         "csv" = {
           for (i in seq_along(results)) {
             write.csv(results[[i]], 
                      file = paste0(file_prefix, "_trial_", i, ".csv"),
                      row.names = FALSE)
           }
         },
         "json" = {
           if (!requireNamespace("jsonlite", quietly = TRUE)) {
             stop("Package 'jsonlite' is required for JSON export")
           }
           for (i in seq_along(results)) {
             jsonlite::write_json(results[[i]], 
                                path = paste0(file_prefix, "_trial_", i, ".json"))
           }
         },
         "excel" = {
           if (!requireNamespace("writexl", quietly = TRUE)) {
             stop("Package 'writexl' is required for Excel export")
           }
           writexl::write_xlsx(results, 
                              path = paste0(file_prefix, ".xlsx"))
         },
         stop("Unsupported format. Use 'csv', 'json', or 'excel'")
  )
  
  invisible(NULL)
} 