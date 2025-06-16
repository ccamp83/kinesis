#' Plot 3D trajectory
#' 
#' @param data Data frame containing trajectory data
#' @param x_col Column name for x coordinates
#' @param y_col Column name for y coordinates
#' @param z_col Column name for z coordinates
#' @param color_by Optional column to color points by
#' @param title Plot title
#' @return A 3D plot using plotly
#' @export
kin.plot.trajectory <- function(data, x_col, y_col, z_col, color_by = NULL, title = "3D Trajectory") {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for this function")
  }
  
  plot <- plotly::plot_ly(data, x = ~get(x_col), y = ~get(y_col), z = ~get(z_col),
                         type = "scatter3d", mode = "lines+markers")
  
  if (!is.null(color_by)) {
    plot <- plot %>% plotly::add_trace(color = ~get(color_by))
  }
  
  plot %>% plotly::layout(title = title,
                         scene = list(
                           xaxis = list(title = x_col),
                           yaxis = list(title = y_col),
                           zaxis = list(title = z_col)
                         ))
}

#' Plot velocity profile
#' 
#' @param data Data frame containing velocity data
#' @param time_col Column name for time
#' @param velocity_col Column name for velocity
#' @param title Plot title
#' @return A line plot using ggplot2
#' @export
kin.plot.velocity <- function(data, time_col, velocity_col, title = "Velocity Profile") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  ggplot2::ggplot(data, ggplot2::aes(x = get(time_col), y = get(velocity_col))) +
    ggplot2::geom_line() +
    ggplot2::labs(title = title,
                  x = "Time",
                  y = "Velocity") +
    ggplot2::theme_minimal()
}

#' Plot grip aperture
#' 
#' @param data Data frame containing grip aperture data
#' @param time_col Column name for time
#' @param aperture_col Column name for grip aperture
#' @param title Plot title
#' @return A line plot using ggplot2
#' @export
kin.plot.grasp <- function(data, time_col, aperture_col, title = "Grip Aperture") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  ggplot2::ggplot(data, ggplot2::aes(x = get(time_col), y = get(aperture_col))) +
    ggplot2::geom_line() +
    ggplot2::labs(title = title,
                  x = "Time",
                  y = "Grip Aperture") +
    ggplot2::theme_minimal()
} 