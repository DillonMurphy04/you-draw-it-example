# Generated from ShinyDrawerCode.Rmd: do not edit by hand

#' Interactive drawing graphic
#' 
#' @param data data for graph
#' 
#' @export
data_to_json <- function(data) {
  jsonlite::toJSON(data, 
                   dataframe = "rows", 
                   auto_unbox = FALSE, 
                   rownames = TRUE)
} 

#' @export
shiny_drawer <- function(data, 
                  linear            = "true", 
                  draw_start        = NULL,
                  points_end        = NULL,
                  x_by              = 0.25,
                  free_draw         = T,
                  points            = "partial",
                  aspect_ratio      = 1.5,
                  title             = "", 
                  x_range           = NULL, 
                  y_range           = NULL,
                  x_lab             = "", 
                  y_lab             = "", 
                  drawn_line_color  = "steelblue",
                  data_tab1_color   = "steelblue", 
                  x_axis_buffer     = 0.01, 
                  y_axis_buffer     = 0.05,
                  show_finished     = T,
                  shiny_message_loc = NULL) {
  
  line_data  <- data$line_data
  point_data <- data$point_data
  
  x_min <- min(line_data$x)
  x_max <- max(line_data$x)
  y_min <- min(line_data$y)
  y_max <- max(line_data$y)
  
  if (is.null(x_range)) {
    x_buffer <- (x_max - x_min) * x_axis_buffer
    x_range <- c(x_min - x_buffer, x_max + x_buffer)
  }
  if (is.null(y_range)) {
    y_buffer <- (y_max - y_min) * y_axis_buffer
    y_range <- c(y_min - y_buffer, y_max + y_buffer)
    if (linear != "true") {
      if (y_range[1] <= 0) {
        y_range[1] <- min(y_min, y_axis_buffer)
      }
    }
  } else {
    if (y_range[1] > y_min | y_range[2] < y_max) {
      stop("Supplied y range doesn't cover data fully.")
    }
  }
  
  if ((draw_start <= x_min) | (draw_start >= x_max)) {
    stop("Draw start is out of data range.")
  }
  
  r2d3::r2d3(data   = data_to_json(data), 
             script = "www/you-draw-it.js",
             d3_version = "5",
             dependencies = c("d3-jetpack"),
             options = list(draw_start        = draw_start, 
                            points_end        = points_end,
                            linear            = as.character(linear),
                            free_draw         = free_draw, 
                            points            = points,
                            aspect_ratio      = aspect_ratio,
                            pin_start         = T, 
                            x_range           = x_range,
                            x_by              = x_by,
                            y_range           = y_range,
                            line_style        = NULL,
                            data_tab1_color   = data_tab1_color, 
                            drawn_line_color  = drawn_line_color,
                            show_finished     = show_finished,
                            shiny_message_loc = shiny_message_loc,
                            title             = title)
  )
  
}
