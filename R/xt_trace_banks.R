#' Trace banklines from channel
#'
#' Trace bankline polygon by connecting bank points from planimetric cross sections.
#'
#' @param channel Channel object with planimetric cross sections
#' @param tracer Tracer algorithm to use for bankline tracing
#' @param ... Additional parameters (ignored)
#' @returns An sf POLYGON representing the banklines
#' @details This function extracts the planimetric cross sections from the channel
#' object and traces their extents to create a bankline polygon. The algorithm
#' can be specified using the tracer parameter.
#' @examples
#' library(sf)
#' seg <- st_sfc(
#'   st_linestring(matrix(c(-0.2, 0.3, 0.2, 1), byrow = TRUE, ncol = 2)),
#'   st_linestring(matrix(c(0.1, 0.1, 1, 1), byrow = TRUE, ncol = 2)),
#'   st_linestring(matrix(c(0.1, 0, 1.3, 0.7), byrow = TRUE, ncol = 2)),
#'   st_linestring(matrix(c(0.3, -0.3, 1.3, 0), byrow = TRUE, ncol = 2)),
#'   st_linestring(matrix(c(0, -0.6, 1, -0.5), byrow = TRUE, ncol = 2)),
#'   st_linestring(matrix(c(0, -0.9, 1, -1), byrow = TRUE, ncol = 2))
#' )
#' channel <- xt_channel(seg)
#'
#' # banks <- xt_trace_banks(channel, tracer = "linear")
#' banks <- xt_trace_banks(channel, tracer = "spline")
#' plot(banks, col = "lightblue")
#' @export
xt_trace_banks <- function(channel, tracer = "linear") {
  checkmate::assert_class(channel, "sxchan")

  plan <- xt_column_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections")
  }

  # Handle text input by calling appropriate tracer function
  if (is.character(tracer)) {
    tracer_fun <- paste0("tracer_", tracer)
    tracer <- rlang::exec(tracer_fun)
  }

  # Execute the tracer function
  tracer(channel)
}
