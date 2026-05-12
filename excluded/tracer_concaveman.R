#' Create a concaveman tracer for bankline generation
#'
#' @param ... Arguments to pass to `concaveman::concaveman()`.
#' @returns A tracer object that takes a channel and returns banklines.
#' @export
tracer_concaveman <- function(...) {
  f <- function(channel) {
    plan <- channel_plan(channel)
    pts <- sf::st_cast(plan, "POINT")
    pts_sf <- sf::st_sf(geometry = pts)
    concaveman::concaveman(pts_sf, ...)
  }
  structure(
    f,
    name = "Concaveman Tracer",
    params = list(...),
    class = "xchan_tracer"
  )
}
