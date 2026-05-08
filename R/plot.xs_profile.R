#' Plot profile cross section
#'
#' Plot a profile cross section showing the elevation profile.
#' This function is primarily for internal use or advanced users.
#' Most users should work with channel objects using plot_plan() or plot_3d().
#'
#' @param x An xs_profile object
#' @param ... Additional arguments passed to plot
#' @param extent Selects the default horizontal span: `"banks"` uses the span
#'   between the outer bank distances; `"full"` uses the full range of profile
#'   sample distances (default `"banks"`).
#' @param from,to Optional adjustments to that window (distance along the
#'   profile). The plotted range starts at `from` when given, otherwise at the
#'   default left edge for `extent`; it ends at `to` when given, otherwise at
#'   the default right edge. You may pass `from` only, `to` only, both, or
#'   neither. Values may lie outside the data range (empty band on that side).
#' @param add Logical. Add to existing plot?
#' @param exaggerate Single positive numeric. Vertical exaggeration factor;
#' defaults to 2. See details.
#' @returns A plot of the profile cross section
#' @details
#' The exaggeration factor is applied to the vertical dimension of the profile,
#' and defaults to 2 so that changes in topography can be more easily
#' perceived. It is strongly recommended not going beyond 3, because
#' exaggeration beyond this point can distort the perception of the profile.
#'
#' For the horizontal axis, `extent` fixes the reference span (`rng_default`).
#' Each of `from` and `to` overrides one endpoint only when supplied; omitted
#' endpoints use `rng_default`.
#'
#' @examples
#' # Plot a profile cross section (advanced use)
#' plot(profile_object)
#'
#' # Plot with vertical exaggeration
#' plot(profile_object, exaggerate = 2)
#'
#' # Narrow or shift the window relative to `extent`
#' plot(profile_object, extent = "full", from = -100)
#' plot(profile_object, extent = "banks", to = 50)
#' @exportS3Method base::plot
plot.xs_profile <- function(
  x,
  ...,
  extent = c("banks", "full"),
  add = FALSE,
  exaggerate = 2,
  from = NULL,
  to = NULL
) {
  extent <- match.arg(extent)
  x <- xt_exaggerate_relief(x, times = exaggerate)

  rng_default <- if (extent == "banks") {
    range(get_bank_distances(x))
  } else {
    range(x$coordinates[, 1])
  }

  if (!is.null(from)) {
    checkmate::assert_number(from, finite = TRUE)
  }
  if (!is.null(to)) {
    checkmate::assert_number(to, finite = TRUE)
  }

  x0 <- if (!is.null(from)) from else rng_default[1L]
  x1 <- if (!is.null(to)) to else rng_default[2L]
  if (!(x0 < x1)) {
    stop("Implied horizontal range is empty: need `from` < `to`.", call. = FALSE)
  }

  if (!add) {
    args <- list(
      x = x$coordinates[, 1],
      y = x$coordinates[, 2],
      type = "n",
      xlab = "Distance",
      ylab = "Elevation",
      xlim = c(x0, x1)
    )
    args <- utils::modifyList(args, list(...))
    do.call(plot, args)
  }

  # Plot the main profile line
  graphics::lines(
    x$coordinates[, 1],
    x$coordinates[, 2],
    col = "black",
    lwd = 2,
    ...
  )

  # Plot bank points
  bank_coords <- get_bank_coords(x)
  graphics::points(
    bank_coords[, 1],
    bank_coords[, 2],
    col = "red",
    pch = 19,
    cex = 1.5,
    ...
  )

  # Plot thalweg points
  thalweg_coords <- get_thalweg_coords(x)
  graphics::points(
    thalweg_coords[, 1],
    thalweg_coords[, 2],
    col = "blue",
    pch = 17,
    cex = 1.2,
    ...
  )

  # Add legend
  graphics::legend(
    "topright",
    legend = c("Profile", "Banks", "Thalwegs"),
    col = c("black", "red", "blue"),
    lty = c(1, NA, NA),
    pch = c(NA, 19, 17),
    cex = 0.8
  )
}
