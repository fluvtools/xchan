#' Plot planimetric cross sections
#'
#' Plot the planimetric cross sections from a channel object.
#'
#' @param channel Channel object with planimetric cross sections
#' @param extent Draw bank-to-bank segments only (`"banks"`), or extend each
#'   transect to match the horizontal span of its profile when profiles exist
#'   (`"full"`).
#' @param ... Additional arguments forwarded to `plot()` for `sfc` objects.
#' @param add Logical. Add to existing plot?
#' @param col Color for the cross-section lines
#' @param lwd Line width
#' @param col_bank_water,col_bank_land Colors for bank markers when
#'   `extent = "full"`: alternating along profile banks (left-to-right), used
#'   when more than one bank boundary exists (for example islands).
#' @param pch_bank,cex_bank Passed to [graphics::points()] for bank markers.
#' @param warn_if_no_profile Warning when `extent = "full"` but the channel has
#'   no profile geometry.
#' @returns `NULL` invisibly (called for side effect).
#' @noRd
plot_plan <- function(
  channel,
  ...,
  extent = c("banks", "full"),
  add = FALSE,
  col = "black",
  lwd = 1,
  col_bank_water = "deepskyblue3",
  col_bank_land = "gray35",
  pch_bank = 16,
  cex_bank = 0.65,
  warn_if_no_profile = TRUE
) {
  checkmate::assert_class(channel, "xchan_tbl")

  extent <- match.arg(extent)

  plan <- channel_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections")
  }

  want_full <- extent == "full"
  if (want_full && !xt_has_profile(channel)) {
    if (warn_if_no_profile) {
      warning(
        'extent = "full" requires profile cross sections; plotting bank-to-bank extent instead.',
        call. = FALSE
      )
    }
    want_full <- FALSE
  }

  if (want_full) {
    profiles <- channel_profile(channel)
    geoms <- vector("list", length(plan))
    for (i in seq_along(plan)) {
      rng <- range(profiles[[i]]$coordinates[, 1])
      geoms[[i]] <- transect_segment_from_relative(plan[[i]], rng[1], rng[2])
    }
    geoms <- sf::st_sfc(geoms, crs = sf::st_crs(plan))
  } else {
    geoms <- plan
  }

  if (!add) {
    plot(geoms, col = col, lwd = lwd, ...)
  } else {
    plot(geoms, col = col, lwd = lwd, add = TRUE, ...)
  }

  if (want_full) {
    profiles <- channel_profile(channel)
    for (i in seq_along(plan)) {
      bc <- get_bank_coords(profiles[[i]])
      nb <- nrow(bc)
      cols <- rep_len(c(col_bank_water, col_bank_land), length.out = nb)
      for (k in seq_len(nb)) {
        xy <- transect_xy_from_relative(plan[[i]], bc[k, 1])
        graphics::points(xy[1], xy[2], col = cols[k], pch = pch_bank, cex = cex_bank)
      }
    }
  }

  invisible(NULL)
}
