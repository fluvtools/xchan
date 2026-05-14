#' Plot planimetric cross sections
#'
#' Plot the planimetric cross sections from a channel object.
#'
#' @param channel [`xchan`] with planimetric cross sections
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
#' @param axis One of `"line"`, `"arrows"`, or `"none"`; see [plot.xchan()].
#' @returns `NULL` invisibly (called for side effect).
#' @noRd
plot_plan <- function(
  channel,
  ...,
  extent = c("banks", "full"),
  axis = c("line", "arrows", "none"),
  add = FALSE,
  col = "black",
  lwd = 1,
  col_bank_water = "deepskyblue3",
  col_bank_land = "gray35",
  pch_bank = 16,
  cex_bank = 0.65,
  warn_if_no_profile = TRUE
) {
  checkmate::assert_class(channel, "xchan")

  extent <- match.arg(extent)
  axis <- match.arg(axis)

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

  dots <- list(...)
  axs_plot <- if (!add && axis != "none") xt_axis(channel) else NULL
  use_balanced_limits <- !add &&
    is.null(dots$xlim) &&
    is.null(dots$ylim) &&
    length(geoms) > 0L
  if (use_balanced_limits) {
    bb <- combine_plan_bbox(geoms, axs_plot)
    bb <- balance_plot_bbox(bb, max_ratio = 4, pad = 0.03)
    dots$xlim <- c(bb[["xmin"]], bb[["xmax"]])
    dots$ylim <- c(bb[["ymin"]], bb[["ymax"]])
  }

  do.call(
    plot,
    c(list(geoms, col = col, lwd = lwd), dots, list(add = add))
  )

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

  if (axis != "none") {
    axs <- xt_axis(channel)
    if (!is.null(axs)) {
      cr <- sf::st_coordinates(axs)[, c("X", "Y"), drop = FALSE]
      axis_col <- "steelblue"
      axis_lwd <- pmax(lwd * 1.1, 1)
      if (axis == "line") {
        graphics::lines(cr[, 1L], cr[, 2L], col = axis_col, lwd = axis_lwd)
      } else if (nrow(cr) >= 2L) {
        for (i in seq_len(nrow(cr) - 1L)) {
          graphics::arrows(
            cr[i, 1L],
            cr[i, 2L],
            cr[i + 1L, 1L],
            cr[i + 1L, 2L],
            length = 0.09,
            angle = 22,
            code = 2L,
            col = axis_col,
            lwd = axis_lwd
          )
        }
      }
    }
  }

  invisible(NULL)
}

#' @noRd
combine_plan_bbox <- function(geoms, axis_geom = NULL) {
  bb <- sf::st_bbox(geoms)
  if (!is.null(axis_geom)) {
    b2 <- sf::st_bbox(sf::st_geometry(axis_geom))
    bb <- sf::st_bbox(
      c(
        xmin = min(bb[["xmin"]], b2[["xmin"]]),
        ymin = min(bb[["ymin"]], b2[["ymin"]]),
        xmax = max(bb[["xmax"]], b2[["xmax"]]),
        ymax = max(bb[["ymax"]], b2[["ymax"]])
      ),
      crs = sf::st_crs(geoms)
    )
  }
  bb
}

#' @noRd
balance_plot_bbox <- function(bb, max_ratio = 4, pad = 0.04) {
  xmin <- unname(bb[["xmin"]])
  xmax <- unname(bb[["xmax"]])
  ymin <- unname(bb[["ymin"]])
  ymax <- unname(bb[["ymax"]])
  dx <- xmax - xmin
  dy <- ymax - ymin
  if (!is.finite(dx) || !is.finite(dy) || dx <= 0 || dy <= 0) {
    return(bb)
  }
  xc <- (xmin + xmax) / 2
  yc <- (ymin + ymax) / 2
  if (dx / dy > max_ratio) {
    dy <- dx / max_ratio
    ymin <- yc - dy / 2
    ymax <- yc + dy / 2
  } else if (dy / dx > max_ratio) {
    dx <- dy / max_ratio
    xmin <- xc - dx / 2
    xmax <- xc + dx / 2
  }
  mx <- (xmax - xmin) * pad
  my <- (ymax - ymin) * pad
  sf::st_bbox(
    c(
      xmin = xmin - mx,
      xmax = xmax + mx,
      ymin = ymin - my,
      ymax = ymax + my
    ),
    crs = sf::st_crs(bb)
  )
}
