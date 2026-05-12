#' Plot one cross section (`xsection`)
#'
#' Plot the profile view when a profile exists (default), or the planimetric
#' transect. For profile-only plots you can also call `plot(x$profile)` on the
#' embedded `xs_profile`.
#'
#' @param x An \code{\link{xsection}} object.
#' @param ... Additional arguments passed to \code{\link{plot.xs_profile}}
#'   (profile view) or forwarded when plotting the plan view.
#' @param view `"auto"` plots the profile when `x$profile` is present, otherwise
#'   the plan view. Use `"profile"` or `"plan"` to force one view.
#' @inheritParams plot.xs_profile
#' @param col,lwd,col_bank_water,col_bank_land,pch_bank,cex_bank,warn_if_no_profile
#'   Used for the plan view only (see \code{\link{plot.xchan}}).
#'
#' @returns Called for its graphical side effect.
#'
#' @examples
#' coords <- matrix(c(-3, 10, 0, 8, 3, 10), ncol = 2, byrow = TRUE)
#' prof <- xchan:::new_profile(coords, bankpoints = c(-3, 3))
#' plan_ls <- sf::st_linestring(matrix(c(0, 0, 6, 0), ncol = 2))
#' seg <- sf::st_sfc(plan_ls, crs = 3005)
#' ch <- xt_as_channel(seg, profile = list(prof))
#' plot(ch[[1]])
#'
#' @exportS3Method base::plot
plot.xsection <- function(
  x,
  ...,
  view = c("auto", "profile", "plan"),
  extent = c("banks", "full"),
  add = FALSE,
  exaggerate = 1,
  from = NULL,
  to = NULL,
  col = "black",
  lwd = 1,
  col_bank_water = "deepskyblue3",
  col_bank_land = "gray35",
  pch_bank = 16,
  cex_bank = 0.65,
  warn_if_no_profile = TRUE
) {
  checkmate::assert_class(x, "xsection")
  view <- match.arg(view)
  if (view == "auto") {
    view <- if (!is.null(x$profile)) "profile" else "plan"
  }

  if (view == "profile") {
    if (is.null(x$profile)) {
      stop(
        "This cross section has no profile geometry; use view = \"plan\" or attach a profile.",
        call. = FALSE
      )
    }
    plot.xs_profile(
      x$profile,
      ...,
      extent = extent,
      add = add,
      exaggerate = exaggerate,
      from = from,
      to = to
    )
  } else {
    xc <- xchan(list(x), crs = NA)
    plot_plan(
      xc,
      ...,
      extent = extent,
      add = add,
      col = col,
      lwd = lwd,
      col_bank_water = col_bank_water,
      col_bank_land = col_bank_land,
      pch_bank = pch_bank,
      cex_bank = cex_bank,
      warn_if_no_profile = warn_if_no_profile
    )
  }
}
