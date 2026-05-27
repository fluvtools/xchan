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
#' @param col,lwd,banks,col_bank,pch_bank,cex_bank,warn_if_no_profile
#'   Used for the plan view only (see \code{\link{plot.xchan}}).
#'
#' @returns Called for its graphical side effect.
#'
#' @examples
#' channel <- xt_as_channel(rep(1, 6))
#' channel <- xt_add_profile(
#'   channel,
#'   distance = distance,
#'   elevation = elevation,
#'   section = id,
#'   banks = is_bank,
#'   data = profile_survey
#' )
#' plot(channel[[1]])
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
  banks = c("auto", "show", "hide"),
  col_bank = "deepskyblue3",
  pch_bank = 16,
  cex_bank = 0.65,
  warn_if_no_profile = TRUE
) {
  checkmate::assert_class(x, "xsection")
  view <- match.arg(view)
  banks <- match.arg(banks)
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
      extent = extent,
      axis = "none",
      add = add,
      col = col,
      lwd = lwd,
      banks = banks,
      col_bank = col_bank,
      pch_bank = pch_bank,
      cex_bank = cex_bank,
      warn_if_no_profile = warn_if_no_profile,
      ...
    )
  }
}
