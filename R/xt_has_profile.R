#' Check if an object has profile cross-section geometry
#'
#' For an \code{xsection}, returns whether \code{profile} is non-\code{NULL}.
#' For \code{xchan}, returns \code{TRUE} only when **every** cross section
#' carries an \code{xs_profile}, and \code{FALSE} when **none** do.
#'
#' Mixing sections with and without profiles is not allowed; constructors and
#' replacement helpers enforce this invariant when building or updating channel
#' geometry.
#'
#' @param x An \code{xsection} or \code{xchan} object.
#'
#' @details
#' All cross sections in an \code{xchan} must share the same profile state (all
#' have \code{xs_profile} geometry or all omit it). If that invariant is
#' violated, functions that read profile state — including
#' \code{xt_has_profile()} for an \code{xchan} — raise an error naming which
#' positions differ.
#'
#' @returns `TRUE` if profile geometry is present as described above; `FALSE` if
#' uniformly absent.
#' @rdname xt_has
#' @examples
#' ch <- xt_as_channel(c(2, 2))
#' xt_has_profile(ch)
#' ch <- xt_add_profile(
#'   ch,
#'   distance = distance,
#'   elevation = elevation,
#'   section = id,
#'   banks = is_bank,
#'   data = profile_survey
#' )
#' xt_has_profile(ch)
#' @export
xt_has_profile <- function(x) {
  if (inherits(x, "xsection")) {
    return(!is.null(x$profile))
  }
  if (inherits(x, "xchan")) {
    assert_xchan_profile_homogeneity(x)
    if (length(x) == 0L) {
      return(FALSE)
    }
    return(!is.null(x[[1L]]$profile))
  }
  stop(
    "`x` must be an `xsection` or `xchan` object.",
    call. = FALSE
  )
}
