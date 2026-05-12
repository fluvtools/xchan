#' Print a channel object
#'
#' @param x An [`xchan`].
#' @param n Maximum number of cross sections to print, in **list order** (same
#'   as `[[1]]`, `[[2]]`, …). Line labels use `attr(x, "section_i")` when set
#'   (see `[` subsetting on \code{\link{xchan}}), otherwise `1`, `2`, …. The default is `6`. Use `Inf` to
#'   print every section.
#' @param ... Ignored.
#' @exportS3Method base::print
print.xchan <- function(x, ..., n = 6) {
  rlang::check_dots_empty()
  nsec <- length(x)
  cat("xchan channel with", nsec, "cross sections.\n")
  crs <- xchan_crs(x)
  if (!is.na(crs)) {
    cat("CRS:", crs$input, "\n")
  }
  checkmate::assert_scalar(n)
  checkmate::assert_number(n, lower = 0, finite = FALSE)
  if (nsec > 0L && n > 0) {
    n_show <- if (is.infinite(n)) {
      nsec
    } else {
      min(max(floor(as.numeric(n)), 0L), nsec)
    }
    w <- xt_width(x)
    ids <- attr(x, "section_i", exact = TRUE)
    if (is.null(ids) || length(ids) != nsec) {
      ids <- seq_len(nsec)
    }
    for (k in seq_len(n_show)) {
      wi <- w[k]
      lab <- ids[k]
      if (inherits(wi, "units")) {
        w_str <- paste(format(as.numeric(wi), trim = TRUE), units::deparse_unit(wi))
      } else {
        w_str <- paste0(format(as.numeric(wi), trim = TRUE), " (-)")
      }
      cat(sprintf("<xsection %d> %s\n", lab, w_str))
    }
    if (is.finite(n) && n_show < nsec) {
      cat("...", nsec - n_show, "more cross sections\n")
    }
  }
  if (nsec > 0L && xt_has_profile(x)) {
    cat("With profile view\n")
  }
  invisible(x)
}
