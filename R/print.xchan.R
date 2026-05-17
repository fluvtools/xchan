#' Print a channel object
#'
#' @param x An [`xchan`].
#' @param n Maximum number of cross sections to print, in **list order** (same
#'   as `[[1]]`, `[[2]]`, …). When a channel axis is available ([xt_axis()]),
#'   each line is prefixed with that section’s **upstream-to-downstream** index
#'   among the sections being printed (\code{1} = most upstream, \code{n} =
#'   most downstream); otherwise indices are \code{1} … \code{n} in list order.
#'   If [xt_section_id()] is set to a vector of length \code{length(x)} and it is
#'   **not** exactly the consecutive integers \code{1}, \code{2}, …, \code{n} in
#'   list order, each printed line also includes \verb{: ID <key>} for that list
#'   position’s key. The default is `6`. Use `Inf` to print every section.
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
    lab <- xchan_upstream_downstream_print_labels(x)
    sid <- xt_section_id(x)
    show_id <- xchan_print_show_section_keys(sid, nsec)
    for (k in seq_len(n_show)) {
      wi <- w[k]
      if (inherits(wi, "units")) {
        w_str <- paste(format(as.numeric(wi), trim = TRUE), units::deparse_unit(wi))
      } else {
        w_str <- paste0(format(as.numeric(wi), trim = TRUE), " (-)")
      }
      if (show_id) {
        id_k <- xchan_print_section_id_value(sid[[k]])
        cat(
          "<xsection ",
          lab[k],
          ": ID ",
          id_k,
          "> ",
          w_str,
          "\n",
          sep = ""
        )
      } else {
        cat(sprintf("<xsection %d> %s\n", lab[k], w_str))
      }
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

#' @noRd
xchan_print_show_section_keys <- function(sid, n) {
  !is.null(sid) &&
    length(sid) == n &&
    !(is.numeric(sid) &&
      !anyNA(sid) &&
      identical(as.integer(sid), seq_len(n)))
}

#' @noRd
xchan_print_section_id_value <- function(x) {
  if (is.factor(x)) {
    return(as.character(x))
  }
  if (is.logical(x)) {
    return(as.character(x))
  }
  format(x, trim = TRUE)
}

#' @noRd
xchan_upstream_downstream_print_labels <- function(x) {
  n <- length(x)
  if (n == 0L) {
    return(integer())
  }
  ds <- tryCatch(
    as.numeric(xt_distance_downstream(x)),
    error = function(e) NULL
  )
  if (is.null(ds) || length(ds) != n || anyNA(ds)) {
    return(seq_len(n))
  }
  ord <- order(ds, seq_len(n))
  lab <- integer(n)
  for (r in seq_len(n)) {
    lab[ord[r]] <- r
  }
  lab
}
