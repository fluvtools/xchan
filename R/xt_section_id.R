#' Cross-section identity keys on a channel
#'
#' Get or set per-section **stable keys** stored on an [`xchan`]. These keys
#' match the `section` column when attaching tabular profiles with
#' [xt_add_profile()].
#'
#' @param channel An [`xchan`] object.
#' @param value `NULL` removes stored keys (downstream functions then treat
#'   section
#'   indices as `seq_len(length(channel))`). Otherwise an atomic vector of
#'   length `length(channel)` with **no** `NA`s and **no** duplicates.
#'
#' @details
#' Keys are stored as attribute `"section_i"` on the [`xchan`] (the name is
#' historical). [xchan()] initializes them to `seq_len(n)`.
#'
#' @returns For `xt_section_id()`, the stored vector or `NULL`. For assignment,
#'   an
#'   updated [`xchan`].
#'
#' @seealso [xt_add_profile()], [xchan()]
#' @export
#' @examples
#' ch <- xchan(list(
#'   xsection(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
#'   xsection(matrix(c(0, 1, 1, 1), ncol = 2, byrow = TRUE))
#' ))
#' xt_section_id(ch)
#' xt_section_id(ch) <- c("A12", "B04")
#' xt_section_id(ch)
#' xt_section_id(ch) <- NULL
#' xt_section_id(ch)
xt_section_id <- function(channel) {
  UseMethod("xt_section_id")
}

#' @rdname xt_section_id
#' @export
xt_section_id.xchan <- function(channel) {
  checkmate::assert_class(channel, "xchan")
  attr(channel, "section_i", exact = TRUE)
}

#' @rdname xt_section_id
#' @export
xt_section_id.default <- function(channel) {
  stop(
    "`xt_section_id()` expects an `xchan`. Got class(es): ",
    paste(class(channel), collapse = ", "),
    ".",
    call. = FALSE
  )
}

#' @rdname xt_section_id
#' @export
`xt_section_id<-` <- function(channel, value) {
  UseMethod("xt_section_id<-")
}

#' @rdname xt_section_id
#' @export
`xt_section_id<-.xchan` <- function(channel, value) {
  checkmate::assert_class(channel, "xchan")
  if (is.null(value)) {
    attr(channel, "section_i") <- NULL
    return(channel)
  }
  n <- length(channel)
  if (inherits(value, "factor")) {
    value <- as.character(value)
  }
  value <- unname(as.vector(value))
  checkmate::assert_atomic(value)
  checkmate::assert_true(length(value) == n)
  checkmate::assert_true(!anyNA(value))
  if (anyDuplicated(value)) {
    stop("`xt_section_id` values must be unique.", call. = FALSE)
  }
  attr(channel, "section_i") <- value
  channel
}

#' @rdname xt_section_id
#' @export
`xt_section_id<-.default` <- function(channel, value) {
  stop(
    "`xt_section_id<-()` expects an `xchan`. Got class(es): ",
    paste(class(channel), collapse = ", "),
    ".",
    call. = FALSE
  )
}
