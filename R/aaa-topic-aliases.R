#' Cross-section objects (`xsection`)
#'
#' One element of an [`xchan`]: a planimetric transect (`plan`, numeric matrix)
#' and optional profile geometry (`profile`, an `xs_profile` object).
#'
#' Users obtain `xsection` objects from [`xchan`][xchan()] indexing (`[[`),
#' [xt_xsection_at()], or [xt_as_channel()]; test with [is.xsection()].
#'
#' @seealso [xchan()], [is.xsection()], [xt_xsection_at()], [plot.xsection()]
#' @name xsection
NULL

#' Flip profile cross-section orientation (internal)
#'
#' Used when reversing flow or eroding the opposite bank. Not exported.
#'
#' @name flip_profile
NULL
