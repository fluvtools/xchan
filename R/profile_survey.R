#' Toy cross-section survey data
#'
#' Synthetic tabular profile observations for six cross sections, intended as
#' example survey input for [xt_add_profile()]. This is **not** data from a real
#' channel.
#'
#' @format A [tibble::tibble()] with 138 rows and 4 columns:
#' \describe{
#'   \item{id}{Integer cross-section key (`1`--`6`), matching the order of
#'     the cross sections.}
#'   \item{distance}{Chord distance along each transect (`units::units()` in
#'     metres). Distances increase from left bank toward right bank, with
#'     negative values on the left floodplain approach and positive values on
#'     the right.}
#'   \item{elevation}{Ground elevation at each vertex (`units::units()` in
#'     metres).}
#'   \item{is_bank}{Logical flag; `TRUE` at the left and right bank vertices
#'     (distances `0` and channel width) for each section.}
#' }
#' @details Each section has 23 survey points: ten on the left approach, three
#' along the channel bottom (including the thalweg), and ten on the right
#' floodplain. Rows are stored in
#' section order with vertices in survey chainage order.
#' @source Synthetically generated in `data-raw/profile_survey.R`.
#' @seealso [xt_add_profile()]
"profile_survey"
