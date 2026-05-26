#' Internal `xs_profile` constructor (centering, sorting, bank injection).
#'
#' @noRd
new_profile <- function(coords, bankpoints) {
  checkmate::assert_matrix(
    coords,
    mode = "numeric",
    any.missing = FALSE,
    ncols = 2
  )
  checkmate::assert_numeric(bankpoints, any.missing = FALSE, min.len = 2L)
  if (length(bankpoints) %% 2 != 0) {
    stop("bankpoints must have an even number of elements.")
  }

  # 1. Distances along the cross section are shifted so that 0 represents the
  #    center point.
  # The mean of the most extreme bankpoints represents the offset from 0 and
  # can be used as the shift quantity.
  shift <- mean(range(bankpoints))
  coords[, 1] <- coords[, 1] - shift
  bankpoints <- bankpoints - shift

  # 2. Bank points and coordinates are sorted from left to right. In case of
  #    vertical cliffs having the same distance value, the order is preserved.
  coords <- coords[order(coords[, 1]), ]
  bankpoints <- sort(bankpoints)

  # 3. Bank points are always present in the profile coordinates.
  coords <- inject_coords(coords, bankpoints)

  # Convert bank distances to coordinate row indices.
  bank_idx <- vapply(
    bankpoints,
    function(b) which.min(abs(coords[, 1] - b)),
    integer(1)
  )

  # 4. Thalwegs are the lowest wetted vertices only; dry floodplain
  # depressions outside bank-paired water intervals do not qualify.
  thalweg_idx <- thalweg_indices_from_banks(coords, bankpoints)
  thalweg_elev <- min(coords[thalweg_idx, 2])

  profile <- list(
    coordinates = coords,
    banks = bank_idx,
    thalwegs = thalweg_idx,
    thalweg_elev = thalweg_elev
  )
  structure(profile, class = "xs_profile")
}
