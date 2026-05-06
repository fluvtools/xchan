#' Trace a line through thalweg positions in plan view
#'
#' Projects each profile’s thalweg distance onto the corresponding planimetric
#' cross section (bank-to-bank segment), then connects those `(x, y)` positions in
#' downstream order along the channel axis — analogous to [xt_trace_centerline()],
#' but through lowest-point (`thalweg`) locations rather than mid-channel.
#'
#' @param channel Channel object with **both** planimetric cross sections and a
#'   profile column (`xs_profile` list).
#' @param axis Optional LINESTRING axis; used for section ordering only if
#'   **`chainage`** is absent (same as [xt_trace_centerline()]).
#'
#' @returns An `sfc` object:
#'   - **Single thalweg** at every section: one `LINESTRING`.
#'   - **Several thalwegs** (e.g. after widening): one `MULTILINESTRING` whose
#'     parts are ordered **left to right** along each cross section (by distance
#'     along the section). Part `j` links the `j`-th thalweg across downstream
#'     stations (requires the **same** number of thalwegs at every section).
#'
#' @details
#' Requires [xt_has_profile()]. Thalweg distances come from [get_thalweg_coords()];
#' positions along the plan line use the same bank-span interpolation as
#' [create_3d_coords()] (first plan vertex = left bank, last = right bank).
#'
#' Section order follows increasing **`chainage`** when present; otherwise
#' increasing projection onto `axis` of the centroid of each section’s thalweg
#' plan points.
#'
#' @seealso [xt_trace_centerline()], [xt_axis()]
#' @export
#' @examples
#' \donttest{
#' # Requires profiles — for example from xt_generate_profile()
#' # th <- xt_trace_thalwegline(channel_with_profiles)
#' # plot(th)
#' }
xt_trace_thalwegline <- function(channel, axis = NULL) {
  if (!is_channel(channel)) {
    stop("Input must be a channel object", call. = FALSE)
  }
  if (!xt_has_profile(channel)) {
    stop(
      "xt_trace_thalwegline() needs profile cross sections; see xt_has_profile().",
      call. = FALSE
    )
  }

  plan <- xt_column_plan(channel)
  profs <- xt_column_profile(channel)
  if (is.null(plan) || is.null(profs)) {
    stop("Channel must have plan and profile columns.", call. = FALSE)
  }
  n <- length(plan)
  if (length(profs) != n) {
    stop("Length of plan and profile columns must match.", call. = FALSE)
  }

  n_thal <- vapply(profs, function(p) length(p$thalwegs), integer(1))
  if (length(unique(n_thal)) != 1L) {
    stop(
      "Every section must have the same number of thalwegs (found: ",
      paste(unique(n_thal), collapse = ", "),
      ").",
      call. = FALSE
    )
  }
  k <- n_thal[1L]
  if (k < 1L) {
    stop("Each profile must have at least one thalweg index.", call. = FALSE)
  }

  centroids <- vector("list", n)
  sec_xy <- vector("list", n)
  for (i in seq_len(n)) {
    xy <- thalweg_plan_xy_matrix(plan[i], profs[[i]])
    sec_xy[[i]] <- xy
    centroids[[i]] <- sf::st_point(colMeans(xy))
  }

  if (has_chainage_column(channel)) {
    trace_order <- order(channel[["chainage"]])
  } else {
    axis_line <- resolve_channel_axis(channel, axis, axis_arg_name = "axis")
    cent_sfc <- sf::st_sfc(centroids, crs = sf::st_crs(plan))
    d_ax <- as.numeric(sf::st_line_project(axis_line, cent_sfc))
    trace_order <- order(d_ax)
  }

  crs_out <- sf::st_crs(plan)

  if (k == 1L) {
    m <- t(vapply(trace_order, function(ii) sec_xy[[ii]][1L, , drop = TRUE], numeric(2)))
    ls <- sf::st_linestring(m)
    return(sf::st_sfc(ls, crs = crs_out))
  }

  coord_list <- vector("list", k)
  for (j in seq_len(k)) {
    coord_list[[j]] <- t(vapply(trace_order, function(ii) {
      sec_xy[[ii]][j, , drop = TRUE]
    }, numeric(2)))
  }
  ml <- sf::st_multilinestring(coord_list)
  sf::st_sfc(ml, crs = crs_out)
}

#' Thalweg (x, y) in plan, rows ordered left to right along the section
#'
#' @param plan_feat One `LINESTRING` feature (`sfg` or length-one `sfc`).
#' @param profile One `xs_profile` object.
#' @returns `k` x 2 matrix; `k` = number of thalwegs.
#' @noRd
thalweg_plan_xy_matrix <- function(plan_feat, profile) {
  checkmate::assert_class(profile, "xs_profile")
  tw <- get_thalweg_coords(profile)
  if (nrow(tw) < 1L) {
    stop("Profile has no thalweg coordinates.", call. = FALSE)
  }
  tw <- tw[order(tw[, 1L]), , drop = FALSE]

  pc <- sf::st_coordinates(plan_feat)
  x0 <- pc[1L, 1L]
  y0 <- pc[1L, 2L]
  x1 <- pc[nrow(pc), 1L]
  y1 <- pc[nrow(pc), 2L]
  run <- x1 - x0
  rise <- y1 - y0

  bd <- range(get_bank_distances(profile))
  denom <- bd[2L] - bd[1L]
  d1 <- tw[, 1L]

  if (!is.finite(denom) || denom == 0) {
    mx <- (x0 + x1) / 2
    my <- (y0 + y1) / 2
    return(cbind(rep(mx, nrow(tw)), rep(my, nrow(tw))))
  }

  frac <- (d1 - bd[1L]) / denom
  x <- x0 + frac * run
  y <- y0 + frac * rise
  cbind(x, y)
}
