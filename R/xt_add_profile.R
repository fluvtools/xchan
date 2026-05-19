#' Attach or replace profile cross sections from tabular data
#'
#' Build `xs_profile` objects from chord-distance / elevation columns (and bank
#' flags), attach them to each [`xsection`], and reconcile plan vs profile
#' bank-to-bank width using the same checks as [xt_width()] on plan and profile.
#' Distances are **straight chord stationing** along the bank-to-bank transect (not
#' arc length along a meandering ground path).
#'
#' @param channel An [`xchan`] or [`xsection`].
#' @param distance,elevation Tidy-eval expressions (see `data`) for chord distance
#'   and elevation; may be plain numeric or [units::units()] (distance is converted
#'   with the channel CRS length unit when available). Distances are expected to
#'   increase **from left bank toward right bank** along each transect (the same
#'   sense as planimetric chord stationing). If your survey chainage ran the
#'   opposite way, flip or negate the distance column before calling this function.
#' @param section Tidy-eval expression giving a **stable cross-section key** for
#'   each row (integer, character, etc.). Values are matched to [xt_section_id()]
#'   when that vector exists and has length `length(channel)`; otherwise to
#'   `seq_len(length(channel))`. Larger keys do
#'   **not** imply downstream order here — only identity. Rows are taken in
#'   **data frame row order** within each section (no sorting is applied by this
#'   function).
#' @param banks Tidy-eval logical vector: `TRUE` at bank vertices (even count per
#'   section). The **outer** banks are those with minimum and maximum chord
#'   distance among `TRUE` rows; inner `TRUE` values encode islands.
#' @param ... Must be empty. Arguments after `...` (`data`, `snap_banks_to`) must
#'   be matched by name.
#' @param data For [`xchan`] and [`xsection`], `NULL` (the default) evaluates
#'   `distance`, `elevation`, `section` (for [`xchan`] only), and `banks` in the
#'   calling environment. Otherwise a data frame (or data-mask object) whose rows
#'   align with those vectors.
#' @param snap_banks_to If `"plan"`, outer bank **distances** in the profile are
#'   set to **plus or minus** half the planimetric segment length (inner bank distances are
#'   unchanged; an error is raised if an inner bank would lie outside that span).
#'   If `"profile"`, plan segments are widened or narrowed with [xt_widen()] so
#'   their length matches the surveyed outer bank span (symmetric split). There
#'   is no affine stretch of interior profile distances.
#'
#' @returns A **new** [`xchan`] or [`xsection`] with profiles attached or replaced
#'   (the input object is not modified). For [`xchan`], **every** section receives a
#'   new profile in one call (required for profile homogeneity).
#'
#' @details
#' Each built `xs_profile` stores chord distances with **0 at the midpoint between
#' the two outer bank distances**, vertices sorted by increasing distance (stable
#' ordering for ties), and thalwegs re-derived from the sorted polyline. Existing
#' profiles on a channel are always **replaced** in full. If the channel
#' already had profiles, supply rows for **all** sections identified by
#' [xt_section_id()] (or `seq_len(n)` when that vector is absent).
#'
#' @export
xt_add_profile <- function(channel, ...) {
  UseMethod("xt_add_profile")
}

#' @rdname xt_add_profile
#' @export
xt_add_profile.xchan <- function(
  channel,
  distance,
  elevation,
  section,
  banks,
  ...,
  data = NULL,
  snap_banks_to = c("profile", "plan")
) {
  rlang::check_dots_empty()
  checkmate::assert_class(channel, "xchan")
  snap_banks_to <- rlang::arg_match(snap_banks_to)

  n <- length(channel)
  sid <- attr(channel, "section_i", exact = TRUE)
  if (is.null(sid) || length(sid) != n) {
    sid <- seq_len(n)
  }

  dist_q <- rlang::enquo(distance)
  elev_q <- rlang::enquo(elevation)
  sec_q <- rlang::enquo(section)
  bank_q <- rlang::enquo(banks)

  if (is.null(data)) {
    env <- rlang::caller_env()
    d_all <- rlang::eval_tidy(dist_q, env = env)
    z_all <- rlang::eval_tidy(elev_q, env = env)
    s_all <- rlang::eval_tidy(sec_q, env = env)
    b_all <- rlang::eval_tidy(bank_q, env = env)
    n_row <- length(d_all)
  } else {
    checkmate::assert_data_frame(data, min.rows = 1L)
    d_all <- rlang::eval_tidy(dist_q, data = data)
    z_all <- rlang::eval_tidy(elev_q, data = data)
    s_all <- rlang::eval_tidy(sec_q, data = data)
    b_all <- rlang::eval_tidy(bank_q, data = data)
    n_row <- nrow(data)
  }

  assert_tab_profile_vectors(d_all, z_all, s_all, b_all, n_row)

  unit <- channel_length_unit(channel)
  input_unit <- units_deparse(d_all)
  if (is.null(unit) && !is.null(input_unit)) {
    unit <- input_unit
  }
  d_all <- coerce_axis_numeric(d_all, unit, "distance")
  z_all <- coerce_axis_numeric(z_all, NULL, "elevation")

  plan <- channel_plan(channel)
  L <- vapply(plan, function(g) as.numeric(sf::st_length(g)), numeric(1))

  profiles <- vector("list", n)
  dw <- rep(0, n)

  for (j in seq_len(n)) {
    key <- sid[j]
    pick <- profile_row_indices_for_key(s_all, key, n_row)
    if (!length(pick)) {
      stop(
        "No rows for section key ",
        format(key),
        " (list position ",
        j,
        "). Supply data for every section.",
        call. = FALSE
      )
    }
    d <- d_all[pick]
    z <- z_all[pick]
    b <- b_all[pick]
    prof <- build_xs_profile_from_table_rows(d, z, b)
    if (snap_banks_to == "plan") {
      prof <- snap_profile_outer_banks_to_plan_width(prof, L[j])
    } else {
      W <- as.numeric(xt_width(prof))
      dw[j] <- W - L[j]
    }
    profiles[[j]] <- prof
  }

  out <- clone_xchan_shell(channel)

  if (snap_banks_to == "profile" && any(dw != 0)) {
    out <- xt_widen(out, dw = dw, side = "both")
  }

  out <- set_channel_profile(out, profiles)
  if (is.null(channel_length_unit(channel)) && !is.null(input_unit)) {
    out <- set_xchan_length_unit(out, input_unit)
  }
  out
}

#' @rdname xt_add_profile
#' @export
xt_add_profile.xsection <- function(
  channel,
  distance,
  elevation,
  banks,
  ...,
  data = NULL,
  snap_banks_to = c("profile", "plan")
) {
  rlang::check_dots_empty()
  checkmate::assert_class(channel, "xsection")
  snap_banks_to <- rlang::arg_match(snap_banks_to)

  dist_q <- rlang::enquo(distance)
  elev_q <- rlang::enquo(elevation)
  bank_q <- rlang::enquo(banks)

  if (is.null(data)) {
    d <- rlang::eval_tidy(dist_q, env = rlang::caller_env())
    z <- rlang::eval_tidy(elev_q, env = rlang::caller_env())
    b <- rlang::eval_tidy(bank_q, env = rlang::caller_env())
  } else {
    checkmate::assert_data_frame(data, min.rows = 1L)
    d <- rlang::eval_tidy(dist_q, data = data)
    z <- rlang::eval_tidy(elev_q, data = data)
    b <- rlang::eval_tidy(bank_q, data = data)
  }

  assert_tab_profile_vectors(d, z, NULL, b, length(d))

  cr <- attr(channel, "crs", exact = TRUE)
  unit <- if (!is.null(cr)) {
    crs_length_unit(sf::st_sfc(xsection_to_linestring(channel), crs = cr))
  } else {
    NULL
  }
  input_unit <- units_deparse(d)
  if (is.null(unit) && !is.null(input_unit)) {
    unit <- input_unit
  }
  d <- coerce_axis_numeric(d, unit, "distance")
  z <- coerce_axis_numeric(z, NULL, "elevation")

  L <- as.numeric(sf::st_length(xsection_to_linestring(channel)))
  prof <- build_xs_profile_from_table_rows(d, z, b)

  if (snap_banks_to == "plan") {
    prof <- snap_profile_outer_banks_to_plan_width(prof, L)
    xsection(channel$plan, profile = prof)
  } else {
    W <- as.numeric(xt_width(prof))
    dw <- W - L
    crs_use <- if (!is.null(cr)) cr else sf::NA_crs_
    wrapped <- xchan(list(xsection(channel$plan, profile = NULL)), crs = crs_use)
    widened <- xt_widen(wrapped, dw = dw, side = "both")
    xsection(widened[[1]]$plan, profile = prof)
  }
}

#' @export
xt_add_profile.default <- function(channel, ...) {
  stop(
    "No `xt_add_profile()` method for class ",
    paste(class(channel), collapse = "/"),
    ".",
    call. = FALSE
  )
}

#' @noRd
clone_xchan_shell <- function(channel) {
  n <- length(channel)
  secs <- vector("list", n)
  for (i in seq_len(n)) {
    secs[[i]] <- xsection(channel[[i]]$plan, profile = NULL)
  }
  structure(
    secs,
    crs = attr(channel, "crs", exact = TRUE),
    axis = attr(channel, "axis", exact = TRUE),
    bankline = attr(channel, "bankline", exact = TRUE),
    section_i = attr(channel, "section_i", exact = TRUE),
    length_unit = attr(channel, "length_unit", exact = TRUE),
    class = class(channel)
  )
}

#' @noRd
profile_row_indices_for_key <- function(s_all, key, n_row) {
  ii <- seq_len(n_row)
  ii[as.character(s_all) == as.character(key)]
}

#' @noRd
assert_tab_profile_vectors <- function(d, z, s, b, n) {
  if (length(d) != n || length(z) != n) {
    stop("`distance` and `elevation` must have equal length.", call. = FALSE)
  }
  if (!is.null(s) && length(s) != n) {
    stop("`section` must have the same length as `distance`.", call. = FALSE)
  }
  if (length(b) != n) {
    stop("`banks` must have the same length as `distance`.", call. = FALSE)
  }
  if (!is.logical(b)) {
    stop("`banks` must be logical.", call. = FALSE)
  }
  invisible(NULL)
}

#' @noRd
coerce_axis_numeric <- function(x, unit, arg) {
  if (inherits(x, "units")) {
    if (is.null(unit)) {
      return(units::drop_units(x))
    }
    return(to_numeric_length(x, unit, arg = arg))
  }
  as.numeric(x)
}

#' @noRd
build_xs_profile_from_table_rows <- function(d, z, b) {
  if (!any(b, na.rm = TRUE)) {
    stop("At least one row must have `banks == TRUE`.", call. = FALSE)
  }
  if (anyNA(b)) {
    stop("`banks` must not contain NA.", call. = FALSE)
  }
  n_b <- sum(b)
  if (n_b %% 2L != 0L) {
    stop(
      "There must be an even number of bank rows per section (got ",
      n_b,
      ").",
      call. = FALSE
    )
  }
  coords <- cbind(as.numeric(d), as.numeric(z))
  if (anyNA(coords)) {
    stop("`distance` and `elevation` must be finite (non-NA).", call. = FALSE)
  }
  bank_idx <- seq_len(nrow(coords))[b]
  bankpoints <- coords[bank_idx, 1L, drop = TRUE]
  new_profile(coords, bankpoints = bankpoints)
}

#' @noRd
snap_profile_outer_banks_to_plan_width <- function(profile, plan_width, tol = 1e-6) {
  checkmate::assert_class(profile, "xs_profile")
  checkmate::assert_number(plan_width, lower = 0, finite = TRUE)
  h <- plan_width / 2
  li <- get_left_bank_index(profile)
  ri <- get_right_bank_index(profile)
  profile$coordinates[li, 1L] <- -h
  profile$coordinates[ri, 1L] <- h
  inner <- setdiff(profile$banks, c(li, ri))
  if (length(inner)) {
    di <- profile$coordinates[inner, 1L]
    if (any(di < -h - tol | di > h + tol)) {
      stop(
        "After snapping outer banks to the plan width, at least one inner bank ",
        "lies outside the bank-to-bank span; use `snap_banks_to = \"profile\"` ",
        "or fix survey bank distances.",
        call. = FALSE
      )
    }
  }
  tw <- which.min(profile$coordinates[, 2L])
  profile$thalwegs <- tw
  profile$thalweg_elev <- profile$coordinates[tw, 2L]
  profile
}
