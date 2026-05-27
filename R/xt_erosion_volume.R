#' Calculate erosion volume from width change
#'
#' Estimates volume removed per cross section for a given total width increase `dw`,
#' split between banks according to `side`.
#'
#' @param channel An [`xchan`][xchan()] or [`xsection`][xsection()] with profile geometry.
#' @param dw Change in width; for [`xsection`][xsection()], a single positive value. For
#'   [`xchan`][xchan()], a single value recycled to every section or one value per cross
#'   section. Plain numeric uses the channel CRS length unit;
#'   [units::units()] lengths are converted automatically.
#' @param side A side specification controlling how widening is split between left and right
#'   banks: [side_left()], [side_right()], [side_both()], or `"left"`, `"right"`, `"both"`.
#' @returns For [`xchan`][xchan()], a numeric vector of erosion volumes (one per section).
#'   For [`xsection`][xsection()], length-one vector. Values carry [units::units()] of (CRS length
#'   unit)^3 when a linear CRS unit is defined.
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
#' xt_erosion_volume(channel, dw = 0.5, side = "left")
#' xt_erosion_volume(channel, dw = 0.5, side = side_left(0.75))
#' @export
xt_erosion_volume <- function(channel, dw, side = "both") {
  UseMethod("xt_erosion_volume")
}

#' @rdname xt_erosion_volume
#' @export
xt_erosion_volume.xchan <- function(channel, dw, side = "both") {
  checkmate::assert_class(channel, "xchan")
  unit <- channel_length_unit(channel)
  dw <- to_numeric_length(dw, unit, arg = "dw")
  raw <- erosion_volume_numeric(channel, dw, side)
  with_volume_units(raw, unit)
}

#' @rdname xt_erosion_volume
#' @export
xt_erosion_volume.xsection <- function(channel, dw, side = "both") {
  checkmate::assert_class(channel, "xsection")
  xc <- xchan(list(channel), crs = sf::NA_crs_)
  xt_erosion_volume(xc, dw = dw, side = side)
}

#' @rdname xt_erosion_volume
#' @exportS3Method xt_erosion_volume default
xt_erosion_volume.default <- function(channel, dw, side = "both") {
  stop(
    "No `xt_erosion_volume()` method for class ",
    paste(class(channel), collapse = "/"),
    ". Use an `xchan` or `xsection` object.",
    call. = FALSE
  )
}

#' @noRd
erosion_volume_numeric <- function(channel, dw, side = "both") {
  profile <- channel_profile(channel)
  if (is.null(profile)) {
    stop("Channel object must have profile cross sections")
  }

  prop_left <- parse_side_arg(side, channel)

  dw <- vctrs::vec_recycle(dw, length(profile))

  volumes <- numeric(length(profile))
  failures <- list()

  for (i in seq_along(profile)) {
    result <- tryCatch(
      {
        xs <- profile[[i]]
        dw_left <- dw[i] * prop_left[i]
        dw_right <- dw[i] - dw_left

        v1 <- erosion_volume_left(xs, dw_left)
        xs_flipped <- flip_profile(xs)
        v2 <- erosion_volume_left(xs_flipped, dw_right)

        list(ok = TRUE, value = v1 + v2)
      },
      error = function(e) {
        list(ok = FALSE, error = e)
      }
    )
    if (result$ok) {
      volumes[i] <- result$value
    } else {
      failures[[length(failures) + 1L]] <- list(
        label = section_label_at(channel, i),
        message = conditionMessage(result$error)
      )
      volumes[i] <- NA_real_
    }
  }

  stop_erosion_section_errors(failures)
  volumes
}

erosion_volume_left <- function(xs, dw) {
  checkmate::assert_numeric(dw, lower = 0, len = 1, any.missing = FALSE)

  if (dw == 0) {
    return(0)
  }

  x_old <- get_left_bank_coords(xs)[1]
  x_new <- x_old - dw
  nodes <- xs$coordinates
  x_extent <- min(nodes[, 1])
  if (x_new < x_extent) {
    stop(
      "Cannot calculate erosion volume for given change in width, as ",
      "the cross section extent is surpassed.",
      call. = FALSE
    )
  }

  y_thalweg <- xs$thalweg_elev
  nodes <- inject_coords(nodes, x_new)
  x_in_between <- nodes[, 1] >= x_new & nodes[, 1] <= x_old
  between_nodes <- nodes[x_in_between, , drop = FALSE]
  n <- nrow(between_nodes)
  if (n < 2) {
    return(0)
  }
  nm1 <- n - 1
  delta_x <- between_nodes[2:n, 1] - between_nodes[1:nm1, 1]
  delta_y <- abs(between_nodes[2:n, 2] - between_nodes[1:nm1, 2])
  avg_y <- (between_nodes[2:n, 2] + between_nodes[1:nm1, 2]) / 2
  y_upper <- pmax(between_nodes[2:n, 2], between_nodes[1:nm1, 2])
  below_thalweg <- between_nodes[, 2] < y_thalweg
  zero_area <- below_thalweg[1:nm1] + below_thalweg[2:n] == 2
  partially_above <- below_thalweg[1:nm1] + below_thalweg[2:n] == 1
  area <- ifelse(
    partially_above,
    delta_x * (y_upper - y_thalweg)^2 / delta_y / 2,
    delta_x * (avg_y - y_thalweg)
  )
  area[zero_area] <- 0
  sum(area)
}
