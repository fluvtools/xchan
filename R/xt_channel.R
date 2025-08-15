#' Create channel cross sections object
#'
#' Channel cross sections are a lightweight wrapper on sf geometries
#' (`sfc` objects), specifically a collection of line segments. This means
#' that you can use functions from sf to manipulate these objects whenever
#' special cross section methods do not exist.
#'
#' @param x Object to create cross sections out of. Can be a vector of
#' (positive) widths if the spatial orientation is not important;
#' can be line segments created using the sf package.
#' @param ... Optional arguments passed to `sf::st_sfc()` relating to the
#' geometrical properties of the cross sections, such as `crs`.
#' @returns A channel cross section object, with class "sxc", which is a
#' subclass of sf's "sfc_LINESTRING" class.
#' The nomenclature is inspired by the sf package, so that
#' "sxc" stands for "spatial cross-section column".
#' @examples
#' # Create cross sections without worrying about spatial orientation.
#' (a <- xt_sxc(1:3))
#' print(a)
#'
#' # Create cross sections from sf line segments, this time with a
#' # coordinate reference system. Note that even though we input a
#' # multilinestring, sxchan parses it into linestrings, so that each
#' # cross section is a linestring.
#' library(sf)
#' seg <- st_multilinestring(list(
#'   matrix(c(0, 1, 0, 1), ncol = 2),
#'   matrix(c(0, 1.5, -0.5, 0), ncol = 2)
#' ))
#' b <- xt_sxc(seg, crs = 3005)
#' plot(b)
#'
#' # Because these objects are just sxc objects from the sf package,
#' # we can manipulate them with sf.
#' # - Subset to grab individual cross section line segments:
#' b[[1]]
#' b[[2]]
#'
#' # - Add arbitrary features to the cross sections.
#' (b2 <- st_sf(b, roughness = 0.1, swimmability = c(4, 2)))
#'
#' # - Want to add / change more columns / features? It's just a data frame:
#' b2$rockiness <- c(1, 1.1)
#' b2
#' plot(b2)
#'
#' # - The geometry column is still a cross section object
#' st_geometry(b2)
#' @rdname xt_sxc
#' @export
xt_channel <- function(plan, profile, ...) UseMethod("xt_sxc")

#' @export
xt_channel.default <- function(x, ...) {
  if (any(x <= 0)) stop("Must have a positive width.")
  ## Construct an sfc object with linestring geometry of specified widths.
  segs <- list()
  for (i in seq_along(x)) {
    segs[[i]] <- sf::st_linestring(matrix(c(0, x[i], i, i), ncol = 2))
  }
  geom <- sf::st_sfc(segs, ...)
  new_sxc(geom)
}

#' @export
xt_channel.sfc <- function(x, ...) {
  is_multi <- vapply(
    x, \(x_) inherits(x_, "MULTILINESTRING"), FUN.VALUE = logical(1L)
  )
  if (any(is_multi)) {
    x <- sf::st_cast(sf::st_cast(x, "MULTILINESTRING"), "LINESTRING")
  }
  updated_sfc <- sf::st_sfc(x, ...)
  new_sxc(updated_sfc)
}

#' @export
xt_channel.sfg <- function(x, ...) {
  sfc <- sf::st_sfc(x)
  xt_sxc(sfc, ...)
}

#' @export
xt_channel.sxc <- function(x, ...) {
  geom <- sf::st_sfc(x, ...)
  new_sxc(geom)
}
