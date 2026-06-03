# Squamish River — V-shaped dredged channel, promotional 3D visuals
#
# Standalone script (not part of the installed package).
#
# Outputs (under promotional/ by default):
#   squamish_vchannel_plotly.png      — static Plotly view
#   squamish_vchannel_plotly.html     — interactive (click-drag to rotate)
#   squamish_vchannel_rayshader.png   — rayshader hillshade + channel lines
#   squamish_vchannel_erosion.gif     — original vs 20 m bank erosion
#
# R packages: xchan (source), terra, sf, plotly, htmlwidgets, magick
# Optional: reticulate + Python plotly, kaleido, numpy (Plotly PNG export)
# Optional: rayshader (set options(rgl.useNULL = TRUE) for headless PNG)
#
# Usage (from repository root):
#   Rscript promotional/squamish_vchannel_3d.R
#   Rscript promotional/squamish_vchannel_3d.R --skip-rayshader
#   Rscript promotional/squamish_vchannel_3d.R --output-dir=promotional

options(rgl.useNULL = TRUE)
Sys.setenv(RGL_USE_NULL = "true")

`%||%` <- function(x, y) if (is.null(x)) y else x

pkg_root <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    normalizePath(file.path(dirname(sub("^--file=", "", file_arg[1L])), ".."))
  } else {
    normalizePath(getwd())
  }
})

parse_cli <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  out_dir <- file.path(pkg_root, "promotional")
  hit <- grep("^--output-dir=", args, value = TRUE)
  if (length(hit)) {
    out_dir <- sub("^--output-dir=", "", hit[1L])
  }
  list(
    out_dir = normalizePath(out_dir, mustWork = FALSE),
    skip_plotly = "--skip-plotly" %in% args,
    skip_rayshader = "--skip-rayshader" %in% args,
    skip_gif = "--skip-gif" %in% args
  )
}

require_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing required packages: ",
      paste(missing, collapse = ", "),
      "\nInstall with install.packages() as needed.",
      call. = FALSE
    )
  }
}

cli <- parse_cli()
require_packages(c("terra", "sf", "plotly", "htmlwidgets", "magick"))
if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Package \"devtools\" is required to load xchan from source.", call. = FALSE)
}
suppressPackageStartupMessages({
  devtools::load_all(pkg_root, quiet = TRUE)
  library(terra)
  library(sf)
  library(plotly)
  library(htmlwidgets)
  library(magick)
})

# ---- configuration -----------------------------------------------------------

PLAN_SPACING_M <- 100
PROFILE_SAMPLE_FREQ_M <- 10
PROFILE_EXTENT_M <- 75
DREDGE_DEPTH_M <- 3
THALWEG_FRAC <- 1 / 3
EROSION_DW_M <- 20
TERRAIN_MAX_CELLS <- 400L
TERRAIN_BUFFER_M <- 150
Z_EXAG <- 1
# Target vertical exaggeration relative to horizontal span (plotly scene units).
PLOTLY_RELIEF_FRAC <- 0.28
RAYSHADER_RELIEF_FRAC <- 0.32
RAYSHADER_PHI <- 38
GIF_HOLD_FRAMES <- 4L
GIF_FPS <- 10L
SIDE_SAMPLE_OFFSET_M <- 100

OUT_PLOTLY_PNG <- file.path(cli$out_dir, "squamish_vchannel_plotly.png")
OUT_PLOTLY_HTML <- file.path(cli$out_dir, "squamish_vchannel_plotly.html")
OUT_RAYSHADER_PNG <- file.path(cli$out_dir, "squamish_vchannel_rayshader.png")
OUT_GIF <- file.path(cli$out_dir, "squamish_vchannel_erosion.gif")

# ---- channel construction ----------------------------------------------------

build_squamish_vchannel <- function(
    bankline = squamish_bankline,
    dem = terra::unwrap(squamish_dem),
    plan_spacing = PLAN_SPACING_M,
    sample_freq = PROFILE_SAMPLE_FREQ_M,
    extent_distance = PROFILE_EXTENT_M,
    depth = DREDGE_DEPTH_M,
    thalweg_frac = THALWEG_FRAC) {
  channel <- xt_generate_plan(bankline, spacing = plan_spacing, progress = TRUE)
  channel <- xt_generate_profile(
    channel,
    dem,
    extent_distance = extent_distance,
    sample_freq = sample_freq,
    progress = TRUE
  )
  xt_dredge_to(
    channel,
    bathy = bathy_vshape(depth = depth, thalweg_frac = thalweg_frac)
  )
}

#' Bank side with lower surrounding terrain (away from mountains).
extract_z <- function(dem, xy) {
  vals <- terra::extract(dem, matrix(xy, ncol = 2L))
  as.numeric(vals[[ncol(vals)]])
}

infer_low_relief_side <- function(
    channel,
    dem,
    sample_offset_m = SIDE_SAMPLE_OFFSET_M) {
  plan <- channel_plan(channel)
  left_e <- numeric()
  right_e <- numeric()
  for (i in seq_along(plan)) {
    xy <- sf::st_coordinates(plan[[i]])
    if (nrow(xy) < 2L) {
      next
    }
    left_pt <- xy[1L, 1:2]
    right_pt <- xy[nrow(xy), 1:2]
    mid <- colMeans(xy[, 1:2, drop = FALSE])
    lr <- right_pt - left_pt
    norm <- sqrt(sum(lr^2))
    if (!is.finite(norm) || norm <= 0) {
      next
    }
    lr <- lr / norm
    left_sample <- mid - lr * sample_offset_m
    right_sample <- mid + lr * sample_offset_m
    left_e <- c(left_e, extract_z(dem, left_sample))
    right_e <- c(right_e, extract_z(dem, right_sample))
  }
  if (!length(left_e) || !length(right_e)) {
    return("right")
  }
  if (stats::median(left_e, na.rm = TRUE) < stats::median(right_e, na.rm = TRUE)) {
    "left"
  } else {
    "right"
  }
}

# ---- shared 3D geometry ------------------------------------------------------

section_xyz_list <- function(channel, extent = c("banks", "full", "wetted")) {
  geoms <- xt_as_sfc(channel, what = "3d", extent = extent)
  lapply(seq_along(geoms), function(i) {
    coords <- sf::st_coordinates(geoms[[i]])
    if (!"L1" %in% colnames(coords)) {
      return(coords[, c("X", "Y", "Z"), drop = FALSE])
    }
    parts <- split(seq_len(nrow(coords)), coords[, "L1"])
    do.call(rbind, lapply(parts, function(idx) {
      coords[idx, c("X", "Y", "Z"), drop = FALSE]
    }))
  })
}

shift_xyz <- function(x, y, z, origin, z_exag = Z_EXAG) {
  list(
    x = x - origin[1L],
    y = y - origin[2L],
    z = (z - origin[3L]) * z_exag
  )
}

crop_dem_to_footprint <- function(
    dem,
    footprint,
    buffer_m = TERRAIN_BUFFER_M) {
  footprint_sf <- st_as_sf(footprint)
  footprint_buf <- st_buffer(footprint_sf, dist = buffer_m)
  dem_ext <- terra::ext(dem)
  buf_bbox <- sf::st_bbox(footprint_buf)
  crop_ext <- terra::ext(
    max(dem_ext[1], buf_bbox["xmin"]),
    min(dem_ext[2], buf_bbox["xmax"]),
    max(dem_ext[3], buf_bbox["ymin"]),
    min(dem_ext[4], buf_bbox["ymax"])
  )
  dem_crop <- terra::crop(dem, crop_ext)
  dem_crop <- terra::mask(dem_crop, terra::vect(footprint_buf))
  pts <- terra::as.points(terra::ifel(is.na(dem_crop), NA, 1), na.rm = TRUE)
  terra::trim(terra::crop(dem_crop, terra::ext(pts)))
}

cap_elevation_matrix <- function(z_mat, probs = c(0.02, 0.98)) {
  vals <- z_mat[is.finite(z_mat)]
  if (length(vals) < 10L) {
    return(z_mat)
  }
  lim <- stats::quantile(vals, probs = probs, na.rm = TRUE)
  z_mat[z_mat < lim[1L] | z_mat > lim[2L]] <- NA_real_
  z_mat
}

terrain_surface_mats <- function(
    dem,
    footprint,
    buffer_m = TERRAIN_BUFFER_M,
    max_cells = TERRAIN_MAX_CELLS) {
  dem_crop <- crop_dem_to_footprint(dem, footprint, buffer_m = buffer_m)

  nc <- terra::ncol(dem_crop)
  nr <- terra::nrow(dem_crop)
  fact <- max(1L, ceiling(max(nc, nr) / max_cells))
  if (fact > 1L) {
    dem_crop <- terra::aggregate(
      dem_crop,
      fact = fact,
      fun = mean,
      na.rm = TRUE
    )
  }

  z_mat <- terra::as.matrix(dem_crop, wide = TRUE)
  z_mat <- cap_elevation_matrix(z_mat)

  dem_e <- terra::ext(dem_crop)
  list(
    dem = dem_crop,
    x = terra::xFromCol(dem_crop, seq_len(ncol(dem_crop))),
    y = terra::yFromRow(dem_crop, seq_len(nrow(dem_crop))),
    z = z_mat,
    ext = c(dem_e[1], dem_e[2], dem_e[3], dem_e[4])
  )
}

terrain_z_color_limits <- function(z_mat, probs = c(0.04, 0.96)) {
  vals <- z_mat[is.finite(z_mat)]
  if (length(vals) < 10L) {
    rng <- range(vals, na.rm = TRUE)
    return(list(zmin = rng[1L], zmax = rng[2L]))
  }
  lim <- stats::quantile(vals, probs = probs, na.rm = TRUE)
  list(zmin = lim[1L], zmax = lim[2L])
}

rayshader_zscale <- function(
    dem_crop,
    mat,
    relief_frac = RAYSHADER_RELIEF_FRAC) {
  dem_e <- terra::ext(dem_crop)
  horiz <- max(
    terra::xmax(dem_e) - terra::xmin(dem_e),
    terra::ymax(dem_e) - terra::ymin(dem_e)
  )
  vert <- max(mat, na.rm = TRUE)
  if (!is.finite(horiz) || horiz <= 0 || !is.finite(vert) || vert <= 0) {
    return(1)
  }
  (horiz / vert) * relief_frac
}

channel_flow_unit_vector <- function(channel) {
  xy <- sf::st_coordinates(xt_axis(channel))
  if (nrow(xy) < 2L) {
    return(c(1, 0))
  }
  v <- c(
    xy[nrow(xy), "X"] - xy[1, "X"],
    xy[nrow(xy), "Y"] - xy[1, "Y"]
  )
  n <- sqrt(sum(v^2))
  if (!is.finite(n) || n <= 0) {
    return(c(1, 0))
  }
  v / n
}

plotly_camera_for_flow <- function(flow_uv) {
  upstream <- c(-flow_uv[1], -flow_uv[2], 0)
  lateral <- c(-flow_uv[2], flow_uv[1], 0)
  eye <- upstream * 1.35 + lateral * 0.75 + c(0, 0, 0.45)
  n <- sqrt(sum(eye^2))
  eye <- eye / n * 1.85
  list(
    eye = as.list(stats::setNames(eye, c("x", "y", "z"))),
    center = list(x = 0, y = 0, z = -0.05)
  )
}

rayshader_theta_for_flow <- function(flow_uv) {
  deg <- atan2(flow_uv[2], flow_uv[1]) * 180 / pi
  (deg + 120) %% 360
}

plotly_aspectratio <- function(terrain, z_limits) {
  xr <- diff(range(terrain$x))
  yr <- diff(range(terrain$y))
  if (!is.finite(xr) || xr <= 0) {
    xr <- 1
  }
  if (!is.finite(yr) || yr <= 0) {
    yr <- 1
  }
  horiz <- max(xr, yr)
  relief <- z_limits$zmax - z_limits$zmin
  if (!is.finite(relief) || relief <= 0) {
    relief <- 1
  }
  z_aspect <- max(0.12, min(0.5, (relief / horiz) * PLOTLY_RELIEF_FRAC * 12))
  list(x = xr / horiz, y = yr / horiz, z = z_aspect)
}

terrain_colorscale <- function() {
  list(
    list(0, "#0d2137"),
    list(0.15, "#1b4332"),
    list(0.3, "#2d6a4f"),
    list(0.45, "#40916c"),
    list(0.58, "#52b788"),
    list(0.7, "#95d5b2"),
    list(0.82, "#d4a373"),
    list(0.92, "#9c6644"),
    list(1, "#e9ecef")
  )
}

bankline_xyz_list <- function(channel, sections = NULL) {
  if (is.null(sections)) {
    sections <- section_xyz_list(channel, extent = "banks")
  }
  left <- list()
  right <- list()
  for (i in seq_along(sections)) {
    xyz <- sections[[i]]
    if (is.null(xyz) || nrow(xyz) < 2L) {
      next
    }
    left[[length(left) + 1L]] <- xyz[1L, , drop = TRUE]
    right[[length(right) + 1L]] <- xyz[nrow(xyz), , drop = TRUE]
  }
  list(
    left = do.call(rbind, left),
    right = do.call(rbind, right)
  )
}

thalweg_xyz_list <- function(channel, sections = NULL) {
  if (is.null(sections)) {
    sections <- section_xyz_list(channel, extent = "banks")
  }
  rows <- lapply(seq_along(channel), function(i) {
    prof <- channel[[i]]$profile
    xyz <- sections[[i]]
    if (is.null(prof) || is.null(xyz) || nrow(xyz) < 1L) {
      return(NULL)
    }
    coords <- prof$coordinates
    banks <- prof$banks
    left <- min(coords[banks, 1L])
    right <- max(coords[banks, 1L])
    bank_coords <- coords[coords[, 1L] >= left & coords[, 1L] <= right, , drop = FALSE]
    target <- left + THALWEG_FRAC * (right - left)
    j <- which.min(abs(bank_coords[, 1L] - target))
    j <- min(j, nrow(xyz))
    xyz[j, , drop = TRUE]
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1L))])
}

axis_xyz <- function(channel, z_ref) {
  axis_line <- xt_axis(channel)
  xy <- sf::st_coordinates(axis_line)
  cbind(X = xy[, "X"], Y = xy[, "Y"], Z = rep(z_ref, nrow(xy)))
}

channel_scene_origin <- function(channel, terrain, bed_extent = "banks") {
  section_lines <- section_xyz_list(channel, extent = bed_extent)
  banks <- bankline_xyz_list(channel, sections = section_lines)
  thalweg <- thalweg_xyz_list(channel, sections = section_lines)
  all_z <- c(terrain$z, thalweg[, 3L], banks$left[, 3L], banks$right[, 3L])
  list(
    origin = c(
      mean(range(terrain$x)),
      mean(range(terrain$y)),
      stats::median(all_z, na.rm = TRUE)
    ),
    section_lines = section_lines,
    banks = banks,
    thalweg = thalweg
  )
}

plotly_scene <- function(
    channel,
    dem,
    subtitle,
    origin,
    banks,
    thalweg,
    terrain,
    flow_uv = channel_flow_unit_vector(channel)) {
  terrain_z <- terrain$z[nrow(terrain$z):1L, , drop = FALSE]
  terrain_y <- rev(terrain$y)
  terrain_z_shift <- (terrain_z - origin[3L]) * Z_EXAG
  z_limits <- terrain_z_color_limits(terrain_z_shift)
  z_min <- z_limits$zmin
  z_max <- z_limits$zmax

  thalweg_shift <- shift_xyz(
    thalweg[, 1L], thalweg[, 2L], thalweg[, 3L], origin
  )
  left_shift <- shift_xyz(
    banks$left[, 1L], banks$left[, 2L], banks$left[, 3L], origin
  )
  right_shift <- shift_xyz(
    banks$right[, 1L], banks$right[, 2L], banks$right[, 3L], origin
  )
  axis_coords <- axis_xyz(channel, stats::median(c(terrain$z, thalweg[, 3L]), na.rm = TRUE))
  axis_shift <- shift_xyz(
    axis_coords[, "X"], axis_coords[, "Y"], axis_coords[, "Z"], origin
  )

  plot_ly() |>
    add_trace(
      type = "surface",
      x = terrain$x - origin[1L],
      y = terrain_y - origin[2L],
      z = terrain_z_shift,
      colorscale = terrain_colorscale(),
      cmin = z_min,
      cmax = z_max,
      showscale = FALSE,
      lighting = list(
        ambient = 0.28,
        diffuse = 0.92,
        specular = 0.18,
        roughness = 0.55,
        fresnel = 0.08
      ),
      opacity = 1,
      connectgaps = FALSE
    ) |>
    add_trace(
      type = "scatter3d",
      mode = "lines",
      x = left_shift$x,
      y = left_shift$y,
      z = left_shift$z,
      line = list(color = "#f4d35e", width = 4),
      showlegend = FALSE,
      hoverinfo = "skip"
    ) |>
    add_trace(
      type = "scatter3d",
      mode = "lines",
      x = right_shift$x,
      y = right_shift$y,
      z = right_shift$z,
      line = list(color = "#f4d35e", width = 4),
      showlegend = FALSE,
      hoverinfo = "skip"
    ) |>
    add_trace(
      type = "scatter3d",
      mode = "lines",
      x = thalweg_shift$x,
      y = thalweg_shift$y,
      z = thalweg_shift$z,
      line = list(color = "#ff6b35", width = 8),
      showlegend = FALSE,
      hoverinfo = "skip"
    ) |>
    add_trace(
      type = "scatter3d",
      mode = "lines",
      x = axis_shift$x,
      y = axis_shift$y,
      z = axis_shift$z,
      line = list(color = "#ffffff", width = 2),
      opacity = 0.4,
      showlegend = FALSE,
      hoverinfo = "skip"
    ) |>
    layout(
      title = list(
        text = paste0("<b>Squamish River</b><br><sup>", subtitle, "</sup>"),
        font = list(color = "#f8f9fa", size = 22),
        x = 0.02,
        xanchor = "left"
      ),
      paper_bgcolor = "#0b1220",
      plot_bgcolor = "#0b1220",
      scene = list(
        bgcolor = "#0b1220",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        zaxis = list(
          title = list(text = "Elevation (m)", font = list(color = "#adb5bd")),
          gridcolor = "#1f2937",
          tickfont = list(color = "#adb5bd")
        ),
        camera = plotly_camera_for_flow(flow_uv),
        aspectmode = "manual",
        aspectratio = plotly_aspectratio(terrain, z_limits)
      ),
      margin = list(l = 0, r = 0, b = 0, t = 70),
      showlegend = FALSE
    )
}

export_plotly <- function(p, png_path, html_path) {
  dir.create(dirname(png_path), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(p, html_path, selfcontained = TRUE)
  message("Wrote interactive view: ", normalizePath(html_path, mustWork = FALSE))

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    message("Skipping PNG (reticulate not installed); use the HTML for a static export.")
    return(invisible(html_path))
  }
  exported <- tryCatch({
    plotly::save_image(
      p,
      png_path,
      width = 2400L,
      height = 1800L,
      engine = "kaleido"
    )
    TRUE
  }, error = function(e) {
    message("Plotly PNG export failed: ", conditionMessage(e))
    FALSE
  })
  if (exported) {
    message("Wrote ", normalizePath(png_path, mustWork = FALSE))
  }
  invisible(if (exported) png_path else html_path)
}

plot_promotional_plotly <- function(
    channel,
    dem,
    png_path = OUT_PLOTLY_PNG,
    html_path = OUT_PLOTLY_HTML,
    subtitle = sprintf(
      "3 m V-channel, thalweg at 1/3 bank-to-bank · xchan"
    )) {
  footprint <- xt_bankline(channel) %||% squamish_bankline
  terrain <- terrain_surface_mats(dem, footprint)
  scene <- channel_scene_origin(channel, terrain)
  p <- plotly_scene(
    channel,
    dem,
    subtitle = subtitle,
    origin = scene$origin,
    banks = scene$banks,
    thalweg = scene$thalweg,
    terrain = terrain
  )
  export_plotly(p, png_path, html_path)
}

plot_promotional_rayshader <- function(
    channel,
    dem,
    output_path = OUT_RAYSHADER_PNG,
    phi = RAYSHADER_PHI,
    theta = rayshader_theta_for_flow(channel_flow_unit_vector(channel))) {
  if (!requireNamespace("rayshader", quietly = TRUE)) {
    stop(
      "Package \"rayshader\" is required for this plot. ",
      "Install with install.packages(\"rayshader\").",
      call. = FALSE
    )
  }
  options(rgl.useNULL = TRUE)
  Sys.setenv(RGL_USE_NULL = "true")
  suppressPackageStartupMessages(library(rayshader))

  footprint <- xt_bankline(channel) %||% squamish_bankline
  terrain <- terrain_surface_mats(dem, footprint)
  scene <- channel_scene_origin(channel, terrain)

  mat <- cap_elevation_matrix(terra::as.matrix(terrain$dem, wide = TRUE))
  mat <- mat[nrow(mat):1L, , drop = FALSE]
  z_base <- min(mat, na.rm = TRUE)
  mat_rel <- mat - z_base
  mat_rel[!is.finite(mat_rel)] <- NA_real_
  mat_plot <- mat_rel
  mat_plot[is.na(mat_plot)] <- 0

  zscale <- rayshader_zscale(terrain$dem, mat_rel)
  dem_e <- terra::ext(terrain$dem)
  ext_vec <- c(dem_e[1], dem_e[2], dem_e[3], dem_e[4])

  shade <- rayshader::sphere_shade(
    mat_plot,
    sunangle = 300,
    texture = "desert"
  )

  rayshader::plot_3d(
    shade,
    mat_plot,
    zscale = zscale,
    windowsize = c(1200, 900),
    phi = phi,
    theta = theta,
    fov = 25,
    solid = FALSE,
    shadow = FALSE,
    background = "#0b1220"
  )

  draw_path <- function(df, color, width) {
    if (is.null(df) || nrow(df) < 2L) {
      return(invisible(NULL))
    }
    rayshader::render_path(
      lat = df[, 2L],
      long = df[, 1L],
      altitude = df[, 3L] - z_base,
      extent = ext_vec,
      heightmap = mat_plot,
      zscale = zscale,
      linewidth = width,
      color = color,
      antialias = TRUE,
      clear_previous = FALSE
    )
  }

  draw_path(scene$banks$left, "#f4d35e", 5)
  draw_path(scene$banks$right, "#f4d35e", 5)
  draw_path(scene$thalweg, "#ff6b35", 7)

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  rayshader::render_snapshot(
    output_path,
    software_render = TRUE,
    clear = TRUE,
    width = 2400,
    height = 1800
  )
  message("Wrote ", normalizePath(output_path, mustWork = FALSE))
  invisible(output_path)
}

plot_erosion_gif <- function(
    channel,
    channel_eroded,
    dem,
    output_path = OUT_GIF,
    erosion_side = "right",
    hold_frames = GIF_HOLD_FRAMES,
    fps = GIF_FPS) {
  footprint <- xt_bankline(channel) %||% squamish_bankline
  terrain <- terrain_surface_mats(dem, footprint)
  scene <- channel_scene_origin(channel, terrain)

  subtitle_base <- sprintf(
    "3 m V-channel · thalweg 1/3 · %d m erosion (%s bank)",
    EROSION_DW_M,
    erosion_side
  )

  p1 <- plotly_scene(
    channel,
    dem,
    subtitle = paste0(subtitle_base, " · original"),
    origin = scene$origin,
    banks = scene$banks,
    thalweg = scene$thalweg,
    terrain = terrain
  )
  scene_e <- channel_scene_origin(channel_eroded, terrain)
  p2 <- plotly_scene(
    channel_eroded,
    dem,
    subtitle = paste0(subtitle_base, " · eroded"),
    origin = scene_e$origin,
    banks = scene_e$banks,
    thalweg = scene_e$thalweg,
    terrain = terrain
  )

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(
      "GIF export needs Plotly PNG via kaleido (reticulate + Python plotly/kaleido).",
      call. = FALSE
    )
  }

  f1 <- tempfile(fileext = ".png")
  f2 <- tempfile(fileext = ".png")
  on.exit({
    unlink(c(f1, f2))
  }, add = TRUE)

  plotly::save_image(p1, f1, width = 1200L, height = 900L, engine = "kaleido")
  plotly::save_image(p2, f2, width = 1200L, height = 900L, engine = "kaleido")

  img1 <- image_read(f1)
  img2 <- image_read(f2)
  frames <- c(
    rep(list(img1), hold_frames),
    rep(list(img2), hold_frames)
  )
  animation <- image_animate(
    image_join(frames),
    fps = fps,
    dispose = "previous"
  )
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  image_write(animation, output_path)
  message("Wrote ", normalizePath(output_path, mustWork = FALSE))
  invisible(output_path)
}

# ---- run (when executed as a script) -----------------------------------------

run_all <- function() {
  dir.create(cli$out_dir, recursive = TRUE, showWarnings = FALSE)

  message("Building Squamish channel with V-shaped dredge ...")
  channel <- build_squamish_vchannel()
  dem <- terra::unwrap(squamish_dem)

  erosion_side <- infer_low_relief_side(channel, dem)
  message(
    "Low-relief bank for erosion animation: ",
    erosion_side,
    " (away from higher terrain)"
  )
  channel_eroded <- xt_widen(channel, dw = EROSION_DW_M, side = erosion_side)

  if (!cli$skip_plotly) {
    message("Plotly: static PNG + interactive HTML ...")
    plot_promotional_plotly(channel, dem)
  }

  if (!cli$skip_rayshader) {
    message("Rayshader: static PNG (software render) ...")
    tryCatch(
      plot_promotional_rayshader(channel, dem),
      error = function(e) {
        message(
          "Rayshader plot skipped: ",
          conditionMessage(e),
          "\nOn macOS, install XQuartz if needed: brew install --cask xquartz"
        )
      }
    )
  }

  if (!cli$skip_gif) {
    message("GIF: alternating original and eroded channel ...")
    tryCatch(
      plot_erosion_gif(
        channel,
        channel_eroded,
        dem,
        erosion_side = erosion_side
      ),
      error = function(e) {
        message("GIF export failed: ", conditionMessage(e))
      }
    )
  }

  message("Done.")
}

if (sys.nframe() == 0L) {
  run_all()
}
