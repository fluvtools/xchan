# """
# Script to implementing your 'bank tracing spline'
# tracer_spline() should return a function of one argument: channel.
# That function should return an sf polygon representing the bankline polygon.
# """

library(sf)
library(tidyverse)


seg <- st_sfc(
  st_linestring(matrix(c(-0.2, 0.3, 0.2, 1), byrow = TRUE, ncol = 2)),
  st_linestring(matrix(c(0.1, 0.1, 1, 1), byrow = TRUE, ncol = 2)),
  st_linestring(matrix(c(0.1, 0, 1.3, 0.7), byrow = TRUE, ncol = 2)),
  st_linestring(matrix(c(0.3, -0.3, 1.3, 0), byrow = TRUE, ncol = 2)),
  st_linestring(matrix(c(0, -0.6, 1, -0.5), byrow = TRUE, ncol = 2)),
  st_linestring(matrix(c(0, -0.9, 1, -1), byrow = TRUE, ncol = 2))
)

# but assume the cross sections are not in order
# us xt_distant_ds(channel)
# if we are generating the x-sections, we can
# assume that the start point is the first point
# but we have to force the user to get them in order
st_cast(seg[1], "POINT")[1]


left <- st_sfc(
  st_point(c(-0.2, 0.3)),
  st_point(c(0.1, 0.1)),
  st_point(c(0.1, 0)),
  st_point(c(0.3, -0.3)),
  st_point(c(0, -0.6)),
  st_point(c(0, -0.9))
  )

right <- st_sfc(
  st_point(c( 0.2, 1)),
  st_point(c( 1, 1)),
  st_point(c( 1.3, 0.7)),
  st_point(c( 1.3, 0)),
  st_point(c( 1, -0.5)),
  st_point(c( 1, -1))
)

CRS <- st_crs(right)

channel <- sxchan:: xt_channel(seg)
plot(channel, col = "lightblue", lwd = 2)
sxchan::tracer_spline(channel)

CRS <- st_crs(right)
N_lengthout <- 100

# connect cross sections' edges to generate new banklines
start_edge <- list()
end_edge <- list()


for (i in 1:length(left)) {
  # Extract start and end points manually
  start_point <- st_cast(
    st_geometry(left[i]),
    "POINT")[1]

  end_point <- st_cast(
    st_geometry(right[i]),
    "POINT")[1]

  # end_point <- st_cast(
  #   st_geometry(
  #     right)[i],
  #   "POINT"
  # )[length(
  #   st_cast(
  #     st_geometry(
  #       right)[i],
  #     "POINT"
  #   )
  # )]

  start_edge[[i]] <- start_point
  end_edge[[i]] <- end_point

}

plot(channel)
plot(st_sfc(do.call(c, end_edge)), add = TRUE)
plot(st_sfc(do.call(c, start_edge)), add = TRUE)

# Smoothen and convert start and end points into a valid sf object
for (i in 1:2) {
  if (i == 1) {
    edge <- start_edge
    out_text <- "start_edge"
  } else {
    edge <- end_edge
    out_text <- "end_edge"
  }

  edge_sf <- st_sf(
    data.frame(id = 1:length(edge)),
    geometry = st_sfc(do.call(c, edge))
  ) |>
    mutate(LONG = map(edge, st_coordinates)) |>
    separate(LONG, c("LONG", "LAT"), sep = ",") |>
    mutate(
      LONG = as.numeric(str_remove(LONG, "c\\(")),
      LAT = as.numeric(str_remove(LAT, "\\)"))
    )

  # edge_sf |> st_write(glue("output/{out_text}.shp"), delete_dsn = TRUE)


  x <- edge_sf$LONG
  y <- edge_sf$LAT

  # Parameter t: cumulative arc-length
  t <- cumsum(c(0, sqrt(diff(x)^2 + diff(y)^2)))


  # Fit cubic splines for x(t) and y(t)
  x_spline <- splinefun(t, x, method = "natural")
  y_spline <- splinefun(t, y, method = "natural")

  # Generate interpolated path
  t_fine <- seq(min(t), max(t), length.out = N_lengthout)
  x_smooth <- x_spline(t_fine)
  y_smooth <- y_spline(t_fine)


  # remake x and y into spatial
  new_edge <- tibble(
    LONG = x_smooth,
    LAT = y_smooth
  ) |>
    mutate(
      geometry = map2(LONG, LAT, ~ st_point(c(.x, .y)))
    ) |>
    select(geometry) |>
    st_as_sf() |>
    st_set_crs(st_crs(CRS)
    ) |>
    summarise(geometry = st_combine(geometry)) |>
    st_cast("LINESTRING")

  if (i == 1) {
    start_edge_lines <- new_edge
  } else {
    end_edge_lines <- new_edge
  }

}
plot(start_edge_lines, add = TRUE)
plot(end_edge_lines, add = TRUE)
plot(channel, col = "lightblue", lwd = 2, add = TRUE)

# plot
# png(
#   path("edges.png"),
#   width = 10, height = 10, units = "in", res = 600
# )
# par(mar = c(0.5, 0.5, 0.5, 0.5))
# plot(start_edge_lines)
# plot(end_edge_lines, add = TRUE)
#
# dev.off()

# st_union(start_edge_lines, end_edge_lines) |>
#   st_write("output/2-gbem/eroded_banks_50pct.shp", delete_dsn = TRUE)

