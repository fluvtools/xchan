# Load necessary libraries
library(sf)

# Adjusted coordinates for the cross-section
coords <- matrix(c(
  # Left side (steep descent into channel)
  -4, 11,
  -1, 10,    # left bank start (steep descent)
  1, 9,
  2, 7,     # left bank point
  4, 1,     # thalweg (deepest point of the channel)

  # Right side (concave-up near channel, switches concavity toward floodplain)
  6, 1.5,   # concave-up section rising gradually
  9, 4,     # continuation of concave-up section
  12, 6,    # right bank point
  18, 8,    # flattening floodplain
  24, 8.5   # extended flat floodplain
), ncol = 2, byrow = TRUE)

#' Inject bankpoint, potentially splitting a linesegment into two
#' if x doesn't already land on a node.
inject_bankpoint <- function(mat, x) {
  if (is.na(x) || length(x) != 1) stop("Expecting one x value.")
  if (x %in% mat[, 1]) return(mat)
  y <- approx(mat[, 1], mat[, 2], x)$y
  new_mat <- rbind(mat, c(x, y))
  new_mat[order(new_mat[, 1]), ]
}

#' Get thalweg
get_thalweg <- function(mat) {
  mat[mat[, 2] == min(mat[, 2]), ]
}

# Separate into left and right/
y_thalweg <- min(coords[, 2])
x_thalwegs <- coords[, 1][coords[, 2] == y_thalweg]
coords_left <- coords[coords[, 1] <= min(x_thalwegs), ]
coords_right <- coords[coords[, 1] >= max(x_thalwegs), ]
xs_left <- st_multilinestring(list(coords_left))
xs_right <- st_multilinestring(list(coords_right))
plot(st_sfc(xs_left, xs_right))
plot(xs_left, add = TRUE, col = "blue")
plot(xs_right, add = TRUE, col = "red")

# Specify x values of LB and RB
x_lb <- 1
x_rb <- 12

coords_left <- inject_bankpoint(coords_left, x_lb)
coords_right <- inject_bankpoint(coords_right, x_rb)

# Interpolation finds the corresponding y values:
pt_lb <- st_point(c(1, 9))
pt_rb <- st_point(c(12, 6))

plot(pt_lb, add = TRUE, col = "blue")
plot(pt_rb, add = TRUE, col = "red")

# Widen right bank: inject the new point and grab the new bankline first.
dw <- 4

x_rb_new <- x_rb + dw
coords_right_new <- coords_right
coords_right_new <- inject_bankpoint(coords_right_new, x_rb_new)

pt_rb_new <- st_point(coords_right_new[coords_right_new[, 1] == x_rb_new, ])

# Do the erosion:
coords_right_new[coords_right_new[, 1] <= pt_rb[1], 1] <- coords_right_new[coords_right_new[, 1] <= pt_rb[1], 1] + dw
xs_right_new <- st_multilinestring(list(coords_right_new))

# Plot
plot(xs_right_new, add = TRUE, col = "green4")
plot(pt_rb_new, add = TRUE, col = "green4")

# Add Bottom
bottom_new <- rbind(get_thalweg(coords_right_new), get_thalweg(coords_left))
plot(st_linestring(bottom_new), add = TRUE)
