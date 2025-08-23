library(sf)
library(tidyverse)
(sf::st_coordinates(demo_bankline)[, 1:2] |>
  as_tibble() |>
  ggplot(aes(X, Y)) +
  geom_point()) |>
  plotly::ggplotly()

x0 <- 656890.9
y0 <- 5553227
x1 <- 657334.9
y1 <- 5552767

# Equation of the line representing the foot of the hill.
m <- (y1 - y0) / (x1 - x0)
int <- y0 - m * x0

# Ground and mountaintop heights
ground <- 500
mountaintop <- 800

height <- function(x, y) {
  #((-x + y / m + (ground - int / m)) - ground)*2 + ground
  a <- 1.1
  -a * x + a * y / m - a * int / m + ground
}

# Get bounding box of bankline
bb <- sf::st_bbox(demo_bankline)

# Extend bbox by 200 units east-west, and 80 units north-south
bb["xmin"] <- bb["xmin"] - 300
bb["xmax"] <- bb["xmax"] + 300
bb["ymin"] <- bb["ymin"] - 160
bb["ymax"] <- bb["ymax"] + 160

# Make a grid of (x,y) points within bbox
x_seq <- seq(bb["xmin"], bb["xmax"], length.out = 100)
y_seq <- seq(bb["ymin"], bb["ymax"], length.out = 100)
grid <- expand.grid(x = x_seq, y = y_seq)

grid$z <- pmin(pmax(ground, height(grid$x, grid$y)), mountaintop)

ggplot(grid, aes(x, y)) +
  geom_point(aes(colour = z))






# install.packages("gstat")
library(gstat)
library(sp)
df <- grid[c("x", "y")]
# Convert df to spatial object
coordinates(df) <- ~x+y

# Define variogram model (exponential with sill=1, range=2, nugget=0)
vgm_model <- vgm(psill = 50, model = "Exp", range = 1000, nugget = 0)




# Simulate GRF
# set.seed(253)
# set.seed(257)
set.seed(259)
sim <- gstat(formula = z~1, locations = ~x+y, dummy = TRUE, beta = 0, model = vgm_model, nmax = 20)
yhat <- pmax(predict(sim, newdata = df, nsim = 1)$sim1, 0)^1.5
grid$zoo <- ifelse(yhat == 0, yhat, yhat + 10) * 5
grid$z2 <- grid$zoo + grid$z

# ggplot(grid, aes(x, y)) +
#   geom_point(aes(colour = zoo))
#
#
# ggplot(grid, aes(x, y)) +
#   geom_point(aes(colour = z2))





# Do it again with a different seed???
set.seed(253)
sim <- gstat(formula = z~1, locations = ~x+y, dummy = TRUE, beta = 0, model = vgm_model, nmax = 20)
yhat <- pmax(predict(sim, newdata = df, nsim = 1)$sim1, 0)^1.5
grid$zoo <- ifelse(yhat == 0, yhat, yhat + 10) * 5
grid$z2 <- grid$zoo + grid$z2






# Make raster
library(terra)

# Cut off the bottom left portion on top of the mountain
grid2 <- grid |>
  filter(
    y > m * x + int - 500,
    y < m * x + int + 1300
  )
# ggplot(grid2, aes(x, y)) +
#   geom_point(aes(colour = z2))




# Suppose df has x, y, z
# Make sure coordinates are on a regular grid (required for raster)
grid2 <- grid2 |>
  select(x, y, z = z2)

# Convert to SpatRaster
r <- rast(grid2, type = "xyz")

# Check
demo_dem <- grid2
plot(demo_dem)

plot(demo_bankline, add = T)


# Save to data folder
usethis::use_data(demo_dem, overwrite = TRUE)
