# Generates the figures used in the JOSS paper (paper.md).
# These are produced separately from the README figures so the paper can use
# paper-specific framing (tight cropping, vertical exaggeration, higher DPI)
# without changing the README narrative.

devtools::load_all(quiet = TRUE)
library(terra)

outdir <- "paper-figures"
dir.create(outdir, showWarnings = FALSE)

# Build the channel exactly as in the README example -----------------------
squamish <- xt_generate_plan(squamish_bankline, spacing = 100)
squamish <- xt_generate_profile(
  squamish,
  unwrap(squamish_dem),
  sample_freq = 10
)
squamish <- xt_dredge_to(squamish, bathy = bathy_rectangle(depth = 3))

# Figure 1: planimetric cross sections -------------------------------------
# Match the device aspect ratio to the data so equal-aspect (asp = 1) mapping
# does not pad the plot with whitespace, and use tight margins.
plan <- channel_plan(squamish)
bl <- xt_bankline(squamish)
axs <- xt_axis(squamish)
bb <- combine_plan_bbox(plan, axs, bl)
bb <- balance_plot_bbox(bb, max_ratio = 4, pad = 0.03)
dx <- bb[["xmax"]] - bb[["xmin"]]
dy <- bb[["ymax"]] - bb[["ymin"]]

w_in <- 5.5
h_in <- w_in * (dy / dx)

png(
  file.path(outdir, "squamish_plan.png"),
  width = w_in,
  height = h_in,
  units = "in",
  res = 300
)
op <- par(mar = c(0, 0, 0, 0))
plot(squamish)
par(op)
dev.off()

# Figure 2: profile cross section ------------------------------------------
# Smaller physical size but larger relative fonts. A -150..150 m window and a
# true vertical exaggeration make the 3 m dredged channel read as a channel
# rather than a flat line. `exaggerate` is passed through as `asp`, so the
# horizontal:vertical scale ratio is fixed at exactly this factor regardless of
# device. A 3 m feature across a ~300 m window needs a large factor to be
# legible; EXAG below is the single knob for it.
EXAG <- 2

png(
  file.path(outdir, "squamish_profile.png"),
  width = 4.6,
  height = 3.4,
  units = "in",
  res = 300,
  pointsize = 11
)
op <- par(mar = c(4, 4, 1, 1), cex.lab = 1.1, cex.axis = 1.0)
plot(
  squamish[[10]],
  view = "profile",
  extent = "full",
  exaggerate = EXAG,
  from = -150,
  to = 150
)
par(op)
dev.off()

message("Wrote figures to ", normalizePath(outdir))
