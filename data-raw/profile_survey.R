## code to prepare `profile_survey` dataset goes here

widths <- c(10, 12, 8, 15, 11, 9)
depths <- c(1, 1, 3, 2, 2, 3)

topo <- function(width, depth, ...) {
  d_left <- -(10:1)
  d_right <- 1:10 + width
  d_inner <- c(0, width / 3, width)
  e_left <- 0.5 + 1.5 * log(10:1 + 1)
  e_right <- rep(0, 10)
  e_inner <- c(0.5, -depth, 0)
  d <- c(d_left, d_inner, d_right)
  d_shifted <- d - min(d)
  e <- c(e_left, e_inner, e_right)
  data.frame(
    distance = units::set_units(d_shifted, "m"),
    elevation = units::set_units(e, "m"),
    is_bank = d == 0 | d == width,
    ...
  )
}

df_list <- mapply(
  topo,
  width = widths,
  depth = depths,
  id = seq_along(widths),
  SIMPLIFY = FALSE
)

profile_survey <- tibble::as_tibble(do.call(rbind, df_list))

profile_survey <- profile_survey[c("id", "distance", "elevation", "is_bank")]

usethis::use_data(profile_survey, overwrite = TRUE)
