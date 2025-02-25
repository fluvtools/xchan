#This function takes the original x-sec object and the eroded one resulting from whether (erode_left_xs) or (erode_right_xs)
#' @export
get_eroded_area <- function(xsec1, xsec2){

  xsec1_df <- data.frame(
    relative_distance = c(xsec1[["left"]][["multiline"]][,1], xsec1[["right"]][["multiline"]][,1]),
    elevation =         c(xsec1[["left"]][["multiline"]][,2], xsec1[["right"]][["multiline"]][,2])
  )
  xsec1_df <- xsec1_df[!duplicated(xsec1_df), ]
  xsec1_df$relative_distance <- xsec1_df$relative_distance - min(xsec1_df$relative_distance)
  xsec1_df$elevation <- xsec1_df$elevation -  min(xsec1_df$elevation)


  xsec2_df <- data.frame(
    relative_distance = c(xsec2[["left"]][["multiline"]][,1], xsec2[["right"]][["multiline"]][,1]),
    elevation =         c(xsec2[["left"]][["multiline"]][,2], xsec2[["right"]][["multiline"]][,2])
  )
  xsec2_df <- xsec2_df[!duplicated(xsec2_df), ]
  xsec2_df$relative_distance <- xsec2_df$relative_distance - min(xsec2_df$relative_distance)
  xsec2_df$elevation <- xsec2_df$elevation - min(xsec2_df$elevation)


  shoelace_area <- function(df) {
    n <- nrow(df)
    area <- 0.5 * abs(sum(df$relative_distance[1:(n-1)] * df$elevation[2:n]) -
                        sum(df$relative_distance[2:n] * df$elevation[1:(n-1)]))
    return(area)
  }

  # Ensure the shape is closed (repeat the first point at the end)
  xsec1_df <- rbind(xsec1_df, xsec1_df[1, ])

  # Compute the area
  area1 <- shoelace_area(xsec1_df)

  # Ensure the shape is closed (repeat the first point at the end)
  xsec2_df <- rbind(xsec2_df, xsec2_df[1, ])

  # Compute the area
  area2 <- shoelace_area(xsec2_df)

  ErodedArea <- abs(area2 - area1)
  return(ErodedArea)
}
