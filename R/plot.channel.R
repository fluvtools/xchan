#' @exportS3Method base::plot
plot.channel <- function(x, view = c("plan", "profile"), ...) {
  if (view == "plan") {
    plot_plan(x)
  }
  if (view == "profile") {
    plot_profile(x)
  }
}
