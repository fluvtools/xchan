xt_width_profile <- function(object) {
  checkmate::assert_class(object, "xs_profile")
  object$right$bank[1] - object$left$bank[1]
}
