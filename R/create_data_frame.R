# Helper function to create tibble or data frame
create_data_frame <- function(...) {
  # Use vctrs for consistent recycling behavior
  recycled_args <- vctrs::vec_recycle_common(...)

  if (requireNamespace("tibble", quietly = TRUE)) {
    rlang::exec(tibble::tibble, !!!recycled_args)
  } else {
    rlang::exec(data.frame, !!!recycled_args)
  }
}
