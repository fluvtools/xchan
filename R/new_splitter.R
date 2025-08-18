#' Split channel erosion between left and right banks
#'
#' Convert a function into a splitter object that can be used to allocate
#' erosion between left and right banks.
#'
#' @param fun A function that takes a channel object and returns left bank
#' proportions.
#' @param name Optional name for the splitter
#' @returns A splitter object, which is a function of a channel object
#' that returns a vector of proportions, one for each cross section in the
#' channel, corresponding to the amount of erosion that's allocated to the
#' _left_ bank (by convention).
#' @examples
#' # Example of linearly switching from left to right bank.
#' # Assumes the channel cross sections are sorted from upstream to downstream.
#' f <- function(channel) {
#'   n <- xt_n_sections(channel)
#'   seq(0, 1, length.out = n)
#' }
#' new_splitter(f)
#'
#' # Example based on erodability of banks.
#' # First make some dummy data representing bank erodability.
#' channel <- demo_channel
#' n <- xt_n_sections(channel)
#' channel$erodability_left <- rep(1, 10, length.out = n)
#' channel$erodability_right <- rep(-3, 3, length.out = n)^2
#'
#' g <- function(channel) {
#'   l <- channel$erodability_left
#'   r <- channel$erodability_right
#'   l / (l + r)
#' }
#' s <- new_splitter(g)
#' s(channel)
#' @export
new_splitter <- function(fun, name = NULL) {
  checkmate::assert_function(fun)
  checkmate::assert_character(name, len = 1, null.ok = TRUE)

  structure(
    fun,
    name = name,
    class = append("sxchan_splitter", class(fun))
  )
}
