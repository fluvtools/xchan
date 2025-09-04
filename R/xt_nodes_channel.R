xt_nodes_channel <- function(profile) {
  checkmate::assert_class(profile, "xs_profile")
  nodes <- profile$coordinates
  x_left <- profile$banks[1]
  x_right <- profile$banks[length(profile$banks)]
  i_left <- i_bank_left(nodes, x_left) + 1L
  i_right <- i_bank_right(nodes, x_right) - 1L
  if (i_right == i_left + 1L) {
    return(matrix(ncol = 2, nrow = 0))
  }
  nodes[i_left:i_right, , drop = FALSE]
}

#' Important because in rectangular channels, there are two points with the
#' same x value at the bank. We want the highest one to be the bank.
i_bank_left <- function(nodes, x_left) {
  i_left_all <- which(nodes[, 1] == x_left)
  j_left_bank <- which(nodes[i_left_all, 2] == max(nodes[i_left_all, 2]))
  i_left_all[j_left_bank]
}

i_bank_right <- function(nodes, x_right) {
  i_right_all <- which(nodes[, 1] == x_right)
  j_right_bank <- which(nodes[i_right_all, 2] == max(nodes[i_right_all, 2]))
  i_right_all[j_right_bank]
}

i_channel <- function(nodes, x_left, x_right) {
  i_left <- i_bank_left(nodes, x_left) + 1L
  i_right <- i_bank_right(nodes, x_right) - 1L
  if (i_right < i_left) {
    return(integer(0L))
  }
  i_left:i_right
}

`xt_nodes_channel<-` <- function(profile, value) {
  checkmate::assert_class(profile, "xs_profile")
  checkmate::assert_matrix(value, ncol = 2L, min.rows = 1L, mode = "numeric")
  nodes <- profile$coordinates
  x_left <- profile$banks[1]
  x_right <- profile$banks[length(profile$banks)]
  i_left <- i_bank_left(nodes, x_left)
  i_right <- i_bank_right(nodes, x_right)
  mat_left <- nodes[1:i_left, , drop = FALSE]
  mat_right <- nodes[i_right:nrow(nodes), , drop = FALSE]
  new_mat <- rbind(mat_left, value, mat_right)
  new_thal_x <- sort(value[which.min(value[, 2]), 1])
  profile$coordinates <- new_mat
  profile$thalwegs <- new_thal_x
  profile
}
