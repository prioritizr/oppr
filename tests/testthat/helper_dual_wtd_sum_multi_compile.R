dual_wtd_sum_multi_compile <- function(project_data, action_data, feature_data,
                                       budget) {
  # assert arguments are valid
  assertthat::assert_that(
    is.list(project_data), is.data.frame(action_data), is.list(feature_data),
    length(project_data) == 2, length(feature_data) == 2,
    assertthat::is.number(budget)
  )

  # compile mip formulations
  o1 <- max_wtd_sum_mip_formulation(
    project_data[[1]], action_data, feature_data[[1]], budget
  )
  o2 <- max_wtd_sum_mip_formulation(
    project_data[[2]], action_data, feature_data[[2]], budget
  )

  # identify number of action status variables
  n_actions <- nrow(action_data)
  n_vars <- length(o1$obj) + (length(o2$obj) - n_actions)
  o1_new_var_idx <- seq_along(o1$obj)
  o2_new_var_idx <- c(
    seq_len(n_actions),
    length(o1$obj) + seq_len(length(o2$obj) - n_actions)
  )

  # prepare obj values
  obj <- matrix(0, nrow = 2, ncol = n_vars)
  obj[1, seq_along(o1$obj)] <- o1$obj
  obj[2, o2_new_var_idx] <- o2$obj

  # lb
  lb <- numeric(n_vars)
  lb[o1_new_var_idx] <- o1$lb
  lb[o2_new_var_idx] <- o2$lb
  lb[seq_len(n_actions)] <- pmax(o1$lb[n_actions], o2$lb[n_actions])

  # ub
  ub <- numeric(n_vars)
  ub[o1_new_var_idx] <- o1$ub
  ub[o2_new_var_idx] <- o2$ub
  ub[seq_len(n_actions)] <- pmin(o1$ub[n_actions], o2$ub[n_actions])

  # vtype
  vtype <- numeric(n_vars)
  vtype[o1_new_var_idx] <- o1$vtype
  vtype[o2_new_var_idx] <- o2$vtype

  # rhs
  rhs <- c(o1$rhs, o2$rhs)

  # sense
  sense <- c(o1$sense, o2$sense)

  # A
  A1 <- cbind(o1$A, matrix(0, nrow = nrow(o1$A), ncol = n_vars - ncol(o1$A)))
  A2 <- cbind(
    o2$A[, seq_len(n_actions), drop = FALSE],
    matrix(0, nrow = nrow(o2$A), ncol = n_vars - ncol(o2$A)),
    o2$A[, seq(n_actions + 1, ncol(o2$A)), drop = FALSE]
  )
  A <- rbind(A1, A2)

  # return result
  list(
    obj = obj,
    lb = lb,
    ub = ub,
    vtype = vtype,
    sense = sense,
    rhs = rhs,
    A = A
  )
}
