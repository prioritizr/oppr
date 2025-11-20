min_set_mip_formulation <- function(project_data, action_data, feature_data) {
  # initialize problem based on phylogenetic diversity formulation
  model <- max_phylo_div_mip_formulation(
    project_data = project_data,
    action_data = action_data,
    tree = star_phylogeny(feature_data$name),
    budget = 100,
    n_approx_points = 5
  )

  # overwrite objective
  model$obj <- model$obj * 0
  model$obj[seq_along(action_data$cost)] <- action_data$cost

  # change constraints to be hard targets
  row_idx <- which(model$rownames == "C4")
  model$sense[row_idx] <- ">="
  model$rhs[row_idx] <- feature_data$target
  model$A[row_idx, grep("R_", model$colnames, fixed = TRUE)] <- 0

  # remove budget constraint
  model$sense <- model$sense[-length(model$sense)]
  model$rhs <- model$rhs[-length(model$rhs)]
  model$rownames <- model$rownames[-length(model$rownames)]
  model$A <- model$A[-nrow(model$A), , drop = FALSE]

  # remove unused variables
  col_idx <- which(startsWith(model$colnames, "R_"))
  model$obj <- model$obj[-col_idx]
  model$ub <- model$ub[-col_idx]
  model$lb <- model$lb[-col_idx]
  model$vtype <- model$vtype[-col_idx]
  model$A <- model$A[, -col_idx, drop = FALSE]

  # exports
  model
}
