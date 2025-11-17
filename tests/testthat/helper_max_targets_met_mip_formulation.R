max_targets_mip_formulation <- function(project_data, action_data,
                                        feature_data, budget) {
  # initialize problem based on phylogenetic diversity formulation
  model <- max_phylo_div_mip_formulation(
    project_data = project_data,
    action_data = action_data,
    tree = star_phylogeny(feature_data$name),
    budget = budget,
    n_approx_points = 5
  )

  # if present, assign weights
  if (assertthat::has_name(feature_data, "weight")) {
    model$obj[grep("R_", model$colnames, fixed = TRUE)] <- feature_data$weight
  }

  # overwrite variables
  col_idx <- which(startsWith(model$colnames, "R_"))
  model$vtype[col_idx] <- rep("B", length(col_idx))

  # overwrite constraints
  row_idx <- which(model$rownames == "C4")
  model$rhs[row_idx] <- rep(0, length(row_idx))
  model$sense[row_idx] <- rep(">=", length(row_idx))
  model$A[matrix(c(row_idx, col_idx), ncol = 2)] <- -1 * feature_data$target

  # Exports
  model
}
