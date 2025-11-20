max_richness_mip_formulation <- function(project_data, action_data,
                                         feature_data, budget) {
  # initialize problem based on phylogenetic diversity formulation
  model <- max_phylo_div_mip_formulation(
    project_data = project_data,
    action_data = action_data,
    tree = star_phylogeny(feature_data$name),
    budget = budget,
    n_approx_points = 2
  )

  # if present, assign weights
  if (assertthat::has_name(feature_data, "weight")) {
    model$obj[grep("R_", model$colnames, fixed = TRUE)] <- feature_data$weight
  }

  # Exports
  model
}
