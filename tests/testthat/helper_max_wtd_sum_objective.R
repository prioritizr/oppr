max_wtd_sum_mip_formulation <- function(project_data, action_data, feature_data,
                                        budget) {
  # initialize problem based on max richness formulation
  model <- max_richness_mip_formulation(
    project_data = project_data,
    action_data = action_data,
    feature_data = feature_data,
    budget = budget
  )

  # set upper bound for feature outcome variables based on maximum
  # expected outcome values
  feature_max_outcome <- vapply(
    seq_len(nrow(feature_data)), FUN.VALUE = numeric(1), function(i) {
      max(
        project_data[[feature_data$name[[i]]]] * project_data$success,
        na.rm = TRUE
      )
    }
  )
  model$ub[model$vtype == "C"] <- feature_max_outcome

  # Exports
  model
}
