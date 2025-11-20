# Initialization
## set seed for reproducibility
source("R/simulate_multi_ppp_data.R")
set.seed(600)

## set simulation parameters
number_objectives <- 3
number_features <- 10
number_actions <- 15
cost_mean <- 100
cost_sd <- 5
success_min_probability <- 0.7
success_max_probability <- 0.99
funded_min_persistence_probability <- 0.5
funded_max_persistence_probability <- 0.9
not_funded_min_persistence_probability <- 0.01
not_funded_max_persistence_probability <- 0.4
locked_in_proportion <- 0.01
locked_out_proportion <- 0.01

# Simulate data
sim <- simulate_multi_ppp_data(
  number_objectives,
  number_features,
  number_actions,
  cost_mean,
  cost_sd,
  success_min_probability,
  success_max_probability,
  funded_min_persistence_probability,
  funded_max_persistence_probability,
  not_funded_min_persistence_probability,
  not_funded_max_persistence_probability,
  locked_in_proportion,
  locked_out_proportion
)

# Exports
sim_multi_projects <- sim$projects
sim_multi_actions <- sim$actions
sim_multi_features <- sim$features
sim_multi_tree <- sim$tree
save(sim_multi_projects, file = "data/sim_multi_projects.rda", compress = "xz")
save(sim_multi_actions, file = "data/sim_multi_actions.rda", compress = "xz")
save(sim_multi_features, file = "data/sim_multi_features.rda", compress = "xz")
save(sim_multi_tree, file = "data/sim_multi_tree.rda", compress = "xz")
