# Initialization
## set seed for reproducibility
source("R/simulate_ppp_data.R")
set.seed(600)

## set simulation parameters
number_features <- 5
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
sim <- simulate_ppp_data(number_features,
                         cost_mean,
                         cost_sd,
                         success_min_probability,
                         success_max_probability,
                         funded_min_persistence_probability,
                         funded_max_persistence_probability,
                         not_funded_min_persistence_probability,
                         not_funded_max_persistence_probability,
                         locked_in_proportion,
                         locked_out_proportion)

# Exports
sim_projects <- sim$projects
sim_actions <- sim$actions
sim_features <- sim$features
sim_tree <- sim$tree
save(sim_projects, file = "data/sim_projects.rda", compress = "xz")
save(sim_actions, file = "data/sim_actions.rda", compress = "xz")
save(sim_features, file = "data/sim_features.rda", compress = "xz")
save(sim_tree, file = "data/sim_tree.rda", compress = "xz")
