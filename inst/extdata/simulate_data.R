# Initialization
## set seed for reproducibility
source("R/ppp_simulate_data.R")
set.seed(600)

## set simulation parameters
number_species <- 5
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
sim <- ppp_simulate_data(number_species,
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
sim_project_data <- sim$project_data
sim_action_data <- sim$action_data
sim_tree <- sim$tree
sim_species_data <- sim$species_data
save(sim_project_data, file = "data/sim_project_data.rda", compress = "xz")
save(sim_action_data, file = "data/sim_action_data.rda", compress = "xz")
save(sim_species_data, file = "data/sim_species_data.rda", compress = "xz")
save(sim_tree, file = "data/sim_tree.rda", compress = "xz")
