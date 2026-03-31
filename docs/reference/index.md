# Package index

## Summary

Summary of the package

- [`oppr`](https://prioritizr.github.io/oppr/reference/oppr.md)
  [`oppr-package`](https://prioritizr.github.io/oppr/reference/oppr.md)
  : oppr: Optimal Project Prioritization

## Data

Simulated datasets and data simulation functions

- [`sim_actions`](https://prioritizr.github.io/oppr/reference/sim_data.md)
  [`sim_projects`](https://prioritizr.github.io/oppr/reference/sim_data.md)
  [`sim_features`](https://prioritizr.github.io/oppr/reference/sim_data.md)
  [`sim_tree`](https://prioritizr.github.io/oppr/reference/sim_data.md)
  : Simulated data
- [`sim_multi_actions`](https://prioritizr.github.io/oppr/reference/sim_multi_data.md)
  [`sim_multi_projects`](https://prioritizr.github.io/oppr/reference/sim_multi_data.md)
  [`sim_multi_features`](https://prioritizr.github.io/oppr/reference/sim_multi_data.md)
  [`sim_multi_tree`](https://prioritizr.github.io/oppr/reference/sim_multi_data.md)
  : Simulated multi-objective data
- [`simulate_multi_ppp_data()`](https://prioritizr.github.io/oppr/reference/simulate_multi_ppp_data.md)
  : Simulate multi-objective data for the 'Project Prioritization
  Protocol'
- [`simulate_ppp_data()`](https://prioritizr.github.io/oppr/reference/simulate_ppp_data.md)
  : Simulate data for the 'Project Prioritization Protocol'
- [`simulate_ptm_data()`](https://prioritizr.github.io/oppr/reference/simulate_ptm_data.md)
  : Simulate data for 'Priority threat management'

## Create and solve problems

Functions for creating new problems and solving them

- [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
  : Project prioritization problem
- [`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
  : Multi-objective project prioritization problem
- [`solve(`*`<OptimizationProblem>`*`,`*`<Solver>`*`)`](https://prioritizr.github.io/oppr/reference/solve.md)
  [`solve(`*`<ProjectProblem>`*`,`*`<missing>`*`)`](https://prioritizr.github.io/oppr/reference/solve.md)
  [`solve(`*`<MultiObjProjectProblem>`*`,`*`<missing>`*`)`](https://prioritizr.github.io/oppr/reference/solve.md)
  : Solve

## Objectives

Functions for adding an objective to a problem

- [`objectives`](https://prioritizr.github.io/oppr/reference/objectives.md)
  : Problem objective
- [`add_max_phylo_div_objective()`](https://prioritizr.github.io/oppr/reference/add_max_phylo_div_objective.md)
  : Add maximum phylogenetic diversity objective
- [`add_max_richness_objective()`](https://prioritizr.github.io/oppr/reference/add_max_richness_objective.md)
  : Add maximum richness objective
- [`add_max_targets_met_objective()`](https://prioritizr.github.io/oppr/reference/add_max_targets_met_objective.md)
  : Add maximum targets met objective
- [`add_max_wtd_sum_objective()`](https://prioritizr.github.io/oppr/reference/add_max_wtd_sum_objective.md)
  : Add maximum weighted sum objective
- [`add_min_set_objective()`](https://prioritizr.github.io/oppr/reference/add_min_set_objective.md)
  : Add minimum set objective

## Weights

Functions for adding weights to a problem

- [`weights`](https://prioritizr.github.io/oppr/reference/weights.md) :
  Weights
- [`add_default_weights()`](https://prioritizr.github.io/oppr/reference/add_default_weights.md)
  : Add default weights
- [`add_feature_weights()`](https://prioritizr.github.io/oppr/reference/add_feature_weights.md)
  : Add feature weights

## Targets

Functions for adding targets to a problem

- [`targets`](https://prioritizr.github.io/oppr/reference/targets.md) :
  Targets
- [`add_absolute_targets()`](https://prioritizr.github.io/oppr/reference/add_absolute_targets.md)
  : Add absolute targets
- [`add_manual_targets()`](https://prioritizr.github.io/oppr/reference/add_manual_targets.md)
  : Add manual targets
- [`add_relative_targets()`](https://prioritizr.github.io/oppr/reference/add_relative_targets.md)
  : Add relative targets

## Constraints

Functions for adding constraints to a problem

- [`constraints`](https://prioritizr.github.io/oppr/reference/constraints.md)
  : Project prioritization problem constraints
- [`add_locked_in_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_in_action_constraints.md)
  : Add locked in action constraints
- [`add_locked_in_project_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_in_project_constraints.md)
  : Add locked in project constraints
- [`add_locked_out_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_out_action_constraints.md)
  : Add locked out action constraints
- [`add_locked_out_project_constraints()`](https://prioritizr.github.io/oppr/reference/add_locked_out_project_constraints.md)
  : Add locked out project constraints
- [`add_manual_locked_action_constraints()`](https://prioritizr.github.io/oppr/reference/add_manual_locked_action_constraints.md)
  : Add manually specified locked constraints for actions
- [`add_manual_locked_project_constraints()`](https://prioritizr.github.io/oppr/reference/add_manual_locked_project_constraints.md)
  : Add manually specified locked constraints for projects

## Decisions

Functions for specifying the type of decisions in a problem

- [`decisions`](https://prioritizr.github.io/oppr/reference/decisions.md)
  : Specify the type of decisions
- [`add_binary_decisions()`](https://prioritizr.github.io/oppr/reference/add_binary_decisions.md)
  : Add binary decisions

## Solvers

Functions for specifying how a problem should be solved

- [`solvers`](https://prioritizr.github.io/oppr/reference/solvers.md) :
  Solvers

- [`add_cbc_solver()`](https://prioritizr.github.io/oppr/reference/add_cbc_solver.md)
  :

  Add a *CBC* solver

- [`add_default_solver()`](https://prioritizr.github.io/oppr/reference/add_default_solver.md)
  : Add a default solver

- [`add_gurobi_solver()`](https://prioritizr.github.io/oppr/reference/add_gurobi_solver.md)
  :

  Add a *Gurobi* solver

- [`add_heuristic_solver()`](https://prioritizr.github.io/oppr/reference/add_heuristic_solver.md)
  : Add a heuristic solver

- [`add_highs_solver()`](https://prioritizr.github.io/oppr/reference/add_highs_solver.md)
  :

  Add a *HiGHS* solver

- [`add_lpsolveapi_solver()`](https://prioritizr.github.io/oppr/reference/add_lpsolveapi_solver.md)
  :

  Add a *lp_solve* solver with *lpSolveAPI*

- [`add_lpsymphony_solver()`](https://prioritizr.github.io/oppr/reference/add_lpsymphony_solver.md)
  :

  Add a *SYMPHONY* solver with *lpsymphony*

- [`add_random_solver()`](https://prioritizr.github.io/oppr/reference/add_random_solver.md)
  : Add a random solver

- [`add_rsymphony_solver()`](https://prioritizr.github.io/oppr/reference/add_rsymphony_solver.md)
  :

  Add a *SYMPHONY* solver with *Rsymphony*

## Approaches

Functions for specifying multi-objective optimization approaches

- [`approaches`](https://prioritizr.github.io/oppr/reference/approaches.md)
  : Multi-objective optimization approaches
- [`add_abs_constraint_approach()`](https://prioritizr.github.io/oppr/reference/add_abs_constraint_approach.md)
  : Add an absolute constraint approach
- [`add_chebyshev_goal_approach()`](https://prioritizr.github.io/oppr/reference/add_chebyshev_goal_approach.md)
  : Add a Chebyshev goal approach
- [`add_ref_point_approach()`](https://prioritizr.github.io/oppr/reference/add_ref_point_approach.md)
  : Add a reference point approach
- [`add_wtd_goal_approach()`](https://prioritizr.github.io/oppr/reference/add_wtd_goal_approach.md)
  : Add a weighted goal achievement approach

## Evaluate solutions

Functions for evaluating and visualizing solutions to a problem

- [`plot(`*`<ProjectProblem>`*`)`](https://prioritizr.github.io/oppr/reference/plot.ProjectProblem.md)
  : Plot a solution to a project prioritization problem
- [`plot_solution_barplot()`](https://prioritizr.github.io/oppr/reference/plot_solution_barplot.md)
  : Plot a bar plot to visualize a project prioritization
- [`plot_solution_phylogram()`](https://prioritizr.github.io/oppr/reference/plot_solution_phylogram.md)
  : Plot a phylogram to visualize a project prioritization
- [`project_cost_effectiveness()`](https://prioritizr.github.io/oppr/reference/project_cost_effectiveness.md)
  : Project cost effectiveness
- [`rank_importance()`](https://prioritizr.github.io/oppr/reference/rank_importance.md)
  : Rank importance
- [`replacement_costs()`](https://prioritizr.github.io/oppr/reference/replacement_costs.md)
  : Replacement cost
- [`solution_statistics()`](https://prioritizr.github.io/oppr/reference/solution_statistics.md)
  : Solution statistics

## Problem manipulation functions

Functions for extracting information from problems

- [`feature_names()`](https://prioritizr.github.io/oppr/reference/feature_names.md)
  : Feature names
- [`action_names()`](https://prioritizr.github.io/oppr/reference/action_names.md)
  : Action names
- [`project_names()`](https://prioritizr.github.io/oppr/reference/project_names.md)
  : Project names
- [`problem_names()`](https://prioritizr.github.io/oppr/reference/problem_names.md)
  : Problem names
- [`number_of_features()`](https://prioritizr.github.io/oppr/reference/number_of_features.md)
  : Number of features
- [`number_of_actions()`](https://prioritizr.github.io/oppr/reference/number_of_actions.md)
  : Number of actions
- [`number_of_projects()`](https://prioritizr.github.io/oppr/reference/number_of_projects.md)
  : Number of projects
- [`number_of_problems()`](https://prioritizr.github.io/oppr/reference/number_of_problems.md)
  : Number of problems

## Miscellaneous functions

Assorted functions distributed with the package

- [`show(`*`<ProjectModifier>`*`)`](https://prioritizr.github.io/oppr/reference/show.md)
  [`show(`*`<ProjectProblem>`*`)`](https://prioritizr.github.io/oppr/reference/show.md)
  [`show(`*`<OptimizationProblem>`*`)`](https://prioritizr.github.io/oppr/reference/show.md)
  [`show(`*`<MultiObjProjectProblem>`*`)`](https://prioritizr.github.io/oppr/reference/show.md)
  [`show(`*`<MultiObjApproach>`*`)`](https://prioritizr.github.io/oppr/reference/show.md)
  : Show
- [`compile()`](https://prioritizr.github.io/oppr/reference/compile.md)
  [`multi_compile(`*`<MultiObjProjectProblem>`*`)`](https://prioritizr.github.io/oppr/reference/compile.md)
  [`multi_compile(`*`<list>`*`)`](https://prioritizr.github.io/oppr/reference/compile.md)
  : Compile a problem
- [`branch_matrix()`](https://prioritizr.github.io/oppr/reference/branch_matrix.md)
  : Branch matrix

## Deprecated functions

Functions that are no longer distributed with the package

- [`plot_feature_persistence()`](https://prioritizr.github.io/oppr/reference/oppr-deprecated.md)
  [`plot_phylo_persistence()`](https://prioritizr.github.io/oppr/reference/oppr-deprecated.md)
  [`add_locked_in_constraints()`](https://prioritizr.github.io/oppr/reference/oppr-deprecated.md)
  [`add_locked_out_constraints()`](https://prioritizr.github.io/oppr/reference/oppr-deprecated.md)
  [`add_manual_locked_constraints()`](https://prioritizr.github.io/oppr/reference/oppr-deprecated.md)
  : Deprecation notice

## Class definitions and methods

Internal data structures and functions

- [`new_waiver()`](https://prioritizr.github.io/oppr/reference/new_waiver.md)
  : Waiver

- [`is.Waiver()`](https://prioritizr.github.io/oppr/reference/is.Waiver.md)
  : Is waiver?

- [`new_optimization_problem()`](https://prioritizr.github.io/oppr/reference/new_optimization_problem.md)
  : Optimization problem

- [`as.list(`*`<OptimizationProblem>`*`)`](https://prioritizr.github.io/oppr/reference/as.list.md)
  :

  Convert `OptimizationProblem` to list

- [`Constraint-class`](https://prioritizr.github.io/oppr/reference/Constraint-class.md)
  [`Constraint`](https://prioritizr.github.io/oppr/reference/Constraint-class.md)
  : Constraint class

- [`Decision-class`](https://prioritizr.github.io/oppr/reference/Decision-class.md)
  [`Decision`](https://prioritizr.github.io/oppr/reference/Decision-class.md)
  : Decision class

- [`MultiObjApproach-class`](https://prioritizr.github.io/oppr/reference/MultiObjApproach-class.md)
  [`MultiObjApproach`](https://prioritizr.github.io/oppr/reference/MultiObjApproach-class.md)
  : Multi-objective approach class

- [`MultiObjProjectProblem-class`](https://prioritizr.github.io/oppr/reference/MultiObjProjectProblem-class.md)
  [`MultiObjProjectProblem`](https://prioritizr.github.io/oppr/reference/MultiObjProjectProblem-class.md)
  : Multi-objective project problem class

- [`Objective-class`](https://prioritizr.github.io/oppr/reference/Objective-class.md)
  [`Objective`](https://prioritizr.github.io/oppr/reference/Objective-class.md)
  : Objective class

- [`OptimizationProblem-class`](https://prioritizr.github.io/oppr/reference/OptimizationProblem-class.md)
  [`OptimizationProblem`](https://prioritizr.github.io/oppr/reference/OptimizationProblem-class.md)
  : Optimization problem class

- [`ProjectModifier-class`](https://prioritizr.github.io/oppr/reference/ProjectModifier-class.md)
  [`ProjectModifier`](https://prioritizr.github.io/oppr/reference/ProjectModifier-class.md)
  : Conservation problem modifier class

- [`ProjectProblem-class`](https://prioritizr.github.io/oppr/reference/ProjectProblem-class.md)
  [`ProjectProblem`](https://prioritizr.github.io/oppr/reference/ProjectProblem-class.md)
  : Project problem class

- [`Solver-class`](https://prioritizr.github.io/oppr/reference/Solver-class.md)
  [`Solver`](https://prioritizr.github.io/oppr/reference/Solver-class.md)
  : Solver class

- [`Target-class`](https://prioritizr.github.io/oppr/reference/Target-class.md)
  [`Target`](https://prioritizr.github.io/oppr/reference/Target-class.md)
  : Target class

- [`Weight-class`](https://prioritizr.github.io/oppr/reference/Weight-class.md)
  [`Weight`](https://prioritizr.github.io/oppr/reference/Weight-class.md)
  : Weight class

- [`nrow()`](https://prioritizr.github.io/oppr/reference/tibble-methods.md)
  [`ncol()`](https://prioritizr.github.io/oppr/reference/tibble-methods.md)
  [`as.list(`*`<tbl_df>`*`)`](https://prioritizr.github.io/oppr/reference/tibble-methods.md)
  : Manipulate tibbles
