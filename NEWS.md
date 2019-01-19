# ppr 0.0.0.3

- The gurobi solver (i.e. `add_gurobi_solver` function) now uses
  `NumericFocus=3` to help avoid numerical issues.
- The `compile` function now throws a warning if problems are likely to
  have numerical issues.

# ppr 0.0.0.2

- Fix bug when solving problems with a phylogenetic objective and branches that
  have a constant probability of persistence (#6).
- Add additional data sanity checks to `problem`. It will now throw
  descriptive error messages if features are missing baseline probabilities, or
  are associated with baseline probabilities below 1e-11.
- Fix unit test for `simulate_ptm_data` that had a very small chance of failing
  due to simulating a data set where an action is not associated with any
  project.
- Feature columns in simulated data produced using `simulate_ppp_data` and
  `simulate_ptm_data` are now sorted.

# ppr 0.0.0.1

- Initial commit.
