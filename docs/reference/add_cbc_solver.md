# Add a *CBC* solver

Add a solver to generate solutions to a project prioritization problem
with the [*CBC*](https://github.com/coin-or/Cbc) (COIN-OR branch and
cut) Forrest & Lougee-Heimer 2005). This function can also be used to
customize the behavior of the solver. It requires the rcbc package to be
installed (only [available on
GitHub](https://github.com/dirkschumacher/rcbc), see below for
installation instructions).

## Usage

``` r
add_cbc_solver(
  x,
  gap = 0.1,
  time_limit = .Machine$integer.max,
  presolve = 2,
  threads = 1,
  first_feasible = FALSE,
  verbose = TRUE
)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
  or
  [`multi_problem()`](https://prioritizr.github.io/oppr/reference/multi_problem.md)
  object.

- gap:

  `numeric` gap to optimality. This gap is relative and expresses the
  acceptable deviance from the optimal objective. For example, a value
  of 0.01 will result in the solver stopping when it has found a
  solution within 1% of optimality. Additionally, a value of 0 will
  result in the solver stopping when it has found an optimal solution.
  The default value is 0 (i.e., 0% from optimality).

- time_limit:

  `numeric` time limit in seconds to run the optimizer. The solver will
  return the current best solution when this time limit is exceeded.

- presolve:

  `integer` number indicating how intensively the solver should try to
  simplify the problem before solving it. Available options are: (0)
  disable pre-solving, (1) conservative level of pre-solving, and (2)
  very aggressive level of pre-solving . The default value is 2.

- threads:

  `integer` number of threads to use for the optimization algorithm. The
  default value of 1 will result in only one thread being used.

- first_feasible:

  `logical` should the first feasible solution be be returned? If
  `first_feasible` is set to `TRUE`, the solver will return the first
  solution it encounters that meets all the constraints, regardless of
  solution quality. Note that the first feasible solution is not an
  arbitrary solution, rather it is derived from the relaxed solution,
  and is therefore often reasonably close to optimality. Defaults to
  `FALSE`.

- verbose:

  `logical` should information be printed during optimization? Defaults
  to `TRUE`.

## Value

A [`problem()`](https://prioritizr.github.io/oppr/reference/problem.md)
object with the solver added to it.

## Details

[*CBC*](https://github.com/coin-or/Cbc) is an open-source mixed integer
programming solver that is part of the Computational Infrastructure for
Operations Research (COIN-OR) project. This solver seems to have much
better performance than the other open-source solvers (i.e.,
[`add_highs_solver()`](https://prioritizr.github.io/oppr/reference/add_highs_solver.md),
[`add_rsymphony_solver()`](https://prioritizr.github.io/oppr/reference/add_rsymphony_solver.md),
[`add_lpsymphony_solver()`](https://prioritizr.github.io/oppr/reference/add_lpsymphony_solver.md))
(see the *Solver benchmarks* vignette for details). As such, it is
strongly recommended to use this solver if the *Gurobi* solver is not
available.

## Installation

The rcbc package is required to use this solver. Since the rcbc package
is not available on the the Comprehensive R Archive Network (CRAN), it
must be installed from [its GitHub
repository](https://github.com/dirkschumacher/rcbc). To install the rcbc
package, please use the following code:

    if (!require(remotes)) install.packages("remotes")
    remotes::install_github("dirkschumacher/rcbc")

Note that you may also need to install several dependencies – such as
the [Rtools software](https://cran.r-project.org/bin/windows/Rtools/) or
system libraries – prior to installing the rcbc package. For further
details on installing this package, please consult the [online package
documentation](https://dirkschumacher.github.io/rcbc/).

## References

Forrest J and Lougee-Heimer R (2005) CBC User Guide. In Emerging theory,
Methods, and Applications (pp. 257–277). INFORMS, Catonsville, MD.
[doi:10.1287/educ.1053.0020](https://doi.org/10.1287/educ.1053.0020) .

## See also

Other solvers:
[`add_default_solver()`](https://prioritizr.github.io/oppr/reference/add_default_solver.md),
[`add_gurobi_solver()`](https://prioritizr.github.io/oppr/reference/add_gurobi_solver.md),
[`add_heuristic_solver()`](https://prioritizr.github.io/oppr/reference/add_heuristic_solver.md),
[`add_highs_solver()`](https://prioritizr.github.io/oppr/reference/add_highs_solver.md),
[`add_lpsolveapi_solver()`](https://prioritizr.github.io/oppr/reference/add_lpsolveapi_solver.md),
[`add_lpsymphony_solver()`](https://prioritizr.github.io/oppr/reference/add_lpsymphony_solver.md),
[`add_random_solver()`](https://prioritizr.github.io/oppr/reference/add_random_solver.md),
[`add_rsymphony_solver()`](https://prioritizr.github.io/oppr/reference/add_rsymphony_solver.md)

## Examples

``` r
# \dontrun{
# load data
sim_pu_raster <- get_sim_pu_raster()
#> Error in get_sim_pu_raster(): could not find function "get_sim_pu_raster"
sim_features <- get_sim_features()
#> Error in get_sim_features(): could not find function "get_sim_features"

# create problem
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0, verbose = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'add_relative_targets': object 'sim_pu_raster' not found

# generate solution %>%
s1 <- solve(p1)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'a' in selecting a method for function 'solve': object 'p1' not found

# plot solution
plot(s1, main = "solution", axes = FALSE)
#> Error: object 's1' not found

# create a similar problem with boundary length penalties and
# specify the solution from the previous run as a starting solution
p2 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(10) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0, start_solution = s1, verbose = FALSE)
#> Error in add_cbc_solver(., gap = 0, start_solution = s1, verbose = FALSE): unused argument (start_solution = s1)

# generate solution
s2 <- solve(p2)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'a' in selecting a method for function 'solve': object 'p2' not found

# plot solution
plot(s2, main = "solution with boundary penalties", axes = FALSE)
#> Error: object 's2' not found
# }
```
