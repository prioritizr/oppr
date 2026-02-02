# Branch matrix

Phylogenetic trees depict the evolutionary relationships between
different species. Each branch in a phylogenetic tree represents a
period of evolutionary history. Species that are connected to the same
branch share the same period of evolutionary history represented by the
branch. This function creates a matrix that shows which species are
connected with which branches. In other words, it creates a matrix that
shows which periods of evolutionary history each species has
experienced.

## Usage

``` r
branch_matrix(x, ...)

# Default S3 method
branch_matrix(x, ...)

# S3 method for class 'phylo'
branch_matrix(x, assert_validity = TRUE, ...)
```

## Arguments

- x:

  [`ape::phylo()`](https://rdrr.io/pkg/ape/man/read.tree.html) tree
  object.

- ...:

  not used.

- assert_validity:

  `logical` value (i.e., `TRUE` or `FALSE` indicating if the argument to
  `x` should be checked for validity. Defaults to `TRUE`.

## Value

A
[Matrix::dgCMatrix](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
sparse matrix object. Each row corresponds to a different species. Each
column corresponds to a different branch. Species that inherit from a
given branch are indicated with a one.

## Examples

``` r
# load Matrix package to plot matrices
library(Matrix)

# load data
data(sim_tree)

# generate species by branch matrix
m <- branch_matrix(sim_tree)

# plot data
par(mfrow = c(1, 2))
plot(sim_tree, main = "phylogeny")
image(m, main = "branch matrix")

```
