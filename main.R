load_all()
load_all()
  # create data
  projects <- tibble::tibble(
    name = letters[1:4],
    success = c(0.95, 0.96, 0.94, 1.00),
    F1 = c(0.91, 0.00, 0.80, 0.10),
    F2 = c(0.00, 0.92, 0.80, 0.10),
    F3 = c(0.00, 0.00, 0.00, 0.10),
    A1 = c(TRUE, FALSE, FALSE, FALSE),
    A2 = c(FALSE, TRUE, FALSE, FALSE),
    A3 = c(FALSE, FALSE, TRUE, FALSE),
    A4 = c(FALSE, FALSE, FALSE, TRUE)
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4"),
    cost = c(0.10, 0.10, 0.15, 0),
    locked_in = FALSE,
    locked_out = FALSE
  )
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  tree <- ape::read.tree(text = "((F1,F2),F3);")
  tree$edge.length <- c(100, 5, 5, 5)
  # make problem
  p <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_max_phylo_div_objective(0.16, tree) %>%
    add_binary_decisions()
  # create optimization problem
  o1 <- compile(p, n_approx = 5)
  o2 <- r_phylo_div_mip_formulation(projects, actions, tree, 0.16, 5)
  print(o1$pwlobj())
  print(o2$pwlobj)
