
.pkgenv <- new.env(parent = emptyenv())

.onUnload <- function(libpath) {
  library.dynam.unload("ppr", libpath)
}

.onAttach <- function(libname, pkgname) {
  # define message generator function
  msg <- function() {
    packageStartupMessage(cli::rule())
    packageStartupMessage(
      "You have loaded both ppr and prioritizr - ",
      "this is likely to cause serious issues.\n",
      "You should only have one of these packages loaded at a time,\n",
      "please unload one ppr or prioritizr using one of the commands below:\n",
      "  detach(\"package:ppr\", unload = TRUE) # unload ppr package\n",
      "  detach(\"package:prioritizr\", unload = TRUE) # unload prioritizr",
      "package\n",
      "and then reload the desired package."
    )
    packageStartupMessage(cli::rule())
  }
  # print message if prioritizr already loaded
  if ("prioritizr" %in% .packages())
   msg()
  # set hook to print message if prioritizr is loaded later on
  setHook(packageEvent("prioritizr", "attach"), function(...) {
    msg()
  })
}
