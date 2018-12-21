
.pkgenv <- new.env(parent = emptyenv())

.onUnload <- function(libpath) {
  library.dynam.unload("ppr", libpath)
}
