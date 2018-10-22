generatePDF <- function(package) {
  
  path <- find.package(package)
  
  system(paste(shQuote(file.path(R.home("bin"), "R")),
               "CMD", "Rd2pdf --force", shQuote(path)))
}



build_manual <- function(pkg = ".", path = NULL) {
  pkg <- as.package(pkg)
  if (is.null(path)) {
    path <- dirname(pkg$path)
  }
  name <- paste0(pkg$package, "_", pkg$version, ".pdf", collapse = " ")
  system(paste0("R CMD Rd2pdf --force --output=", path, "/", name, " ", shQuote(pkg$path), collapse = " "))
}
