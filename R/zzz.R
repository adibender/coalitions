.onLoad <- function(libname=find.package("coalitions"), pkgname="coalitions") {

  if (getRversion() >= "2.5.1") {
    utils::globalVariables(c(".", "party_colors_de"))
  }

  invisible()

}
