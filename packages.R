### Write down what package versions work with your R code, and
### attempt to download and load those packages. The first argument is
### the version of R that you used, e.g. "3.0.2" and then the rest of
### the arguments are package versions. For
### CRAN/Bioconductor/R-Forge/etc packages, write
### e.g. RColorBrewer="1.0.5" and if RColorBrewer is not installed
### then we use install.packages to get the most recent version, and
### warn if the installed version is not the indicated version. For
### GitHub packages, write "user/repo@commit"
### e.g. "tdhock/animint@f877163cd181f390de3ef9a38bb8bdd0396d08a4" and
### we use install_github to get it, if necessary.
works_with_R <- function(Rvers,...){
  local.lib <- file.path(getwd(), "library")
  old.path.vec <- .libPaths()
  if(is.null(getOption("repos"))){
    options(repos="http://cloud.r-project.org")
  }
  if(! local.lib %in% old.path.vec){
    dir.create(local.lib, showWarnings=FALSE, recursive=TRUE)
    .libPaths(local.lib)
  }
  pkg_ok_have <- function(pkg,ok,have){
    stopifnot(is.character(ok))
    if(!as.character(have) %in% ok){
      warning("works with ",pkg," version ",
              paste(ok,collapse=" or "),
              ", have ",have)
    }
  }
  pkg_ok_have("R",Rvers,getRversion())
  pkg.vers <- list(...)
  for(pkg.i in seq_along(pkg.vers)){
    vers <- pkg.vers[[pkg.i]]
    pkg <- if(is.null(names(pkg.vers))){
      ""
    }else{
      names(pkg.vers)[[pkg.i]]
    }
    if(pkg == ""){# Then it is from GitHub.
      ## suppressWarnings is quieter than quiet.
      if(!suppressWarnings(require(requireGitHub))){
        ## If requireGitHub is not available, then install it using
        ## devtools.
        if(!suppressWarnings(require(devtools))){
          install.packages("devtools")
          require(devtools)
        }
        install_github("tdhock/requireGitHub")
        require(requireGitHub)
      }
      requireGitHub(vers)
    }else{# it is from a CRAN-like repos.
      if(!suppressWarnings(require(pkg, character.only=TRUE))){
        install.packages(pkg)
      }
      pkg_ok_have(pkg, vers, packageVersion(pkg))
      library(pkg, character.only=TRUE)
    }
  }
}
options(namedCapture.engine="PCRE")
options(datatable.fread.input.cmd.message=FALSE)
options(repos="http://cloud.r-project.org")
works_with_R(
  "4.0.2",
  data.table="1.13.1",
  ggplot2="3.3.2",
  "tdhock/penmap@7c88273c26bdb1246425fa4fe5a07b8197c7ad5a",
  tikzDevice="0.12.3.1",
  penaltyLearning="2020.5.13")
options(
  tikzDocumentDeclaration=paste(
    "\\documentclass[12pt]{article}"),
  tikzMetricsDictionary="tikzMetrics")
