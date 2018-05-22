#' Looking Up Installed Packages
#'
#' Looks up the packages that were installed on machine
#'
#' @param path the path to where the packages are saved (stop at, but include, the "Versions" folder)
#' @param updated whether or not you have already updated R to a new version
#'
#' @import dplyr
#'
#' @export

installed_packages <- function(path = NULL, updated = FALSE) {

  if (is.null(path)){
    sysname <- R.version$os

    if (grepl("darwin", sysname)) {
      path <- "/Library/Frameworks/R.framework/Versions/"
    } else if (grepl("lin", sysname)) {
      stop("Looks like you are on a linux system. Please input the path to the packages.")
    } else {
      stop("Looks like you are on a Windows system. Please input the path to the packages.")
    }
  } else {
    path <- path
  }

  if (updated & length(list.files(path)) > 2) {
    ver <- 2
  } else {
    ver <- 1
  }

  versions <- list.files(path) %>%
    sort %>%
    .[[ver]]

  new_path <- paste0(path, "/", versions, "/Resources/library/")
  packages <- list.files(new_path)

  attr(packages, "path") <- new_path
  packages

}

#' Looks Up Installed Package Versions
#'
#' Looks up the package versions that are installed
#'
#' @import dplyr
#'
#' @export

installed_versions <- function(path = NULL, updated = FALSE) {

  packages <- installed_packages(path, updated)
  path <- attr(packages, "path")

  versions <- vector("list", length(packages))
  for (i in packages){
    file <- try(readLines(paste0(path, i, "/DESCRIPTION")))
    versions[[i]] <- file %>%
      .[grepl("Version:", .)] %>%
      gsub("Version: ", "", .)
  }

  versions <- unlist(versions) %>% data.frame %>% setNames(c("Version"))
  versions
}


#' Reinstalling installed packages after update
#'
#' Looks up the packages that were installed on the previous version of R
#'
#' @param path the path to where the packages are saved (stop at, but include, the "Versions" folder)
#' @param updated whether or not you have already updated R to a new version
#'
#' @import dplyr
#'
#' @export

installed_all <- function(path = NULL, updated = FALSE) {

  pack_ver <- installed_versions(path, updated)
  pack_ver %>%
    tibble::rownames_to_column("Package")

}


#' Installed Packages-Versions Available on CRAN
#'
#' Looks up the package-version combinations available currently on CRAN
#'
#' @param path the path to where the packages are saved (stop at, but include, the "Versions" folder)
#' @param updated whether or not you have already updated R to a new version
#'
#' @import dplyr
#'
#' @export

installed_cran <- function(path = NULL, updated = FALSE) {

  installed <- installed_all(path, updated)
  available_cran <- available.packages()[, c("Package", "Version")] %>%
    unique %>%
    data.frame %>%
    dplyr::filter(Package %in% installed$Package)

  available_cran %>%
    dplyr::filter(Version %in% installed$Version)

}

#' Installed Packages-Versions NOT Available on CRAN
#'
#' Looks up the packages-version not available on CRAN. In other words, these may be packages that were installed from GitHub
#' or another repository.
#'
#' @param path the path to where the packages are saved (stop at, but include, the "Versions" folder)
#' @param updated whether or not you have already updated R to a new version
#'
#' @import dplyr
#'
#' @export

installed_not_cran <- function(path = NULL, updated = FALSE) {

  installed <- installed_all(path, updated)
  available_cran <- available.packages()[, c("Package", "Version")] %>%
    unique %>%
    data.frame %>%
    dplyr::filter(Package %in% installed$Package)

  available_cran %>%
    dplyr::filter(!Version %in% installed$Version)

}

#' Save List of Packages Prior to Updating R
#'
#' Saves a list of packages that can be used after updating R
#'
#' @param savefilepath the path where to save the list of packages on and not on CRAN
#' @param path the path to where the packages are saved (stop at, but include, the "Versions" folder)
#' @param updated whether or not you have already updated R to a new version
#'
#' @import dplyr
#'
#' @export

installed_prior <- function(savefilepath, path = NULL, updated = FALSE) {

  cran     <- installed_cran(path, updated)
  not_cran <- installed_not_cran(path, updated)

  saveRDS(cran, paste0(savefilepath, "/CRANpackages.rds"))
  saveRDS(cran, paste0(savefilepath, "/NOTCRANpackages.rds"))
}




#' re-export magrittr pipe operator
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL

