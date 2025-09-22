#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("roxygen2", quietly = TRUE)) {
    stop("The 'roxygen2' package is required. Please install it before running this script.")
  }
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("The 'testthat' package is required. Please install it before running this script.")
  }
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  script_path <- normalizePath(".")
}
script_dir <- dirname(script_path)
pkg_dir <- normalizePath(file.path(script_dir, ".."))

message("Documenting package...")
roxygen2::roxygenise(pkg_dir)

message("Running unit tests...")
testthat::test_local(pkg_dir)

oldwd <- getwd()
on.exit(setwd(oldwd), add = TRUE)
work_dir <- dirname(pkg_dir)
setwd(work_dir)
pkg_name <- basename(pkg_dir)

message("Building source package...")
build_output <- system2("R", c("CMD", "build", pkg_name), stdout = TRUE, stderr = TRUE)
cat(build_output, sep = "\n")

build_line <- tail(grep("\\* building", build_output, value = TRUE), 1)
if (!length(build_line)) {
  stop("Unable to determine built tarball path.")
}
tarball <- sub(".* (\\S+\\.tar\\.gz).*", "\\1", build_line)
tarball <- gsub("[‘’'\\\"]", "", tarball)
if (!file.exists(file.path(work_dir, tarball))) {
  stop("Built tarball not found: ", tarball)
}

message("Running R CMD check (--as-cran)...")
status <- system2("R", c("CMD", "check", "--as-cran", "--no-manual", tarball))
if (!identical(status, 0L)) {
  stop("R CMD check failed with status ", status)
}

message("Package built and checked successfully. Output tarball: ", tarball)
