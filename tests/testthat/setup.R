# Source all R files from the app's R/ directory
app_dir <- file.path(dirname(dirname(getwd())), "R")
if (!dir.exists(app_dir)) {
  app_dir <- file.path(dirname(dirname(dirname(testthat::test_path()))), "R")
}

r_files <- list.files(app_dir, pattern = "\\.R$", full.names = TRUE)
# Source utils first, then modules
utils_files <- grep("utils_", r_files, value = TRUE)
mod_files <- grep("mod_", r_files, value = TRUE)
for (f in c(utils_files, mod_files)) {
  source(f, local = FALSE)
}
