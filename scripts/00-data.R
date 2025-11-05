# this R script runs all the scripts in the `generate_datasets` directory
# (folder)

# custom function to source all .R script files in a directory
source_dir <- function(path,
                       trace = TRUE,
                       recursive = TRUE,
                       ...) {
  rscripts <- list.files(path,
                         pattern = "\\.R$",
                         full.names = TRUE,
                         recursive = recursive)
  
  # check for .R scripts
  if (length(rscripts) > 0) {
    # source each file
    for (file in rscripts) {
      if (trace)
        cat("sourcing:", basename(file), "\n")
      source(file)
    }
    cat("finished source of all .R scripts in path:", path, "\n")
  }
  else {
    cat("No .R scripts found in path:", path, "\n")
  }
}

# run custom function specifying directory path 
source_dir(path = "~/R/exteff-ts/scripts/generate_datasets")



