# this R script will source all the scripts necessary from the
# `generate_datasets` directory (folder)

rscripts <- list.files(
  path = "~/R/exteff-ts/scripts/generate_datasets",
  pattern = "\\.R$",
  recursive = TRUE,
  full.names = TRUE
)

sapply(rscripts, source)

