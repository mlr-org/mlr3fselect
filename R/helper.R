catn = function(..., file = "") {
  cat(paste0(..., collapse = "\n"), "\n", sep = "", file = file)
}
