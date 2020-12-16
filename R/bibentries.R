format_bib = function(...) {
  str = vapply(list(...), function(entry) tools::toRd(bibentries[[entry]]), FUN.VALUE = "")
  paste0(str, collapse = "\n\n")
}

#' @importFrom utils bibentry
bibentries = c(
  bossek_2017 = bibentry("inproceedings",
    doi         = "10.1145/3067695.3082470",
    year        = "2017",
    month       = "jul",
    publisher   = "ACM",
    author      = "Jakob Bossek",
    title       = "ecr 2.0",
    booktitle   = "Proceedings of the Genetic and Evolutionary Computation Conference Companion"
  ),

  bergstra_2012 = bibentry("article",
    title       = "Random Search for Hyper-Parameter Optimization",
    author      = "James Bergstra and Yoshua Bengio",
    year        = "2012",
    journal     = "Journal of Machine Learning Research",
    volume      = "13",
    number      = "10",
    pages       = "281--305",
    url         = "https://jmlr.csail.mit.edu/papers/v13/bergstra12a.html"
  )
)
