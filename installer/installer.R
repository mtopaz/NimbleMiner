packageslInstallation <- function(names_of_packages){
  new_packages <- names_of_packages[!(names_of_packages %in% installed.packages()[, "Package"])]
  if (length(new_packages))
    install.packages(new_packages, dependencies = TRUE)
  sapply(new_packages, require, character.only = TRUE)
}

packages <- c("shiny", "stringi", "data.table", "DT", "shinythemes", "ggplot2", "keras", "readr", "shinyTree", "shinyjs", "tm", "xtable",
              "tau", "stopwords", "caret","fs","devtools")
packageslInstallation(packages)

packages_dir <<- paste0(getSrcDirectory(function(x) {x}),"/");


library(devtools)


unzip(paste0(packages_dir,"/wordVectors.zip"),exdir=paste0(packages_dir,"/wordVectors"))
install(paste0(packages_dir,"/wordVectors/wordVectors"))

unzip(paste0(packages_dir,"/rword2vec.zip"),exdir=paste0(packages_dir,"/rword2vec"))
install(paste0(packages_dir,"/rword2vec/rword2vec"))
install.packages(paste0(packages_dir,"/maxent.tar.gz"), repos = NULL, type="source")
install.packages(paste0(packages_dir,"/RTextTools.tar.gz"), repos = NULL, type="source")
install.packages(paste0(packages_dir,"/NimbleMiner_0.1.0.tar.gz"), repos = NULL, type="source")
