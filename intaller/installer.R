packageslInstallation <- function(names_of_packages){
  new_packages <- names_of_packages[!(names_of_packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) 
    install.packages(new_packages, dependencies = TRUE)
  sapply(new_packages, require, character.only = TRUE)
}


packages <- c("shiny", "stringi", "data.table", "DT", "shinythemes", "ggplot2", "readr", "shinyTree", "shinyjs", "RTextTools", "tm", "xtable", "tau", "stopwords", "caret","devtools")
packageslInstallation(packages)


packageLocalInstallation<-function(name_of_package,packages_dir){
  if (!(name_of_package %in% installed.packages()[,"Package"])){
    install.packages(paste0(packages_dir,"//",name_of_package),repos = NULL, type="source")
  }
}

cat("Please, enter the path to the folder with packages rword2vec and wordVectors (for example, \"C:/NimbleMiner/packages\"")
packages_dir <- readLines(con=stdin(),1)

library(devtools)

install(paste0(packages_dir,"\\rword2vec"))
install(paste0(packages_dir,"\\wordVectors"))
