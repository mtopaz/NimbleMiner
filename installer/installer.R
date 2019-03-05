packageslInstallation <- function(names_of_packages){
  new_packages <- names_of_packages[!(names_of_packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) 
    install.packages(new_packages, dependencies = TRUE)
  sapply(new_packages, require, character.only = TRUE)
}


packages <- c("shiny", "stringi", "data.table", "DT", "shinythemes", "ggplot2", "keras", "readr", "shinyTree", "shinyjs", "tm", "xtable",
              "tau", "stopwords", "caret","devtools")
packageslInstallation(packages)

cat("Please, enter the path to the folder with packages rword2vec and wordVectors (for example, on Windows \"C:/NimbleMiner/installer\" or on Mac \"/Users/User1/Documents/NimbleMiner/installer\"")
packages_dir <- readLines(con=stdin(),1)

library(devtools)

if(Sys.info()['sysname']=='Windows'){
  install(paste0(packages_dir,"\\rword2vec"))
  install(paste0(packages_dir,"\\wordVectors"))
  install(paste0(packages_dir,"\\RTextTools"))
} else {
  install(paste0(packages_dir,"/rword2vec"))
  install(paste0(packages_dir,"/wordVectors"))
  install(paste0(packages_dir,"/RTextTools"))  
}  
