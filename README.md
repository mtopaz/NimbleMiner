# NimbleMiner
NimbleMiner: a software that allows clinicians to interact with word embedding models (skip-gram models- word2vec and Glove) to rapidly create lexicons of similar terms.

To run the application, please follow these steps:

1. Copy all files and folders from NimbleMiner repository
2. Install base software:
* [Download](https://cran.r-project.org/) and install the last version of R (3.5.0 or later)
* [Download](https://www.rstudio.com/products/rstudio/download/#download) and install the last version of RStudio (1.1.447 or later)
* Download and install devtools:: 
   * o	On windows: [Rtools](https://cran.r-project.org/bin/windows/Rtools/) (35 or last recommended version) - Change the default path ( C:\RBuildTools ) to C:\Rtools
   * On Mac: [Xcode command line tools](https://developer.apple.com/downloads).
2. Restart the computer.

3. Install packages:  

* Open script installer/installer.R in RStudio  
* Run script with help of button "Source"
* Wait till all packages will be installed
* Restart R session (or restart RStudio)

4. Open NimbleMiner.R in RStudio and run it by the button "Run App"


**NB:**  The \"installer.R\" script referenced above installs CRAN dependencies before using devtools to install dependencies maintained on GitHub.  To address dependencies manually or with an external package manager, be advised that NimbleMiner has these CRAN package dependies:

c("shiny", "stringi", "data.table", "DT", "shinythemes", "ggplot2", "keras", "readr", "shinyTree", "shinyjs", "tm", "xtable", "tau", "stopwords", "caret","fs", "devtools", "RTextTools")

These dependencies are from GitHub. Install using devtools.

+ [wordVectors](https://github.com/bmschmidt/wordVectors)
+ [rword2vec](https://github.com/mukul13/rword2vec)
+ [maxnet](https://github.com/mrmaxent/maxnet) *is the R implementation of the [MaxEnt](https://biodiversityinformatics.amnh.org/open_source/maxent/) maximum entropy modeling application.  Be careful with your spelling.* 
