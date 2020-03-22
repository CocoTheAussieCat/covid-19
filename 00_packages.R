# PACKAGE AND LIBRARY INSTALLATION ------------------------------------------
package_list <- list("tidyverse",
                     "lubridate",
                     "here",
                     "scales",
                     "RColorBrewer",
                     "glue", 
                     "wesanderson",
                     "shiny",
                     "shinydashboard",
                     "plotly",
                     "patchwork")

# Uncomment to install packages
# for (package in package_list) {
#  install.packages(package)
#}

# Call libraries
for (package in package_list) {
  library(package, character.only = T)
}

# Clean up
rm(package_list, package)