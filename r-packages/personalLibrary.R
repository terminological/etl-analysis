
wd = getwd()
setwd('~/Git/etl-analysis/r-packages/')

library(devtools)
library(roxygen2)

# create_package("phdUtils")

for (dir in list.dirs(recursive = FALSE)) {
  document(dir)
  packageName <- str_replace(dir,"\\.\\/","")
  if(paste0("package:",packageName) %in% search()) detach(paste0("package:",packageName), unload=TRUE, character.only = TRUE)
  devtools::install(packageName)
  library(packageName, character.only = TRUE)
}

library(huxtable)
iris %>% group_by(Petal.Width,Petal.Length) %>% saveTable("iris")
                 
iris %>% group_by(Species,Petal.Width) %>% mergeCells() %>% saveTable("iris")

setwd(wd)
