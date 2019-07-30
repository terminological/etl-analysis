setwd('~/Git/etl-analysis/r-packages/')

library(devtools)
library(roxygen2)

create_package("phdUtils")

for (dir in list.dirs(recursive = FALSE)) {
  setwd(dir)
  document()
  setwd("..")
  install(str_replace(dir,"\\.\\/",""))
}