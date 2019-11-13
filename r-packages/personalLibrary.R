# install.packages(c("devtools", "roxygen2"))
# install.packages(c("extrafont", "webshot")) # or webshot2
# sudo apt-get install unixodbc-dev
# install.packages(c("config","dbplyr","dbplot","odbc"))
# devtools::install_github("thomasp85/patchwork")
# font_import()


# devtools::install_github("rstudio/webshot2")
# devtools::install_github("rstudio/promises")
# devtools::install_github("r-lib/later")


wd = getwd()
setwd('~/Git/etl-analysis/r-packages/')

library(devtools)
library(roxygen2)
library(tidyverse)
library(ggplot2)

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

theme_set(themePhd(base_size=20))  # pre-set the bw theme.
ggplot(data=iris,aes(x=Sepal.Width, y=Sepal.Length)) + geom_point()

setwd(wd)
