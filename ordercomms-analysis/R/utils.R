createDirectory <- function() {
  directory <- paste("~/Documents/timeToView/",format(Sys.time(),"%Y%m%d"),sep="");
  dir.create(file.path(directory), showWarnings = FALSE);
  setwd(file.path(directory));
}

syncToDrive <- function() {
  system('rclone copy ~/Documents/timeToView GoogleDrive:academic/projects/benefits/graphs -v');
}