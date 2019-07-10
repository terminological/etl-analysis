library(DBI)
library(odbc)
library(datasets)
library(dplyr)
library(reshape2)
library(ggplot2)

source_directory <- getSrcDirectory(function(dummy) {dummy});
if (source_directory == "") source_directory="/home/robchallen/Git/working-drafts/src/main/r"
source(paste(source_directory,'/standardAxes.R',sep=""));
source(paste(source_directory,'/utils.R',sep=""));
createDirectory();

pwd <- readline(prompt="Enter DB: ");
# pwd <- rstudioapi::askForPassword("Database password");

con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 13 for SQL Server",
                 Server = "10.174.129.118",
                 Database = "RobsDatabase",
                 UID = "RobertSQL",
                 PWD = pwd,
                 Port = 1433);

dbListTables(con);

data3 <- dbReadTable(con, "aggTimeToAction");
count(data3)

data_tmp <- data3 %>% 
		filter(chi_2>80) %>% 
		filter(minutes_to_view > 0) %>% 
		filter(minutes_to_view < 4*60) %>% 
		filter(minutes_to_action < 4*60)
count(data_tmp)
cor(x=data_tmp$patient_age, y=data_tmp$minutes_to_action, use="complete.obs")
cor(x=data_tmp$chi_2, y=data_tmp$minutes_to_action, use="complete.obs")

model.lm <- lm(minutes_to_action ~ normalcy, data = data_tmp)
summary(model.lm)

model.lm <- lm(minutes_to_action ~ dependency_level, data = data_tmp)
summary(model.lm)

model.lm <- lm(minutes_to_action ~ minutes_to_view, data = data_tmp)
summary(model.lm)
data_tmp$minutes_to_action_residuals <- resid(model.lm)
ggplot(
	data=data_tmp, 
	aes(x=minutes_to_view, y=minutes_to_action_residuals)) +
	geom_bin2d(na.rm = TRUE)


model.lm <- lm(minutes_to_action ~ patient_age, data = data_tmp)
summary(model.lm)

model.lm <- lm(minutes_to_action ~ chi_2, data = data_tmp)
summary(model.lm)

model.lm <- lm(minutes_to_action ~ patient_group, data = data_tmp)
summary(model.lm)

# cor(x=data2$patient_age, y=data2$minutes_to_resolution, use="complete.obs")
# cor(x=data2$patient_age, y=data2$minutes_to_resolution, use="complete.obs")

timeToActionByTimeToView <- ggplot(
  data=data_tmp, 
  aes(x=minutes_to_view, y=minutes_to_action)) +
  geom_bin2d(na.rm = TRUE)

timeToActionByTimeToView
ggsave('timeToActionByTimeToView.png',width=10,height=5,units='in')


# cor(data_no_na$patient_age, data_no_na$minutes_to_resolution)
# cor(data2, use="complete.obs", method="kendall") 

# https://stats.stackexchange.com/questions/108007/correlations-with-categorical-variables


# ggplot(data2,aes(x=minutes_to_resolution,y=minutes_to_view)) + 
#   geom_point(colour="blue", alpha=0.2) + 
#   geom_density2d(colour="black")