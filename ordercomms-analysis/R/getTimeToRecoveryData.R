library(DBI)
library(odbc)
library(datasets)
library(dplyr)
library(reshape2)
library(hexbin)
library(cowplot)
library(ggrepel)

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

data2 <- dbReadTable(con, "aggTimeToRecovery");

data2 <- data2 %>%
		mutate(month_cat=format(date, "%Y-%m")) %>%
		mutate(time_of_week=as.numeric(difftime(date,as.POSIXct(cut(date,"week")),unit="mins"))%%(24*7*60)) %>%
		mutate(time_of_day=time_of_week %% (24*60)) %>%
		mutate(day_time_cat = cut(
						(time_of_day+16*60)%%(24*60),
						breaks=c(0,4*60,8*60,10*60,25*60),
						labels=c("Morning","Day","Afternoon","Night"),
						ordered_result=FALSE,
						include.lowest=TRUE
				)) %>%
    mutate(degree = (2*as.numeric(numeric_result) - (as.numeric(high_range) + as.numeric(low_range)))/(as.numeric(high_range)-as.numeric(low_range))) %>%
    mutate(degree_cat = cut(degree,
           breaks=c(-Inf,-5,-4,-3,-2,-0.99,1,2,3,4,5,6,7,8,9,10,Inf),
           labels=c("under -5","-5 to -4","-4 to -3","-3 to -2","-2 to -1","normal","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","over 10"),
           ordered_result=TRUE,
           include.lowest=TRUE
           )) %>%
    mutate(age_group = patient_age %/% 10 * 10) %>%
		filter(as.Date(date) <= as.Date("2017-08-31")) %>%
		filter(ward_name != "Other") %>%
		collect();

save.image(file="~/R/timeToRecoveryData");

###########################################
# characterise data set

data2_count <- nrow(data2);

# distribution by hour
data2_by_hour_to_resolution <- data2 %>%
		mutate(
				hours_to_resolution = minutes_to_resolution %/% 60,
				total = n()
		) %>%
		group_by(hours_to_resolution) %>%
		summarize(
				count=n()
		) %>% mutate(
				cumulative=cumsum(count),
				recovered_percent=cumulative / data2_count
		)

ggplot(data = data2_by_hour_to_resolution %>% filter(hours_to_resolution < 240),
		aes(x=hours_to_resolution,y=count))+
		geom_bar(stat="identity",width=1, fill="#c0c0ff")+
		geom_line(aes(y=recovered_percent*20000))+
		xlab("hours from abnormality")+
		ylab("normal test result")+
    scale_x_continuous(limits=c(0,240),breaks = seq(0, 240, 24))+
		scale_y_continuous(limits=c(0,20000),breaks = seq(0, 20000, 2000),
				sec.axis=sec_axis(~./200, name="percent recovered")
		);
ggsave('timeToRecoveryDistribution.png',width=10,height=5,units='in')

#############################

# distribution by month
data2_by_month <- data2 %>%
    filter(minutes_to_resolution < 31*24*60) %>%
    filter(as.Date(date) > as.Date("2014-08-31")) %>% 
		group_by(month_cat) %>%
		summarize(
				count=n(),
				viewed_percent=sum(total_views != 0)/n(),
				lower_fence=fivenum(minutes_to_resolution, na.rm = TRUE)[1],
				lower_quartile=fivenum(minutes_to_resolution, na.rm = TRUE)[2],
				median=fivenum(minutes_to_resolution, na.rm = TRUE)[3],
				upper_quartile=fivenum(minutes_to_resolution, na.rm = TRUE)[4],
				higher_fence=fivenum(minutes_to_resolution, na.rm = TRUE)[5]
		)

ttr_box_by_time <- 
  ggplot(data2_by_month, aes(x=month_cat)) +
    geom_boxplot(
      aes(
        lower = lower_quartile/60,
        upper = upper_quartile/60,
        middle = median/60,
        ymin = 0,
        ymax = (upper_quartile+1.5*(upper_quartile-lower_quartile))/60
      ), 
      stat="identity", fill="#c0c0c0", size=0.3) +
    theme(axis.text.x = element_text(angle=90,size=9))+
    xlab("month") +
    ylab("hours") +
    scale_y_continuous(limits=c(0,360),breaks = seq(0, 360, 24));

ttr_box_by_time #+ggtitle("Median time to test recovery by over time")
ggsave('historicalTimeToRecoveryBoxPlot.png',width=10,height=5,units='in')

###################################
# correlations

timeToRecoveryByTimeToView <- ggplot(
				data=data2 %>% filter(minutes_to_view > 0) %>% filter(minutes_to_view < 6*60) %>% filter( minutes_to_resolution < 5*24*60), 
				aes(x=minutes_to_view, y=minutes_to_resolution/60)) +
		geom_bin2d(na.rm = TRUE, bins=120) +
    scale_fill_gradient(low = "#f8f8ff", high = "#2020ff") +
    xlab("time to view (minutes)") +
    ylab("time to resolution of abnormality  (hours)")+
  scale_x_continuous(limits=c(0,6*60),breaks=seq(0,6*60,60))+
  scale_y_continuous(limits=c(0,5*24),breaks=seq(0,5*24,24))
timeToRecoveryByTimeToView+ggtitle("Correlation betwen time to view and time to resolution of test abnormality")
ggsave('timeToRecoveryByTimeToView.png',width=10,height=5,units='in')

data_creat <- data2 %>% filter(test=="VANC")
timeToRecoveryByTimeToView <- ggplot(
  data=data_creat %>% filter(minutes_to_view > 0) %>% filter(minutes_to_view < 6*60) %>% filter( minutes_to_resolution < 5*24*60), 
  aes(x=minutes_to_view, y=minutes_to_resolution/60)) +
  geom_bin2d(na.rm = TRUE, bins=120) +
  scale_fill_gradient(low = "#f8f8ff", high = "#2020ff") +
  xlab("time to view (minutes)") +
  ylab("time to resolution of abnormality  (hours)")+
  scale_x_continuous(limits=c(0,6*60),breaks=seq(0,6*60,60))+
  scale_y_continuous(limits=c(0,5*24),breaks=seq(0,5*24,24))
timeToRecoveryByTimeToView+ggtitle("Correlation betwen time to view and time to resolution of creatinine abnormality")
ggsave('timeToCreatRecoveryByTimeToView.png',width=10,height=5,units='in')

timeToRecoveryByDegree <- ggplot(
  data=data2 %>% filter(degree > -5) %>% filter(degree < 10) %>% filter( minutes_to_resolution < 5*24*60), 
  aes(x=degree, y=minutes_to_resolution/60)) +
  geom_bin2d(na.rm = TRUE, bins=120) +
  scale_fill_gradient(low = "#f8f8ff", high = "#2020ff") +
  xlab("degree of abnormality") +
  ylab("time to resolution of abnormality  (hours)")+
  scale_x_continuous(limits=c(-5,10),breaks=seq(-5,10,1))+
  scale_y_continuous(limits=c(0,5*24),breaks=seq(0,5*24,24))
timeToRecoveryByDegree+ggtitle("Correlation betwen time to degree and time to resolution of test abnormality")
ggsave('timeToRecoveryByDegreeAbnormality.png',width=10,height=5,units='in')

#########################################
# Detailed correlations
# https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
plot_bubble = function(input,grouper,bubble_colour) {
  grouper=enquo(grouper);
  tmpCorrelations <- input %>%
    filter(minutes_to_view > 0) %>% filter( minutes_to_resolution < 31*24*60) %>% 
    group_by(!!grouper) %>%
    summarize(
      count=n(),
      median=fivenum(minutes_to_resolution, na.rm = TRUE)[3],
      cor = cor(minutes_to_resolution, minutes_to_view, use="complete.obs", method="spearman")
    ) %>% 
    mutate(the_label=!!grouper)
  
  return(ggplot(
    tmpCorrelations %>% filter(median<18000) %>% top_n(40,count),
    aes(x=median/60,y=cor,label=the_label,size=count))+
    geom_hline(aes(yintercept=0),color="#808080")+
    geom_point(shape=21, fill=bubble_colour, colour="black")+
    scale_size_area(max_size=20)+
    geom_text_repel(size=3, segment.colour="#00000080") +
    ylab("correlation with time to view") +
    xlab("median time to resolution of abnormality  (hours)")+
    expand_limits(x = 0, y=c(-0.15,0.25))+
    scale_x_continuous(breaks=seq(0,12000/60,12)))
}

plot_bubble(data2,test_name,"#ff808080")+ggtitle("Time to recovery and correlation to time to view by test")
ggsave('timeToRecoveryAndTimeToViewByTest.png',width=10,height=5,units='in')
plot_bubble(data2,ward_name,"#80ff8080")+ggtitle("Time to recovery and correlation to time to view by ward")
ggsave('timeToRecoveryAndTimeToViewByWard.png',width=10,height=5,units='in')
plot_bubble(data2,patient_group,"#8080ff80")+ggtitle("Time to recovery and correlation to time to view by patient group")
ggsave('timeToRecoveryAndTimeToViewByPatientGroup.png',width=10,height=5,units='in')
plot_bubble(data2,day_time_cat,"#0c0c0c80")+ggtitle("Time to recovery and correlation to time to view by time of day")
ggsave('timeToRecoveryAndTimeToViewByTimeOfDay.png',width=10,height=5,units='in')
plot_bubble(data2,degree_cat,"#c080c080")+ggtitle("Time to recovery and correlation to time to view by degree abnormality") +
  scale_x_continuous(limits=c(0,144),breaks=seq(0,144,12))
ggsave('timeToRecoveryAndTimeToViewByDegreeAbormality.png',width=10,height=5,units='in')
plot_bubble(data2,total_views,"#80c0c080")+ggtitle("Time to recovery and correlation to time to view by number of viewing clinicians")
ggsave('timeToRecoveryAndTimeToViewByNumberOfInvolvedClinicians.png',width=10,height=5,units='in')
plot_bubble(data2,age_group,"#d0d08080")+ggtitle("Time to recovery and correlation to time to view by patient decade")
ggsave('timeToRecoveryAndTimeToViewByPatientDecade.png',width=10,height=5,units='in')

##############################

## TODO: something wrong here with the median_ttv enquo business

plot_box = function(input,grouper,orderer=NA,box_colour) {
  grouper=enquo(grouper);
  

  tmpData <- input %>%
    filter(minutes_to_resolution > 0) %>% filter( minutes_to_resolution < 31*24*60) %>%
    group_by(!!grouper) %>%
    summarize(
      count=n(),
      viewed_percent=sum(total_views != 0)/n(),
      lower_fence_ttv=fivenum(minutes_to_resolution, na.rm = TRUE)[1],
      lower_quartile_ttv=fivenum(minutes_to_resolution, na.rm = TRUE)[2],
      median_ttv=fivenum(minutes_to_resolution, na.rm = TRUE)[3],
      upper_quartile_ttv=fivenum(minutes_to_resolution, na.rm = TRUE)[4],
      higher_fence_ttv=fivenum(minutes_to_resolution, na.rm = TRUE)[5]
    ) 
  
  if (is.na(orderer)) orderer=enquo(grouper) else orderer=enquo(orderer);
  
  tmpData <- tmpData %>% 
    mutate(
      the_label=!!grouper,
      the_order=!!orderer
    )
  View(tmpData)
  return(
    ggplot(tmpData %>% top_n(40,count) , aes(x=reorder(the_label,the_order))) +
      geom_boxplot(
        aes(
          lower = lower_quartile_ttv/60,
          upper = upper_quartile_ttv/60,
          middle = median_ttv/60,
          ymin = 0,
          ymax = (upper_quartile_ttv+1.5*(upper_quartile_ttv-lower_quartile_ttv))/60
        ), 
        stat="identity", fill=box_colour, size=0.3) +
      theme(axis.text.x = element_text(angle=90,size=9,vjust=1,hjust=1),axis.title.x=element_blank())+
      ylab("time to resolution of abnormality (hours)")+
      scale_y_continuous(breaks=seq(0,60*24,48))
  )
}

plot_box(data2,test_name,median_ttv,"#ff808080")+ggtitle("Time to resolution of abnormality by test")
ggsave('timeToRecoveryByTest.png',width=10,height=5,units='in')
plot_box(data2,ward_name,median_ttv,"#80ff8080")+ggtitle("Time to resolution of abnormality by ward")
ggsave('timeToRecoveryByWard.png',width=10,height=5,units='in')
plot_box(data2,patient_group,median_ttv,"#8080ff80")+ggtitle("Time to resolution of abnormality by patient group")
ggsave('timeToRecoveryByPatientGroup.png',width=10,height=5,units='in')
plot_box(data2,day_time_cat,NA,"#0c0c0c80")+ggtitle("Time to resolution of abnormality by time of day")
ggsave('timeToRecoveryByTimeOfDay.png',width=10,height=5,units='in')
plot_box(data2,degree_cat,NA,"#c080c080")+ggtitle("Time to resolution of abnormality by degree abnormality")
ggsave('timeToRecoveryByDegreeAbormality.png',width=10,height=5,units='in')
plot_box(data2,total_views,NA,"#80c0c080")+ggtitle("Time to resolution of abnormality by number of viewing clinicians")
ggsave('timeToRecoveryByNumberOfInvolvedClinicians.png',width=10,height=5,units='in')
plot_box(data2,age_group,NA,"#d0d080")+ggtitle("Time to resolution of abnormality by patient decade")
ggsave('timeToRecoveryAndTimeToViewByPatientDecade.png',width=10,height=5,units='in')







####################
cor(x=data_creat$minutes_to_view, y=data_creat$minutes_to_resolution, use="complete.obs", method="spearman")
cor(x=data2$patient_age, y=data2$minutes_to_resolution, use="complete.obs")
cor(x=data2$patient_age, y=data2$minutes_to_resolution, use="complete.obs")
summary(lm(minutes_to_resolution ~ degree, data=data2))
cor(data2$minutes_to_resolution, data2$minutes_to_view, use="complete.obs")




# cor(data_no_na$patient_age, data_no_na$minutes_to_resolution)
# cor(data2, use="complete.obs", method="kendall") 

# https://stats.stackexchange.com/questions/108007/correlations-with-categorical-variables


# ggplot(data2,aes(x=minutes_to_resolution,y=minutes_to_view)) + 
#   geom_point(colour="blue", alpha=0.2) + 
#   geom_density2d(colour="black")