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

load(file="~/R/timeToRecoveryData");



timespans <- data.frame(
	xmin=c(as.Date("2013-02-01"),as.Date("2014-11-01")),
	#xmax=c(as.Date("2013-04-01"),as.Date("2015-01-01")),
	xmax=c(as.Date("2014-02-01"),as.Date("2015-11-01")),
	label=c("pre","post")
)

data2_2012 <- data2 %>% filter(
	as.Date(date) < as.Date("2016-09-01")
	& as.Date(date) >= as.Date("2012-10-01")
);

data2_count <- nrow(data2_2012);

data2_days <- floor(difftime(max(data2_2012$date),min(data2_2012$date),"days"))
# distribution by hour
data2_2012_by_ttr <- data2_2012 %>%
		mutate(
				hours_to_resolution = minutes_to_resolution %/% 60,
				total = n()
		) %>%
		group_by(hours_to_resolution) %>%
		summarize(
				count=n()
		) %>% mutate(
				cumulative=cumsum(count),
				recovered_percent=cumulative / data2_count,
				norm_count = count/sd(count)
		)

h2012_ttr_distribution <- ggplot(data = data2_2012_by_ttr %>% filter(hours_to_resolution < 120),
		aes(x=hours_to_resolution,y=norm_count))+
		geom_area(stat="identity",fill="#c0c0ff")+
		geom_line(stat="identity",size=1, colour="#0000ff")+
		# geom_line(aes(y=recovered_percent*70))+
		xlab("hours to recovery")+
		ylab("density")+
		scale_x_continuous(limits=c(0,120),breaks = seq(0, 240, 12))+
		scale_y_continuous(limits=c(0,70), breaks = seq(0, 80, 20) #,
		#		sec.axis=sec_axis(~.*100/70, name="% recovered"))+
		)+
		theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))

h2012_ttr_distribution
ggsave('timeToRecoveryDistribution.png',width=5,height=3.5,units='in')

#############################

# distribution by month
# data2_by_month <- data2 %>%
#		 filter(minutes_to_resolution < 31*24*60) %>%
#		 filter(as.Date(date) > as.Date("2014-08-31")) %>% 
# 		group_by(month_cat) %>%
# 		summarize(
# 				count=n(),
# 				viewed_percent=sum(total_views != 0)/n(),
# 				lower_fence=fivenum(minutes_to_resolution, na.rm = TRUE)[1],
# 				lower_quartile=fivenum(minutes_to_resolution, na.rm = TRUE)[2],
# 				median=fivenum(minutes_to_resolution, na.rm = TRUE)[3],
# 				upper_quartile=fivenum(minutes_to_resolution, na.rm = TRUE)[4],
# 				higher_fence=fivenum(minutes_to_resolution, na.rm = TRUE)[5]
# 		)
# 
# ttr_box_by_time <- 
#	 ggplot(data2_by_month, aes(x=month_cat)) +
#		 geom_boxplot(
#			 aes(
#				 lower = lower_quartile/60,
#				 upper = upper_quartile/60,
#				 middle = median/60,
#				 ymin = 0,
#				 ymax = (upper_quartile+1.5*(upper_quartile-lower_quartile))/60
#			 ), 
#			 stat="identity", fill="#c0c0c0", size=0.3) +
#		 theme(axis.text.x = element_text(angle=90,size=9))+
#		 xlab("month") +
#		 ylab("hours") +
#		 scale_y_continuous(limits=c(0,360),breaks = seq(0, 360, 24));
# 
# ttr_box_by_time #+ggtitle("Median time to test recovery by over time")
# ggsave('historicalTimeToRecoveryBoxPlot.png',width=10,height=5,units='in')

###################################
# correlations

timeToRecoveryByTimeToView <- ggplot(
				data=data2_2012 %>% filter(minutes_to_view > 0) %>% filter(minutes_to_view < 6*60) %>% filter( minutes_to_resolution < 5*24*60), 
				aes(x=minutes_to_view, y=minutes_to_resolution/60)) +
		geom_bin2d(na.rm = TRUE, bins=120) +
		scale_fill_gradient(low = "#f8f8ff", high = "#2020ff") +
		xlab("time to view (minutes)") +
		ylab("time to resolution (hours)")+
	scale_x_continuous(limits=c(0,6*60),breaks=seq(0,6*60,60))+
	scale_y_continuous(limits=c(0,5*24),breaks=seq(0,5*24,24))+
	theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))
timeToRecoveryByTimeToView;#+ggtitle("Correlation betwen time to view and time to resolution of test abnormality")
ggsave('timeToRecoveryByTimeToView.png',width=5,height=3.5,units='in')

####################

h2012_data2_by_day <- data2_2012 %>% 
	mutate (time_group = as.Date(date)) %>%
	filter(time_group != as.Date("2015-09-06")) %>%
	group_by(time_group) %>%
	summarize(
		count=n(),
		lower_quartile_ttr=fivenum(minutes_to_resolution, na.rm = TRUE)[2],
		median_ttr=fivenum(minutes_to_resolution, na.rm = TRUE)[3],
		upper_quartile_ttr=fivenum(minutes_to_resolution, na.rm = TRUE)[4]
	)
width <- 21 # days
h2012_data2_by_day['roll_count'] <- stats::filter(h2012_data2_by_day['count'],filter=rep(1/width,width),method='convolution',sides=2,circular=FALSE)
h2012_data2_by_day['roll_median_ttr'] <- stats::filter(h2012_data2_by_day['median_ttr'],filter=rep(1/width,width),method='convolution',sides=2,circular=FALSE)
h2012_data2_by_day['roll_lower_quartile_ttr'] <- stats::filter(h2012_data2_by_day['lower_quartile_ttr'],filter=rep(1/width,width),method='convolution',sides=2,circular=FALSE)
h2012_data2_by_day['roll_upper_quartile_ttr'] <- stats::filter(h2012_data2_by_day['upper_quartile_ttr'],filter=rep(1/width,width),method='convolution',sides=2,circular=FALSE)

###############
# PLOTS

# Median time to view test result over week
h2012_plot_ttr <- 
	ggplot(
		h2012_data2_by_day, 
		aes(x=time_group)) +
	xlab("date")+
	geom_rect(data=timespans, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=+Inf),fill="#e0e0e080",size=1,inherit.aes = FALSE)+
	scale_x_date(date_breaks="3 months",date_labels="%b %Y")+
	theme(axis.text.x = element_text(angle=45,size=9,hjust=1,vjust=1)) +
	ylab("hours")+
	scale_y_continuous(limits=c(0,480), breaks = seq(0, 600, 24*7))+
	geom_point(aes(y = median_ttr/60),color='#8080ff',size=1)+
	geom_line(aes(y = roll_median_ttr/60, group =1), color="blue",size = 1);
	

h2012_plot_ttr;#+ggtitle("median time to view test result over time")
ggsave('medianTtrHistorical.png',width=10,height=5,units='in')

# totals viewed result over week
h2012_plot_counts <- 
	ggplot() +
	xlab("date")+
	scale_x_date(date_breaks="3 months",date_labels="%b %Y")+
	theme(axis.text.x = element_text(angle=45,size=9,hjust=1,vjust=1)) +
	ylab("results/day")+
	# scale_y_continuous(breaks = seq(0, 100, 5)) +
	geom_point(data=h2012_data2_by_day, aes(x=time_group,y = count),color='#ffb0ff',size=1)+
	geom_line(data=h2012_data2_by_day, aes(x=time_group,y = roll_count,group=1), color="magenta",size = 1)
	
h2012_plot_counts+ggtitle("test results and result views over time")
ggsave('resultAndViewRatesHistorical.png',width=10,height=5,units='in')

# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#required-r-package
# https://cran.r-project.org/web/packages/cowplot/vignettes/introduction.html

## TODO: Pre and Post - banding etc.

h2012_data2_by_day_pre <- h2012_data2_by_day %>% filter(time_group >= timespans$xmin[1] & time_group < timespans$xmax[1])
h2012_data2_by_day_post <- h2012_data2_by_day %>% filter(time_group >= timespans$xmin[2] & time_group < timespans$xmax[2])

h2012_variance_ttr_pre <- ggplot(h2012_data2_by_day_pre,aes(x=median_ttr/60))+geom_histogram(bins=20,fill='#8080ff',colour='blue')+xlab("median time to view")+
	scale_x_continuous(limits = c(0, 40000/60))+
	scale_y_continuous(limits = c(0, 120))
h2012_variance_ttr_post <- ggplot(h2012_data2_by_day_post,aes(x=median_ttr/60))+geom_histogram(bins=20,fill='#8080ff',colour='blue')+xlab("median time to view")+
	scale_x_continuous(limits = c(0, 40000/60))+
	scale_y_continuous(limits = c(0, 120))

h2012 <- plot_grid(
	h2012_plot_ttr+rremove("x.text")+rremove("xlab"),
	h2012_variance_ttr_pre+rremove("ylab")+rremove("xlab"),
	h2012_variance_ttr_post+rremove("ylab")+rremove("xlab"),
	nrow=1,
	rel_widths=c(2,1,1),
	align="hv",
	labels = c("D1","D2","D3"))
historical;
save_plot("historicalTTRPub.png", historical,
					base_height = 2.5,
					base_width = 7.5
)








# cor(x=data_creat$minutes_to_view, y=data_creat$minutes_to_resolution, use="complete.obs", method="spearman")
# cor(x=data2$patient_age, y=data2$minutes_to_resolution, use="complete.obs")
# cor(x=data2$patient_age, y=data2$minutes_to_resolution, use="complete.obs")
# summary(lm(minutes_to_resolution ~ degree, data=data2))
# cor(data2$minutes_to_resolution, data2$minutes_to_view, use="complete.obs")




# cor(data_no_na$patient_age, data_no_na$minutes_to_resolution)
# cor(data2, use="complete.obs", method="kendall") 

# https://stats.stackexchange.com/questions/108007/correlations-with-categorical-variables


# ggplot(data2,aes(x=minutes_to_resolution,y=minutes_to_view)) + 
#	 geom_point(colour="blue", alpha=0.2) + 
#	 geom_density2d(colour="black")
