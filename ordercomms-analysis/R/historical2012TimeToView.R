# By month
# Weekly time to view time series analysis
# install.packages("pwr")
# install.packages("stargazer")
# install.packages("htmlTable")
# install.packages("broom")
# install.packages("formattable")

library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(ggrepel)
library(cowplot)
library(ggpubr)
library(scales)
library(pwr)
library(stargazer)
library(htmlTable)
library(formattable)
library(knitr)
library(broom)

source_directory <- getSrcDirectory(function(dummy) {dummy});
source(paste(source_directory,'/standardAxes.R',sep=""));
source(paste(source_directory,'/utils.R',sep=""));
createDirectory();

##############
#
source(paste(source_directory,'/getTimeToViewData.R',sep=""));
source(paste(source_directory,'/getTimeToRecoveryData.R',sep=""));

##############
# DATA

data_2012 <- data %>% filter(
	investigation_abnormal==1 
	& discipline_name=='Chem/Haem'
	& as.Date(date) < as.Date("2016-09-01")
	& as.Date(date) >= as.Date("2012-09-01")
);

data2_2012 <- data2 %>% filter(
	as.Date(date) < as.Date("2016-09-01")
	& as.Date(date) >= as.Date("2012-10-01")
);

timespans <- data.frame(
	xmin=c(as.Date("2013-02-01"),as.Date("2014-11-01")),
	xmax=c(as.Date("2014-02-01"),as.Date("2015-11-01")),
	label=c("pre","post")
)

#################
# DISTRIBUTION

data_count <- nrow(data_2012);
viewed_total <- nrow(data_2012 %>% filter(viewed));
data2_count <- nrow(data2_2012);

# mean clinician views


# ttv distribution by minute
data_2012_by_ttv <- data_2012 %>%
	filter(minutes_to_view > 0) %>%
	group_by(minutes_to_view) %>%
	summarize(
		count=n()
	) %>% mutate(
		cumulative=cumsum(count),
		recovered_percent=cumulative / data_count,
		norm_count=count/sd(count)
	)

# ttr distribution by hour
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

# Time to view distribution graph
h2012_ttv_distribution <- ggplot(
			data = data_2012_by_ttv %>% filter(minutes_to_view < 60*24),
			aes(x=minutes_to_view,y=norm_count)
	)+
	geom_hline(yintercept = viewed_total/data_count*20, colour="#ff0000") +
	geom_area(stat="identity",fill="#808080")+
	geom_line(stat="identity",size=1,colour="#000000")+
	geom_line(aes(y=recovered_percent*20),size=1,colour="#ffc0c0")+
	xlab("minutes to view")+
	scale_x_continuous(limits=c(0,60*24),breaks = seq(0, 60*24, 60*3))+
	ylab("density")+
	scale_y_continuous(
		limits=c(0,20),
		breaks=seq(0, 20, 5),
				sec.axis=sec_axis(~./20*100, name="% viewed")
		)+
		theme(
		axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
		axis.text=element_text(size=18),
		axis.title=element_text(size=18,face="bold")
	)

h2012_ttv_distribution;
ggsave('2012timeToViewDistribution.svg',width=5,height=3.5,units='in')

# Time to recovery distribution graph
h2012_ttr_distribution <- ggplot(
		data = data2_2012_by_ttr %>% filter(hours_to_resolution < 120),
		aes(x=hours_to_resolution,y=norm_count)
	)+
	geom_area(stat="identity",fill="#c0c0ff")+
		geom_line(stat="identity",size=1, colour="#0000ff")+
	xlab("hours to recovery")+
	ylab("density")+
		scale_x_continuous(limits=c(0,120),breaks = seq(0, 240, 12))+
	scale_y_continuous(limits=c(0,70), breaks = seq(0, 80, 20))+
		theme(
		axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
		axis.text=element_text(size=18),
		axis.title=element_text(size=18,face="bold")
	)

h2012_ttr_distribution
ggsave('2012timeToRecoveryDistribution.svg',width=5,height=3.5,units='in')

# Clinicians viewing distribution graph
h2012_clinician_views_distribution <- ggplot(
		data_2012, 
		aes(x=total_views)
	) +
	geom_histogram(bins = max(data$total_views),fill="#ffb080",colour="black") +
	xlab("no. clinicians")+
	scale_x_continuous(breaks=seq(0,50,5))

h2012_clinician_views_distribution
ggsave('2012clinicianViewsDistribution.svg',width=3.5,height=3.5,units='in')

h2012_distribution <- plot_grid(
	h2012_ttv_distribution,
	h2012_ttr_distribution,
	nrow=2,
	align="h",
	rel_widths=c(2,1),
	labels = c("A","B")
)
h2012_distribution

save_plot("2012distributionPub.svg", h2012_distribution,base_height = 7,base_width = 8)

################################
# Summarise data by day

tmpOrigin <- timespans$xmin[1] + difftime(timespans$xmax[1],timespans$xmin[1])/2

## Time to view - all
h2012_data_by_day <- data_2012 %>% 
	mutate(time_group = as.Date(date)) %>%
	filter(time_group != as.Date("2015-09-06")) %>%
	group_by(time_group) %>%
	summarize(
			count=n(),
			unviewed_count=sum(total_views == 0),
			unviewed_percent=sum(total_views == 0)/n(),
			lower_quartile_ttv=fivenum(minutes_to_view, na.rm = TRUE)[2],
			median_ttv=fivenum(minutes_to_view, na.rm = TRUE)[3],
			upper_quartile_ttv=fivenum(minutes_to_view, na.rm = TRUE)[4],
			views = sum(total_views),
			mean_views = mean(total_views)
	) %>% 
	mutate(
	  days=as.numeric(difftime(time_group,tmpOrigin,units="day")),
	  period=cut(time_group, 
	             breaks=c(as.Date('2000-01-01'),timespans$xmin[1],timespans$xmax[1],timespans$xmin[2],timespans$xmax[2],as.Date('2100-01-01')),
	             labels=FALSE,
	             include.lowest=TRUE
	             )
	)

lm.count = lm(count ~ days,h2012_data_by_day %>% filter(period==2))
lm.unviewed_percent = lm(unviewed_percent ~ days,h2012_data_by_day %>% filter(period==2))
lm.median_ttv = lm(median_ttv ~ days,h2012_data_by_day %>% filter(period==2))
lm.mean_views = lm(mean_views ~ days,h2012_data_by_day %>% filter(period==2))
h2012_data_by_day['resid_count'] <- h2012_data_by_day['count']-predict.lm(lm.count,h2012_data_by_day)
h2012_data_by_day['resid_unviewed_percent'] <- h2012_data_by_day['unviewed_percent']-predict.lm(lm.unviewed_percent,h2012_data_by_day)
h2012_data_by_day['resid_median_ttv'] <- h2012_data_by_day['median_ttv']-predict.lm(lm.median_ttv,h2012_data_by_day)
h2012_data_by_day['resid_mean_views'] <- h2012_data_by_day['mean_views']-predict.lm(lm.mean_views,h2012_data_by_day)


width <- 21 # days
norm_filter <- dnorm(seq(-2,2, length = width))/sum(dnorm(seq(-2,2, length = width)))

h2012_data_by_day['roll_count'] <- stats::filter(h2012_data_by_day['count'],filter=norm_filter,method='convolution',sides=2,circular=FALSE)
h2012_data_by_day['roll_unviewed_count'] <- stats::filter(h2012_data_by_day['unviewed_count'],filter=norm_filter,method='convolution',sides=2,circular=FALSE)
h2012_data_by_day['roll_median_ttv'] <- stats::filter(h2012_data_by_day['median_ttv'],filter=norm_filter,method='convolution',sides=2,circular=FALSE)
h2012_data_by_day['roll_lower_quartile_ttv'] <- stats::filter(h2012_data_by_day['lower_quartile_ttv'],filter=norm_filter,method='convolution',sides=2,circular=FALSE)
h2012_data_by_day['roll_upper_quartile_ttv'] <- stats::filter(h2012_data_by_day['upper_quartile_ttv'],filter=norm_filter,method='convolution',sides=2,circular=FALSE)
h2012_data_by_day['roll_views'] <- stats::filter(h2012_data_by_day['views'],filter=norm_filter,method='convolution',sides=2,circular=FALSE)
h2012_data_by_day <- mutate(h2012_data_by_day, roll_unviewed_percent = roll_unviewed_count/roll_count);
h2012_data_by_day <- mutate(h2012_data_by_day, roll_mean_views = roll_views/roll_count);

## Time to view - viewed
viewed_h2012_data_by_day <- data_2012 %>%
	filter(date!=as.POSIXct("2015-09-06")) %>%
	filter(viewed) %>%
	mutate (time_group = as.Date(date)) %>%
	filter(time_group != as.Date("2015-09-06")) %>%
	group_by(time_group) %>%
	dplyr::summarize(
		count=n(),
		median_age=fivenum(minutes_to_view, na.rm = TRUE)[3]
	)  %>%
  mutate(
    days=as.numeric(difftime(time_group,tmpOrigin,units="day")),
    period=cut(time_group,
               breaks=c(as.Date('2000-01-01'),timespans$xmin[1],timespans$xmax[1],timespans$xmin[2],timespans$xmax[2],as.Date('2100-01-01')),
               labels=FALSE,
               include.lowest=TRUE
    )
  )

lm.viewed = lm(count ~ days,viewed_h2012_data_by_day %>% filter(period==2))
viewed_h2012_data_by_day['resid_count'] <- viewed_h2012_data_by_day['count']-predict.lm(lm.viewed,viewed_h2012_data_by_day)

viewed_h2012_data_by_day['roll_count'] <- stats::filter(viewed_h2012_data_by_day['count'],filter=norm_filter,method='convolution',sides=2,circular=TRUE)
viewed_h2012_data_by_day['roll_median_age'] <- stats::filter(viewed_h2012_data_by_day['median_age'],filter=norm_filter,method='convolution',sides=2,circular=TRUE)

## Time to recovery
h2012_data2_by_day <- data2_2012 %>% 
	mutate (time_group = as.Date(date)) %>%
	filter(time_group != as.Date("2015-09-06")) %>%
	group_by(time_group) %>%
	summarize(
		count=n(),
		lower_quartile_ttr=fivenum(minutes_to_resolution, na.rm = TRUE)[2],
		median_ttr=fivenum(minutes_to_resolution, na.rm = TRUE)[3],
		upper_quartile_ttr=fivenum(minutes_to_resolution, na.rm = TRUE)[4]
	) %>% 
  mutate(
    days=as.numeric(difftime(time_group,tmpOrigin,units="day")),
    period=cut(time_group, 
               breaks=c(as.Date('2000-01-01'),timespans$xmin[1],timespans$xmax[1],timespans$xmin[2],timespans$xmax[2],as.Date('2100-01-01')),
               labels=FALSE,
               include.lowest=TRUE
    )
  )

lm.median_ttr = lm(median_ttr ~ days,h2012_data2_by_day %>% filter(period==2))
h2012_data2_by_day['resid_median_ttr'] <- h2012_data2_by_day['median_ttr']-predict.lm(lm.median_ttr,h2012_data2_by_day)

h2012_data2_by_day['roll_count'] <- stats::filter(h2012_data2_by_day['count'],filter=norm_filter,method='convolution',sides=2,circular=FALSE)
h2012_data2_by_day['roll_median_ttr'] <- stats::filter(h2012_data2_by_day['median_ttr'],filter=norm_filter,method='convolution',sides=2,circular=FALSE)
h2012_data2_by_day['roll_lower_quartile_ttr'] <- stats::filter(h2012_data2_by_day['lower_quartile_ttr'],filter=norm_filter,method='convolution',sides=2,circular=FALSE)
h2012_data2_by_day['roll_upper_quartile_ttr'] <- stats::filter(h2012_data2_by_day['upper_quartile_ttr'],filter=norm_filter,method='convolution',sides=2,circular=FALSE)





#################
# PLOTS
#################

################
# TIME SERIES
# Median time to view test result over week
# TODO: error bars instead of upper and lower quartile points or some area
h2012_plot_ttv <- 
	ggplot(
		h2012_data_by_day, 
		aes(x=time_group)) +
		xlab("date")+
	geom_rect(data=timespans, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=+Inf),fill="#e0e0e080",size=1,inherit.aes = FALSE)+
  geom_smooth(
    aes(x=time_group,y=median_ttv,weight=ifelse(period==2,1,0)),
    method="lm",
    colour="#000000",
    fill="#808080"
  )+
	scale_x_date(date_breaks="3 months",date_labels="%b %Y")+
	theme(axis.text.x = element_text(angle=45,size=9,hjust=1,vjust=1)) +
		ylab("minutes")+
	#	scale_y_continuous(limits=c(0,600), breaks = seq(0, 600, 60))+
  	scale_y_continuous(limits=c(60,180), breaks = seq(0, 360, 60))+
  	geom_point(aes(y = median_ttv),color='#808080',size=0.5)+
		geom_line(aes(y = roll_median_ttv, group =1), color="#404040",size = 1)
	# geom_point(aes(y = lower_quartile_ttv),color='#c0c0c0',size=1)+
	# geom_line(aes(y = roll_lower_quartile_ttv, group =1), color="#808080",size = 1)+
	# geom_point(aes(y = upper_quartile_ttv),color='#c0c0c0',size=1)+
	# geom_line(aes(y = roll_upper_quartile_ttv, group =1), color="#808080",size = 1)+
  

h2012_plot_ttv#+ggtitle("median time to view test result over time")
ggsave('medianTtvH2012.svg',width=10,height=5,units='in')

# Percent unviewed view test result over week
h2012_plot_unviewed <- 
	ggplot(h2012_data_by_day, aes(x=time_group)) +
	geom_rect(data=timespans, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=+Inf),fill="#e0e0e080",size=1,inherit.aes = FALSE)+
  geom_smooth(
    aes(x=time_group,y=unviewed_percent*100,weight=ifelse(period==2,1,0)),
    method="lm",
    colour="#800000",
    fill="#806060"
  )+
  xlab("date")+
	scale_x_date(date_breaks="3 months",date_labels="%b %Y")+
	theme(axis.text.x = element_text(angle=45,size=9,hjust=1,vjust=1)) +
	ylab("percent")+
		scale_y_continuous(limits=c(2.5,15),breaks = seq(0, 100, 5)) +
		geom_point(aes(y = unviewed_percent*100),color='#ff8080',size=0.5)+
		geom_line(aes(y = roll_unviewed_percent*100, group=1), color="red",size = 1)

h2012_plot_unviewed#+ggtitle("proportion of unviewed test results per day over time")
ggsave('unreviewedRatesH2012.svg',width=10,height=5,units='in')

# totals viewed result over week
h2012_plot_counts <- 
	ggplot() +
	geom_rect(data=timespans, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=+Inf),fill="#e0e0e080",size=1,inherit.aes = FALSE)+
	xlab("date")+
	scale_x_date(date_breaks="3 months",date_labels="%b %Y")+
	theme(axis.text.x = element_text(angle=45,size=9,hjust=1,vjust=1)) +
		ylab("results/day")+
		# scale_y_continuous(breaks = seq(0, 100, 5)) +
		geom_point(data=h2012_data_by_day, aes(x=time_group,y = count),color='#ffb0ff',size=0.5)+
		geom_point(data=viewed_h2012_data_by_day, aes(x=time_group,y = count),color='#c0c0ff',size=0.5)+
		geom_line(data=h2012_data_by_day, aes(x=time_group,y = roll_count,group=1), color="magenta",size = 1)+
		geom_line(data=viewed_h2012_data_by_day, aes(x=time_group,y = roll_count,group=2), color="blue",size = 1)

h2012_plot_counts#+ggtitle("test results and result views over time")
ggsave('resultAndViewRatesH2012.svg',width=10,height=5,units='in')

# Median time to view test result over week
h2012_plot_viewers <- 
	ggplot(data=h2012_data_by_day) +
	geom_rect(data=timespans, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=+Inf),fill="#e0e0e080",size=1,inherit.aes = FALSE)+
  geom_smooth(
    aes(x=time_group,y=mean_views,weight=ifelse(period==2,1,0)),
    method="lm",
    colour="#804000",
    fill="#808060"
  )+
	xlab("date")+
	scale_x_date(date_breaks="3 months",date_labels="%b %Y")+
	theme(axis.text.x = element_text(angle=45,size=9,hjust=1,vjust=1)) +
	ylab("no. viewers")+
	scale_y_continuous(limits=c(2.25,3.5)) +
	geom_point(aes(x=time_group,y = mean_views),color='#ffd0b0',size=0.5)+
	geom_line(aes(x=time_group,y = roll_mean_views), color="orange",size = 1)

h2012_plot_viewers#+ggtitle("average clinicians viewing over time")
ggsave('viewsH2012.svg',width=10,height=5,units='in')

# Median time to recovery over week
h2012_plot_ttr <- 
	ggplot(
		h2012_data2_by_day, 
		aes(x=time_group)) +
	xlab("date")+
	geom_rect(data=timespans, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=+Inf),fill="#e0e0e080",size=1,inherit.aes = FALSE)+
  geom_smooth(
    aes(x=time_group,y=median_ttr/60,weight=ifelse(period==2,1,0)),
    method="lm",
    colour="#000080",
    fill="#606080"
  )+
	scale_x_date(date_breaks="3 months",date_labels="%b %Y")+
	theme(axis.text.x = element_text(angle=45,size=9,hjust=1,vjust=1)) +
	ylab("hours")+
	scale_y_continuous(limits=c(0,480), breaks = seq(0, 600, 24*7))+
	geom_point(aes(y = median_ttr/60),color='#8080ff',size=0.5)+
	geom_line(aes(y = roll_median_ttr/60, group =1), color="blue",size = 1);
	
h2012_plot_ttr;##+ggtitle("median time to view test result over time")
ggsave('medianTtrHistorical.svg',width=10,height=5,units='in')

###############
# VARIANCES
#

# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#required-r-package
# https://cran.r-project.org/web/packages/cowplot/vignettes/introduction.html
######
# Pre and post data sets
h2012_data_by_day_pre <- h2012_data_by_day %>% filter(period==2)
h2012_data_by_day_post <- h2012_data_by_day %>% filter(period==4)
h2012_data2_by_day_pre <- h2012_data2_by_day %>% filter(period==2)
h2012_data2_by_day_post <- h2012_data2_by_day %>% filter(period==4)

#TODO fix limits on variance graphs
h2012_variance_ttv_pre <- ggplot(h2012_data_by_day_pre,aes(x=resid_median_ttv))+
	geom_histogram(bins=20,fill='#808080',colour='black')+
	xlab("residual")+
	expand_limits(x=c(-75,+75),y=c(0,100))
h2012_variance_ttv_post <- ggplot(h2012_data_by_day_post,aes(x=resid_median_ttv))+
	geom_histogram(bins=20,fill='#808080',colour='black')+
	xlab("residual")+
  expand_limits(x=c(-75,+75),y=c(0,100))

h2012_variance_ttv_dense <- ggplot()+
  geom_density(data=h2012_data_by_day_post,aes(x=resid_median_ttv),fill='#c0c0c080',colour='black')+
  geom_density(data=h2012_data_by_day_pre,aes(x=resid_median_ttv),fill='#40404080',colour='black')+
  xlab("residual")+
  expand_limits(x=c(-75,+75))

h2012_variance_unviewed_pre <- ggplot(h2012_data_by_day_pre, aes(x=resid_unviewed_percent*100))+
	geom_histogram(bins=20,fill='#ff8080',colour='black')+
	xlab("residual")+
	expand_limits(x=c(-8,8),y=c(0,80))
h2012_variance_unviewed_post <- ggplot(h2012_data_by_day_post, aes(x=resid_unviewed_percent*100))+
	geom_histogram(bins=20,fill='#ff8080',colour='black')+
	xlab("residual")+
	expand_limits(x=c(-8,8),y=c(0,80))

h2012_variance_unviewed_dense <- ggplot()+
  geom_density(data=h2012_data_by_day_post,aes(x=resid_unviewed_percent*100),fill='#ffc0c080',colour='black')+
  geom_density(data=h2012_data_by_day_pre,aes(x=resid_unviewed_percent*100),fill='#ff404080',colour='black')+
  xlab("residual")+
  expand_limits(x=c(-7.5,+7.5))

h2012_variance_viewers_pre <- ggplot(h2012_data_by_day_pre, aes(x=resid_mean_views))+
	geom_histogram(bins=20,fill='#ffd0b0',colour='black')+
	xlab("residual")+
	expand_limits(x=c(-1,1),y=c(0,80))
h2012_variance_viewers_post <- ggplot(h2012_data_by_day_post, aes(x=resid_mean_views))+
	geom_histogram(bins=20,fill='#ffd0b0',colour='black')+
	xlab("residual")+
	expand_limits(x=c(-1,1),y=c(0,80))

h2012_variance_viewers_dense <- ggplot()+
  geom_density(data=h2012_data_by_day_post,aes(x=resid_mean_views),fill='#ffd0b080',colour='black')+
  geom_density(data=h2012_data_by_day_pre,aes(x=resid_mean_views),fill='#ffb09080',colour='black')+
  xlab("residual")+
  expand_limits(x=c(-1,+1))

h2012_variance_ttr_pre <- ggplot(h2012_data2_by_day_pre,aes(x=resid_median_ttr/60))+
	geom_histogram(bins=20,fill='#8080ff',colour='black')+
	xlab("median time to view")+
	scale_x_continuous(limits = c(-300, 300))+
	scale_y_continuous(limits = c(0, 120))
h2012_variance_ttr_post <- ggplot(h2012_data2_by_day_post,aes(x=resid_median_ttr/60))+
	geom_histogram(bins=20,fill='#8080ff',colour='black')+
	xlab("median time to view")+
	scale_x_continuous(limits = c(-300, 300))+
	scale_y_continuous(limits = c(0, 120))

h2012_variance_ttr_dense <- ggplot()+
  geom_density(data=h2012_data2_by_day_post,aes(x=resid_median_ttr/60),fill='#c0c0ff80',colour='black')+
  geom_density(data=h2012_data2_by_day_pre,aes(x=resid_median_ttr/60),fill='#8080ff80',colour='black')+
  xlab("residual")+
  scale_x_continuous(limits = c(-300, 300))

# tmp <- sd(h2012_data_by_day$mean_views)
poster <- theme(plot.title=element_text(size=24),axis.text.y=element_text(size=24),axis.text.x=element_text(size=18),axis.title.y=element_text(size=24,face="bold"),axis.title.x=element_text(size=24,face="bold"))
poster2 <- theme(plot.title=element_text(size=24),axis.text.y=element_text(size=24),axis.text.x=element_blank(),axis.title.y=element_text(size=24,face="bold"),axis.title.x=element_blank())
poster3 <- theme(axis.text.y=element_blank(),axis.text.x=element_text(size=14),axis.title.y=element_blank(),axis.title.x=element_blank())
h2012_resids <- plot_grid(
  h2012_variance_ttv_dense+poster3+ggtitle("time to view"),
  h2012_variance_unviewed_dense+poster3+ggtitle("not viewed"),
  h2012_variance_viewers_dense+poster3+ggtitle("no. viewers"),
  h2012_variance_ttr_dense+poster3+ggtitle("time to recovery"),
  nrow=1,
  rel_widths=c(1,1,1,1),
  align="hv")

h2012 <- plot_grid(
    h2012_plot_counts+poster2+ggtitle("tests performed and reviewed"),
		h2012_plot_ttv+poster2+ggtitle("time to view"),
		h2012_plot_unviewed+poster2+ggtitle("percentage not viewed"),
		h2012_plot_viewers+poster2+ggtitle("number of viewers"),
		h2012_plot_ttr+poster+ggtitle("time to recovery"),
		nrow=5,
		align="hv"
)
#h2012;
save_plot("h2012Pub.svg", h2012,
					base_height = 20,
					base_width = 10
)

save_plot("h2012Var.png", h2012_resids,
          base_height = 3,
          base_width = 10
)
###################################
# correlations

# timeToViewByTime <- ggplot(
#   data=data_2012 %>% filter(minutes_to_view > 0),
#   aes(x=date,y=minutes_to_view)) +
#   geom_bin2d(na.rm = TRUE, binwidth=c(1,1)) +
#   scale_fill_gradient(low = "#f8f8ff", high = "#2020ff") +
#   xlab("date") +
#   ylab("time to view (minutes)")+
#   # scale_x_continuous(limits=c(0,6*60),breaks=seq(0,6*60,60))+
#   scale_y_continuous(limits=c(0,600),breaks=seq(0,600,60))+
#   theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust=0.5),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))


timeToRecoveryByTimeToView <- ggplot(
				data=data2_2012 %>% filter(minutes_to_view > 0) %>% filter(minutes_to_view < 6*60) %>% filter( minutes_to_resolution < 5*24*60), 
				aes(x=minutes_to_view, y=minutes_to_resolution/60)) +
		geom_bin2d(na.rm = TRUE, bins=120) +
		scale_fill_gradient(low = "#f8f8ff", high = "#2020ff") +
		xlab("time to view (minutes)") +
		ylab("time to resolution (hours)")+
	scale_x_continuous(limits=c(0,6*60),breaks=seq(0,6*60,60))+
	scale_y_continuous(limits=c(0,5*24),breaks=seq(0,5*24,24))+
	theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust=0.5),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))
timeToRecoveryByTimeToView;##+ggtitle("Correlation betwen time to view and time to resolution of test abnormality")
ggsave('timeToRecoveryByTimeToView.svg',width=5,height=3.5,units='in')

cor(h2012_data_by_day$median_ttv, h2012_data2_by_day$median_ttr)

##############
# regressions

# stargazer(
# 	lm(median_ttv~days,pre),
# 	lm(upper_quartile_ttv~days,pre),
# 	lm(lower_quartile_ttv~days,pre),
# 	lm(mean_views~days,pre),
# 	lm(unviewed_percent~days,pre),
# 	type="html" ,out="2012table1.html")



# unlist might be useful here


# http://my.ilstu.edu/~wjschne/444/IndependentSamples.html#(9)
# https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

# var.test(pre$median_ttv,post$median_ttv)

ttest_median_ttv <- t.test(x=h2012_data_by_day_pre$resid_median_ttv, y=h2012_data_by_day_post$resid_median_ttv )
ttest_unviewed_percent <- t.test(x=h2012_data_by_day_pre$resid_unviewed_percent*100, y=h2012_data_by_day_post$resid_unviewed_percent*100 )
ttest_mean_views <- t.test(x=h2012_data_by_day_pre$resid_mean_views, y=h2012_data_by_day_post$resid_mean_views )
ttest_median_ttr <- t.test(x=h2012_data2_by_day_pre$resid_median_ttr/60, y=h2012_data2_by_day_post$resid_median_ttr/60 )

# data_frame(names(ttest_lq_ttv),ttest_median_ttv,ttest_uq_ttv,ttest_lq_ttv,ttest_unviewed,ttest_mean_views)
# ldply(c(ttest_median_ttv))#,ttest_uq_ttv)) #,ttest_lq_ttv,ttest_unviewed,ttest_mean_views))

ttest_df <- rbind(
			tidy(ttest_median_ttv),
			tidy(ttest_unviewed_percent),
			tidy(ttest_mean_views),
			tidy(ttest_median_ttr)
		)
ttest_df$name = c("Median time to view", "Unviewed tests", "No. clinicians viewing", "Median time to recovery")
# formattable cannot reorder columns
# formattable(ttest_df, list(
#	 conf.low = FALSE,
#	 conf.high = FALSE,
#	 statistic = FALSE,
#	 parameter = FALSE)
# )


ttest_out <- data.frame(name = c("Median time to view", "Unviewed tests", "No. clinicians viewing", "Median time to recovery"))
ttest_out$effect.size <- ttest_df$estimate
ttest_out$p.value <- ttest_df$p.value
ttest_out$conf.low <- ttest_df$conf.low
ttest_out$conf.high <- ttest_df$conf.high

# export to file
cat(
	ttest_out %>% kable("html", digits=2),
	file="2012table2.html")

