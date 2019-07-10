# Daily time to view time series analysis

library(ggplot2)
library(dplyr)
library(reshape2)
library(ggrepel)
library(cowplot)
library(ggpubr)

source_directory <- getSrcDirectory(function(dummy) {dummy});
if (source_directory == "") source_directory="/home/robchallen/Git/working-drafts/r"
source(paste(source_directory,'/standardAxes.R',sep=""));
source(paste(source_directory,'/utils.R',sep=""));
createDirectory();

tmp <- data_baseline %>%
  filter(as.Date(date) != as.Date("2015-09-06"))

prefix <- "weekday"
tmp <- tmp %>%  filter(time_of_week<=(5*24*60))
per_hour_scaling <- 3*52*5/60

# prefix <- "weekend"
# tmp <- tmp %>% filter(time_of_week>(5*24*60))
# per_hour_scaling <- 3*52*2/60
################
# DATA

data_by_day <- tmp %>%
  group_by(time_of_day) %>%
  summarize(
    count=n(),
    unviewed_count=sum(total_views == 0),
    unviewed_percent=unviewed_count/count,
    lower_quartile_ttv=fivenum(minutes_to_view, na.rm = TRUE)[2],
    median_ttv=fivenum(minutes_to_view, na.rm = TRUE)[3],
    upper_quartile_ttv=fivenum(minutes_to_view, na.rm = TRUE)[4]
  )

width <- 61 # minutes

data_by_day['roll_count'] <- stats::filter(data_by_day['count'],filter=rep(1/width,width),method='convolution',sides=2,circular=TRUE)
data_by_day['roll_unviewed_count'] <- stats::filter(data_by_day['unviewed_count'],filter=rep(1/width,width),method='convolution',sides=2,circular=TRUE)
data_by_day['roll_median_ttv'] <- stats::filter(data_by_day['median_ttv'],filter=rep(1/width,width),method='convolution',sides=2,circular=TRUE)
data_by_day['roll_lower_quartile_ttv'] <- stats::filter(data_by_day['lower_quartile_ttv'],filter=rep(1/width,width),method='convolution',sides=2,circular=TRUE)
data_by_day['roll_upper_quartile_ttv'] <- stats::filter(data_by_day['upper_quartile_ttv'],filter=rep(1/width,width),method='convolution',sides=2,circular=TRUE)
data_by_day <- mutate(data_by_day, roll_unviewed_percent = roll_unviewed_count/roll_count)

viewed_data_by_day <- tmp %>% filter(viewed) %>%
  group_by(time_of_day_viewed) %>%
  summarize(
    count=n(),
    median_age=fivenum(minutes_to_view, na.rm = TRUE)[3]
  )
viewed_data_by_day['roll_count'] <- stats::filter(viewed_data_by_day['count'],filter=rep(1/59,59),method='convolution',sides=2,circular=TRUE)
viewed_data_by_day['roll_median_age'] <- stats::filter(viewed_data_by_day['median_age'],filter=rep(1/59,59),method='convolution',sides=2,circular=TRUE)

data_by_hour_of_day <- tmp %>%
  mutate(time_group = time_of_day %/% 60) %>%
  group_by(time_group) %>%
  summarize(
    count=n(),
    viewed_percent=sum(total_views != 0)/n(),
    lower_fence_ttv=fivenum(minutes_to_view, na.rm = TRUE)[1],
    lower_quartile_ttv=fivenum(minutes_to_view, na.rm = TRUE)[2],
    median_ttv=fivenum(minutes_to_view, na.rm = TRUE)[3],
    upper_quartile_ttv=fivenum(minutes_to_view, na.rm = TRUE)[4],
    higher_fence_ttv=fivenum(minutes_to_view, na.rm = TRUE)[5]
  )

##################
# GRAPHS

# median roll TTV with minutely plot
by_day_plot_1 <- plot_day_xaxis(
  ggplot(data_by_day, aes(x=time_of_day/60)) +
  geom_point(aes(y = median_ttv),color='#80808080',size=1) +
  geom_line(aes(y =roll_median_ttv),colour='black',size=1)+
  ylab("minutes")+
  scale_y_continuous(limits=c(0,180), breaks = seq(0, 240, 60))
)

# unviewed percent with minutely plot
by_day_plot_2 <- plot_day_xaxis(
  ggplot(data_by_day, aes(x=time_of_day/60)) +
  scale_y_continuous(limits=c(0,15),breaks = seq(0, 100, 5)) +
  geom_point(aes(y = unviewed_percent*100), colour='#ff808080', size = 1) +
  geom_line(aes(y = roll_unviewed_percent*100),colour='red',size=1)+
  ylab("percent")
)

# view rates and result rates
by_day_plot_3 <- plot_day_xaxis(ggplot() +
  geom_point(data = data_by_day, aes(x=time_of_day/60, y = count/per_hour_scaling), colour='#d080d0', size = 1)+
  geom_point(data=viewed_data_by_day, aes(x=time_of_day_viewed/60, y = count/per_hour_scaling),color='#8080ff80',size=1)+
  geom_line(data = data_by_day, aes(x=time_of_day/60, y = roll_count/per_hour_scaling),colour='magenta',size=1)+
  geom_line(data=viewed_data_by_day, aes(x=time_of_day_viewed/60, y = roll_count/per_hour_scaling),colour='blue',size=1)+
  scale_y_continuous(limits=c(0,260),breaks = seq(0, 250, 50)) +
  ylab("results/hour")
)

# Clean percent viewed
by_day_plot_4 <- plot_day_xaxis(
  ggplot(data_by_day, aes(x=time_of_day/60)) +
    scale_y_continuous(limits=c(2.5,12.5),breaks = seq(0, 100, 5)) +
    geom_line(aes(y = roll_unviewed_percent*100),colour='red',size=1)+
    ylab("percent")
)

# TTV box plot
by_day_plot_5 <- plot_day_xaxis(
  ggplot(data_by_hour_of_day, aes(x=time_group)) +
  geom_boxplot(
    aes(
      x = time_group+0.5, 
      lower = lower_quartile_ttv,
      upper = upper_quartile_ttv,
      middle = median_ttv,
      ymin = 0,
      ymax = upper_quartile_ttv+1.5*(upper_quartile_ttv-lower_quartile_ttv)
    ),
    stat="identity", fill="#c0c0c0", size=0.3) +
    scale_y_continuous(limits=c(0,1750),breaks = seq(0, 1500, 500)) +
  ylab("minutes"))

# Clean viewed versus available
by_day_plot_6 <- plot_day_xaxis(ggplot() +
    geom_line(data = data_by_day, aes(x=time_of_day/60, y = roll_count/per_hour_scaling),colour='magenta',size=1)+
    geom_line(data=viewed_data_by_day, aes(x=time_of_day_viewed/60, y = roll_count/per_hour_scaling),colour='blue',size=1)+
    scale_y_continuous(limits=c(0,260),breaks = seq(0, 250, 50)) +
    ylab("results/hour")
)

# continuous lines
by_day <- plot_grid(by_day_plot_3+rremove("x.text")+rremove("xlab"),
                    by_day_plot_2+rremove("x.text")+rremove("xlab"),
                    by_day_plot_1,
                    nrow=3,
                    align="v",
                    labels = c("A","B","C"))

# presentation view
by_day2 <- plot_grid(by_day_plot_3+ggtitle("test results and result views")+rremove("x.text")+rremove("xlab"),
                     by_day_plot_2+ggtitle("probability of unviewed test result")+rremove("x.text")+rremove("xlab"),
                     by_day_plot_1+ggtitle("median time to view test result"),
                     nrow=3,
                     align="v"
)

# publication view
by_day_3 <- plot_grid(by_day_plot_6+rremove("x.text")+rremove("xlab"),
                    by_day_plot_4+rremove("x.text")+rremove("xlab"),
                    by_day_plot_5,
                    nrow=3,
                    align="v",
                    labels = c("A","B","C"))

# presentation view
by_day_4 <- plot_grid(by_day_plot_3+ggtitle("test results and result views")+rremove("x.text")+rremove("xlab"),
                     by_day_plot_4+ggtitle("percent of unviewed test result")+rremove("x.text")+rremove("xlab"),
                     by_day_plot_5+ggtitle("median time to view test result"),
                     nrow=3,
                     align="v"
)
########################

by_day_plot_1+ggtitle("median time to view test result over day")
ggsave(paste0(prefix,'MedianTtvByDayContinuous.png'),width=10,height=5,units='in')
by_day_plot_2+ggtitle("proportion of unviewed test results per minute over day")
ggsave(paste0(prefix,'UnviewedRatesByDay.png'),width=10,height=5,units='in')
by_day_plot_3+ggtitle("test results and result views over day")
ggsave(paste0(prefix,'ResultAndViewRatesByDay.png'),width=10,height=5,units='in')
by_day_plot_4+ggtitle("proportion of unviewed test results per minute over day")
ggsave(paste0(prefix,'UnviewedRatesByDayClean.png'),width=10,height=5,units='in')
by_day_plot_5+ggtitle("median time to view test result over day")
ggsave(paste0(prefix,'ResultAndViewRatesByDay.png'),width=10,height=5,units='in')
by_day;
save_plot(paste0(prefix,"ByDayPub2.png"), by_day,base_height = 10,base_width = 10)
by_day2;
save_plot(paste0(prefix,"ByDayPresent2.png"), by_day2,base_height = 10,base_width = 10)
by_day_3;
save_plot(paste0(prefix,"ByDayPub.png"), by_day_3,base_height = 10,base_width = 10)
by_day_4;
save_plot(paste0(prefix,"ByDayPresent.png"), by_day_3,base_height = 10,base_width = 10)

####################
# TODO:

ttvByCountTests <- ggplot(data_by_day)+
  geom_point(aes(x=count, y=median_ttv), colour="#b0b0b0",size=1)+
  # geom_path(aes(x=roll_count, y=roll_median_ttv),size=1)+
  geom_smooth(aes(x=count, y=median_ttv), method = "lm", se = FALSE,colour="black")+
  xlab("reports available/min")+
  ylab("minutes to view")
ttvByCountTests+ggtitle("Time to view result by numbers of reports published per minute")
ggsave(paste0(prefix,'TtvByResultAvailability.png'),width=10,height=5,units='in')

unviewedByCountTests <- ggplot(data_by_day)+
  geom_point(aes(x=count, y=unviewed_count), colour="#ffb0b0",size=1)+
  geom_smooth(aes(x=count, y=unviewed_count), method = "lm", se = FALSE,colour="red",size=1)+
  xlab("reports available/min")+
  ylab("unviewed reports/min")
unviewedByCountTests+ggtitle("Unviewed reports by numbers of reports published per minute")
ggsave(paste0(prefix,'UnviewedByResultAvailability.png'),width=10,height=5,units='in')


# syncToDrive()
