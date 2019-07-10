
library(ggplot2)
library(dplyr)
library(reshape2)

source_directory <- getSrcDirectory(function(dummy) {dummy});
if (source_directory == "") source_directory="/home/robchallen/Git/working-drafts/src/main/r"
source(paste(source_directory,'/standardAxes.R',sep=""));
source(paste(source_directory,'/utils.R',sep=""));
createDirectory();

data_by_calendar_month <- data_baseline %>%
  mutate(time_group = format(date,"%m")) %>%
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

ttv_box_by_calendar_month <- plot_year_xaxis_2(
  ggplot(data_by_calendar_month, aes(x=time_group)) +
  geom_boxplot(
    aes(
      x = time_group, 
      lower = lower_quartile_ttv,
      upper = upper_quartile_ttv,
      middle = median_ttv,
      ymin = 0,
      ymax = upper_quartile_ttv+1.5*(upper_quartile_ttv-lower_quartile_ttv)
    ), 
    stat="identity", fill="#c0c0c0", size=0.3) +
    theme(axis.text.x = element_text(angle=90,size=9))+
    scale_y_continuous(limits=c(0,650),breaks=seq(0,600,100))+
  ylab("minutes")
)
ttv_box_by_calendar_month+ggtitle("Median time to view test results by calendar month")
ggsave('ttvByCalendarMonthBoxPlot.png',width=10,height=5,units='in')

unviewed_by_calendar_month <- plot_year_xaxis_2(
  ggplot(data_by_calendar_month, aes(x=time_group)) +
    geom_bar(
      aes(
        x = time_group, 
        y= (1-viewed_percent)*100
      ), stat="identity", fill="#ffc0c0", colour="black", width=0.75, size=0.3) +
    ylab("percent")+
    theme(axis.text.x = element_text(angle=90,size=9))+
    expand_limits(y=c(0,10))
)
unviewed_by_calendar_month+ggtitle("percent of results unviewed by calendar month")
ggsave('unviewedByCalendarMonth.png',width=10,height=5,units='in')


################

data_by_day_of_week <- data_baseline %>%
  mutate(time_group = factor(time_of_week%/%(24*60),
      levels=c('0','1','2','3','4','5','6'),
      labels=c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')
  )) %>%
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

ttv_box_by_day_of_week <- ggplot(data_by_day_of_week, aes(x=time_group)) +
    geom_boxplot(
      aes(
        x = time_group, 
        lower = lower_quartile_ttv,
        upper = upper_quartile_ttv,
        middle = median_ttv,
        ymin = 0,
        ymax = upper_quartile_ttv+1.5*(upper_quartile_ttv-lower_quartile_ttv)
      ), 
      stat="identity", fill="#c0c0c0", size=0.3) +
    ylab("minutes")+
  theme(axis.text.x = element_text(angle=90,size=9))+
  scale_y_continuous(limits=c(0,650),breaks=seq(0,600,100))+
    xlab("day")
ttv_box_by_day_of_week+ggtitle("Median time to view test results by day of week")
ggsave('ttvByDayOfWeekBoxPlot.png',width=10,height=5,units='in')

unviewed_by_day_of_week <- ggplot(data_by_day_of_week, aes(x=time_group)) +
    geom_bar(
      aes(
        x = time_group, 
        y= (1-viewed_percent)*100
      ), stat="identity", fill="#ffc0c0", colour="black", width=0.75, size=0.3) +
    ylab("percent")+
  theme(axis.text.x = element_text(angle=90,size=9))+
    xlab("day")+
    expand_limits(y=c(0,10))

unviewed_by_day_of_week+ggtitle("Percent of results unviewed by day of week")
ggsave('unviewedByDayOfWeek.png',width=10,height=5,units='in')

################

data_by_hour_of_day <- data_baseline %>%
  mutate(time_group = format(date,"%H:00")) %>%
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

ttv_box_by_hour_of_day <- ggplot(data_by_hour_of_day, aes(x=time_group)) +
  geom_boxplot(
    aes(
      x = time_group, 
      lower = lower_quartile_ttv,
      upper = upper_quartile_ttv,
      middle = median_ttv,
      ymin = 0,
      ymax = upper_quartile_ttv+1.5*(upper_quartile_ttv-lower_quartile_ttv)
    ), 
    stat="identity", fill="#c0c0c0", size=0.3) +
  ylab("minutes")+
  theme(axis.text.x = element_text(angle=90,size=9))+
  xlab("hour")

ttv_box_by_hour_of_day+ggtitle("Median time to view test results by hour of day-+")
ggsave('ttvByHourOfDayBoxPlot.png',width=10,height=5,units='in')

unviewed_by_hour_of_day <- ggplot(data_by_hour_of_day, aes(x=time_group)) +
  geom_bar(
    aes(
      x = time_group, 
      y= (1-viewed_percent)*100
    ), stat="identity", fill="#ffc0c0", colour="black", width=0.75, size=0.3) +
  ylab("percent")+
  theme(axis.text.x = element_text(angle=90,size=9))+
  xlab("hour")+
  expand_limits(y=c(0,10))

unviewed_by_hour_of_day+ggtitle("percent of results unviewed by hour of day")
ggsave('unviewedByHourOfDay.png',width=10,height=5,units='in')

by_time <- plot_grid(ttv_box_by_calendar_month+rremove("x.text")+rremove("xlab"),
                     ttv_box_by_day_of_week+rremove("x.text")+rremove("ylab")+rremove("xlab"),
                     ttv_box_by_hour_of_day+rremove("x.text")+rremove("ylab")+rremove("xlab"),
                     unviewed_by_calendar_month,
                     unviewed_by_day_of_week+rremove("ylab"),
                     unviewed_by_hour_of_day+rremove("ylab"),
                     nrow=2,
                     align="v",
                     labels = c("A1","B1","C1","A2","B2","C2"));
# by_time;
save_plot("byTime.png", by_time,
          base_height = 5,
          base_width = 10
)

by_time_without_day <- plot_grid(ttv_box_by_calendar_month+rremove("x.text")+rremove("xlab"),
                     ttv_box_by_day_of_week+rremove("x.text")+rremove("y.text")+rremove("ylab")+rremove("xlab"),
                     unviewed_by_calendar_month,
                     unviewed_by_day_of_week+rremove("ylab")+rremove("y.text"),
                     nrow=2,
                     align="v",
                     labels = c("A1","B1","C1","A2","B2","C2"));
by_time_without_day
save_plot("byTimeWithoutDay.png", by_time_without_day,
          base_height = 10,
          base_width = 10
)