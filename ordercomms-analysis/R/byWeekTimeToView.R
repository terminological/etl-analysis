# Weekly time to view time series analysis

library(ggplot2)
library(dplyr)
library(reshape2)
library(ggrepel)
library(cowplot)
library(ggpubr)

source_directory <- getSrcDirectory(function(dummy) {dummy});
if (source_directory == "") source_directory="/home/robchallen/Git/working-drafts/src/main/r"
source(paste(source_directory,'/standardAxes.R',sep=""));
source(paste(source_directory,'/utils.R',sep=""));
createDirectory();

##############
# DATA

data_by_week <- data_baseline %>%
  mutate (time_group = time_of_week) %>%
  group_by(time_group) %>%
  dplyr::summarize(
    count=n(),
    unviewed_count=sum(total_views == 0),
    unviewed_percent=sum(total_views == 0)/n(),
    median_ttv=fivenum(minutes_to_view, na.rm = TRUE)[3]
  )
data_by_week['roll_count'] <- stats::filter(data_by_week['count'],filter=rep(1/121,121),method='convolution',sides=2,circular=TRUE)
data_by_week['roll_unviewed_count'] <- stats::filter(data_by_week['unviewed_count'],filter=rep(1/121,121),method='convolution',sides=2,circular=TRUE)
data_by_week['roll_median_ttv'] <- stats::filter(data_by_week['median_ttv'],filter=rep(1/121,121),method='convolution',sides=2,circular=TRUE)
data_by_week <- mutate(data_by_week, roll_unviewed_percent = roll_unviewed_count/roll_count);

viewed_data_by_week <- data_baseline %>%
  filter (total_views > 0) %>%
  mutate (time_group = time_of_week_viewed) %>%
  group_by(time_group) %>%
  dplyr::summarize(
    count=n(),
    median_age=fivenum(minutes_to_view, na.rm = TRUE)[3]
  )
viewed_data_by_week['roll_count'] <- stats::filter(viewed_data_by_week['count'],filter=rep(1/121,121),method='convolution',sides=2,circular=TRUE)
viewed_data_by_week['roll_median_age'] <- stats::filter(viewed_data_by_week['median_age'],filter=rep(1/121,121),method='convolution',sides=2,circular=TRUE)

###############
# PLOTS

# Median time to view test result over week
by_week_plot_1 <- plot_weekday_xaxis(
  ggplot(data_by_week, aes(x=time_group/60)) +
    ylab("minutes")+
    scale_y_continuous(limits=c(0,240),breaks = seq(0, 2400, 60)) +
    geom_point(aes(y = median_ttv),color='#808080',size=0.5)+
    geom_line(aes(y = roll_median_ttv), color="black",size = 1)
);
by_week_plot_1+ggtitle("median time to view test result over week")
ggsave('medianTtvByweek.png',width=10,height=5,units='in')

# Median time to view test result over week
by_week_plot_2 <- plot_weekday_xaxis(
  ggplot(data_by_week, aes(x=time_group/60)) +
    ylab("percent")+
    scale_y_continuous(limits=c(0,15),breaks = seq(0, 100, 5)) +
    geom_point(aes(y = unviewed_percent*100),color='#ffc0c0',size=0.5)+
    geom_line(aes(y = roll_unviewed_percent*100), color="red",size = 1)
);
by_week_plot_2+ggtitle("proportion of unviewed test results per minute over week")
ggsave('unviewedRatesByWeek.png',width=10,height=5,units='in')

# Median time to view test result over week
by_week_plot_3 <- plot_weekday_xaxis(
  ggplot() +
    ylab("results/min")+
    # scale_y_continuous(breaks = seq(0, 100, 5)) +
    geom_point(data=data_by_week, aes(x=time_group/60,y = count),color='#ffb0ff',size=0.5)+
    geom_point(data=viewed_data_by_week, aes(x=time_group/60,y = count),color='#c0c0ff',size=0.5)+
    geom_line(data=data_by_week, aes(x=time_group/60,y = roll_count), color="magenta",size = 1)+
    geom_line(data=viewed_data_by_week, aes(x=time_group/60,y = roll_count), color="blue",size = 1)
);
by_week_plot_3+ggtitle("test results and result views over week")
ggsave('resultAndViewRatesByWeek.png',width=10,height=5,units='in')

# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#required-r-package
# https://cran.r-project.org/web/packages/cowplot/vignettes/introduction.html

# print sizes - width between 8.3 cms to 17.35 cms - BMJ
# nature: 89mm (single) 183mm (double) and max depth 247mm

by_week <- plot_grid(by_week_plot_3+rremove("x.text")+rremove("xlab"),
          by_week_plot_2+rremove("x.text")+rremove("xlab"),
          by_week_plot_1,
          nrow=3,
          align="v",
          labels = c("A","B","C"));
by_week;
save_plot("byWeek.svg", by_week,
          base_aspect_ratio=1.4,
          base_width = 89/25.4
)

response_curve_data <- inner_join(data_by_week,viewed_data_by_week, by=c("time_group"="time_group"), suffix = c(".tests", ".views"));
response_curve_data <- mutate(response_curve_data,day_of_week=ifelse((time_group%/%(24*60)) >= 5,"weekend","weekday"));

response_curve <- ggplot(response_curve_data, aes(x=roll_count.tests, y=roll_count.views))+
  geom_point(aes(x=count.tests, y=count.views), colour="#b0b0b0",size=0.5)+
  geom_path(aes(x=roll_count.tests, y=roll_count.views,colour=day_of_week),size=1)+
  xlab("reports available/min")+
  ylab("reports viewed/min")+
  theme(legend.title=element_blank())
response_curve+ggtitle("Phase space for results viewed by results available")
ggsave('resultViewsByResultAvailability.png',width=10,height=5,units='in')

# syncToDrive()