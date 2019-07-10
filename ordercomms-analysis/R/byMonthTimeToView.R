# By month
# Weekly time to view time series analysis

library(ggplot2)
library(dplyr)
library(reshape2)
library(ggrepel)
library(cowplot)
library(ggpubr)

source_directory <- getSrcDirectory(function(dummy) {dummy});
source(paste(source_directory,'/standardAxes.R',sep=""));
source(paste(source_directory,'/utils.R',sep=""));
createDirectory();

##############
# DATA

data_by_year <- data_baseline %>%
  mutate (time_group = format(date, "%m-%d")) %>%
  group_by(time_group) %>%
  summarize(
    count=n(),
    unviewed_count=sum(total_views == 0),
    unviewed_percent=sum(total_views == 0)/n(),
    median_ttv=fivenum(minutes_to_view, na.rm = TRUE)[3]
  )
data_by_year['roll_count'] <- stats::filter(data_by_year['count'],filter=rep(1/11,11),method='convolution',sides=2,circular=TRUE)
data_by_year['roll_unviewed_count'] <- stats::filter(data_by_year['unviewed_count'],filter=rep(1/11,11),method='convolution',sides=2,circular=TRUE)
data_by_year['roll_median_ttv'] <- stats::filter(data_by_year['median_ttv'],filter=rep(1/11,11),method='convolution',sides=2,circular=TRUE)
data_by_year <- mutate(data_by_year, roll_unviewed_percent = roll_unviewed_count/roll_count);

viewed_data_by_year <- viewed_data_baseline %>%
  mutate (time_group = format(date, "%m-%d")) %>%
  group_by(time_group) %>%
  summarize(
    count=n(),
    median_age=fivenum(minutes_to_view, na.rm = TRUE)[3]
  )
viewed_data_by_year['roll_count'] <- stats::filter(viewed_data_by_year['count'],filter=rep(1/11,11),method='convolution',sides=2,circular=TRUE)
viewed_data_by_year['roll_median_age'] <- stats::filter(viewed_data_by_year['median_age'],filter=rep(1/11,11),method='convolution',sides=2,circular=TRUE)

###############
# PLOTS

# Median time to view test result over week
by_year_plot_1 <- plot_year_xaxis(
  ggplot(data_by_year, aes(x=time_group)) +
    ylab("minutes")+
    scale_y_continuous(limits=c(0,240),breaks = seq(0, 2400, 60)) +
    geom_point(aes(y = median_ttv),color='#808080',size=0.5)+
    geom_line(aes(y = roll_median_ttv, group =1), color="black",size = 1)
);
by_year_plot_1+ggtitle("median time to view test result over year")
ggsave('medianTtvByYear.png',width=10,height=5,units='in')

# Median time to view test result over week
by_year_plot_2 <- plot_year_xaxis(
  ggplot(data_by_year, aes(x=time_group)) +
    ylab("percent")+
    scale_y_continuous(limits=c(0,15),breaks = seq(0, 100, 5)) +
    geom_point(aes(y = unviewed_percent*100),color='#ffc0c0',size=0.5)+
    geom_line(aes(y = roll_unviewed_percent*100, group=1), color="red",size = 1)
);
by_year_plot_2+ggtitle("proportion of unviewed test results per day over year")
ggsave('unreviewedRatesByYear.png',width=10,height=5,units='in')

# Median time to view test result over week
by_year_plot_3 <- plot_year_xaxis(
  ggplot() +
    ylab("results/day")+
    # scale_y_continuous(breaks = seq(0, 100, 5)) +
    geom_point(data=data_by_year, aes(x=time_group,y = count),color='#ffb0ff',size=0.5)+
    geom_point(data=viewed_data_by_year, aes(x=time_group,y = count),color='#c0c0ff',size=0.5)+
    geom_line(data=data_by_year, aes(x=time_group,y = roll_count,group=1), color="magenta",size = 1)+
    geom_line(data=viewed_data_by_year, aes(x=time_group,y = roll_count,group=2), color="blue",size = 1)
);
by_year_plot_3+ggtitle("test results and result views over year")
ggsave('resultAndViewRatesByYear.png',width=10,height=5,units='in')

# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#required-r-package
# https://cran.r-project.org/web/packages/cowplot/vignettes/introduction.html

by_year <- plot_grid(by_year_plot_3+rremove("x.text")+rremove("xlab"),
                     by_year_plot_2+rremove("x.text")+rremove("xlab"),
                     by_year_plot_1,
                     nrow=3,
                     align="v",
                     labels = c("A","B","C"))
by_year;
save_plot("byMonth.png", by_year,
          base_height = 5,
          base_width = 10
)

syncToDrive()

