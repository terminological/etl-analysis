# By month
# Weekly time to view time series analysis
# install.packages("pwr")

library(ggplot2)
library(dplyr)
library(reshape2)
library(ggrepel)
library(cowplot)
library(ggpubr)
library(scales)
library(pwr)

source_directory <- getSrcDirectory(function(dummy) {dummy});
source(paste(source_directory,'/standardAxes.R',sep=""));
source(paste(source_directory,'/utils.R',sep=""));
createDirectory();

##############
# DATA

historical_data_by_day <- data_baseline %>% 
  mutate (time_group = as.Date(date)) %>%
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
  )
width <- 21 # days

historical_data_by_day['roll_count'] <- stats::filter(historical_data_by_day['count'],filter=rep(1/width,width),method='convolution',sides=2,circular=FALSE)
historical_data_by_day['roll_unviewed_count'] <- stats::filter(historical_data_by_day['unviewed_count'],filter=rep(1/width,width),method='convolution',sides=2,circular=FALSE)
historical_data_by_day['roll_median_ttv'] <- stats::filter(historical_data_by_day['median_ttv'],filter=rep(1/width,width),method='convolution',sides=2,circular=FALSE)
historical_data_by_day['roll_lower_quartile_ttv'] <- stats::filter(historical_data_by_day['lower_quartile_ttv'],filter=rep(1/width,width),method='convolution',sides=2,circular=FALSE)
historical_data_by_day['roll_upper_quartile_ttv'] <- stats::filter(historical_data_by_day['upper_quartile_ttv'],filter=rep(1/width,width),method='convolution',sides=2,circular=FALSE)
historical_data_by_day['roll_views'] <- stats::filter(historical_data_by_day['views'],filter=rep(1/width,width),method='convolution',sides=2,circular=FALSE)
historical_data_by_day <- mutate(historical_data_by_day, roll_unviewed_percent = roll_unviewed_count/roll_count);
historical_data_by_day <- mutate(historical_data_by_day, roll_mean_views = roll_views/roll_count);

viewed_historical_data_by_day <- data_baseline %>% filter(date != as.POSIXct("2015-09-06")) %>%
  filter(viewed) %>%
  mutate (time_group = as.Date(date)) %>%
  filter(time_group != as.Date("2015-09-06")) %>%
  group_by(time_group) %>%
  summarize(
    count=n(),
    median_age=fivenum(minutes_to_view, na.rm = TRUE)[3]
  )
viewed_historical_data_by_day['roll_count'] <- stats::filter(viewed_historical_data_by_day['count'],filter=rep(1/width,width),method='convolution',sides=2,circular=TRUE)
viewed_historical_data_by_day['roll_median_age'] <- stats::filter(viewed_historical_data_by_day['median_age'],filter=rep(1/width,width),method='convolution',sides=2,circular=TRUE)

###############
# PLOTS

# Median time to view test result over week
historical_plot_ttv <- 
  ggplot(
    historical_data_by_day, 
    aes(x=time_group)) +
    xlab("date")+
  scale_x_date(date_breaks="3 months",date_labels="%b %Y")+
  theme(axis.text.x = element_text(angle=45,size=9,hjust=1,vjust=1)) +
    ylab("minutes")+
    scale_y_continuous(limits=c(0,400), breaks = seq(0, 600, 60))+
    geom_point(aes(y = median_ttv),color='#808080',size=1)+
    geom_line(aes(y = roll_median_ttv, group =1), color="black",size = 1)+
  geom_point(aes(y = lower_quartile_ttv),color='#b0b0ff',size=1)+
  geom_line(aes(y = roll_lower_quartile_ttv, group =1), color="#8080ff",size = 1)+
geom_point(aes(y = upper_quartile_ttv),color='#8080ff',size=1)+
  geom_line(aes(y = roll_upper_quartile_ttv, group =1), color="blue",size = 1)

historical_plot_ttv+ggtitle("median time to view test result over time")
ggsave('medianTtvHistorical.png',width=10,height=5,units='in')

# Percent unviewed view test result over week
historical_plot_unviewed <- 
  ggplot(historical_data_by_day, aes(x=time_group)) +
  xlab("date")+
  scale_x_date(date_breaks="3 months",date_labels="%b %Y")+
  theme(axis.text.x = element_text(angle=45,size=9,hjust=1,vjust=1)) +
  ylab("percent")+
    scale_y_continuous(limits=c(0,15),breaks = seq(0, 100, 5)) +
    geom_point(aes(y = unviewed_percent*100),color='#ff8080',size=1)+
    geom_line(aes(y = roll_unviewed_percent*100, group=1), color="red",size = 1)

historical_plot_unviewed+ggtitle("proportion of unviewed test results per day over time")
ggsave('unreviewedRatesHistorical.png',width=10,height=5,units='in')

# totals viewed result over week
historical_plot_counts <- 
  ggplot() +
  xlab("date")+
  scale_x_date(date_breaks="3 months",date_labels="%b %Y")+
  theme(axis.text.x = element_text(angle=45,size=9,hjust=1,vjust=1)) +
    ylab("results/day")+
    # scale_y_continuous(breaks = seq(0, 100, 5)) +
    geom_point(data=historical_data_by_day, aes(x=time_group,y = count),color='#ffb0ff',size=1)+
    geom_point(data=viewed_historical_data_by_day, aes(x=time_group,y = count),color='#c0c0ff',size=1)+
    geom_line(data=historical_data_by_day, aes(x=time_group,y = roll_count,group=1), color="magenta",size = 1)+
    geom_line(data=viewed_historical_data_by_day, aes(x=time_group,y = roll_count,group=2), color="blue",size = 1)

historical_plot_counts+ggtitle("test results and result views over time")
ggsave('resultAndViewRatesHistorical.png',width=10,height=5,units='in')

# Median time to view test result over week
historical_plot_viewers <- 
  ggplot() +
  xlab("date")+
  scale_x_date(date_breaks="3 months",date_labels="%b %Y")+
  theme(axis.text.x = element_text(angle=45,size=9,hjust=1,vjust=1)) +
  ylab("no. viewers")+
  # scale_y_continuous(breaks = seq(0, 100, 5)) +
  geom_point(data=historical_data_by_day, aes(x=time_group,y = mean_views),color='#ffd0b0',size=1)+
  geom_line(data=historical_data_by_day, aes(x=time_group,y = roll_mean_views), color="orange",size = 1)

historical_plot_viewers+ggtitle("average clinicians viewing over time")
ggsave('viewsHistorical.png',width=10,height=5,units='in')


# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#required-r-package
# https://cran.r-project.org/web/packages/cowplot/vignettes/introduction.html



historical_variance_ttv <- ggplot(historical_data_by_day, aes(x=median_ttv))+geom_histogram(bins=20,fill='#808080',colour='black')+xlab("median time to view")
tmp<-sd(historical_data_by_day$median_ttv)
power.t.test(n=91,power = 0.8, delta = NULL, sd=tmp, alternative = "one.sided",type="two.sample")


historical_variance_unviewed <- ggplot(historical_data_by_day, aes(x=unviewed_percent*100))+geom_histogram(bins=20,fill='#ff8080',colour='black')+xlab("propotion tests unviewed")
tmp <- sd(historical_data_by_day$unviewed_percent)

historical_variance_viewers <- ggplot(historical_data_by_day, aes(x=mean_views))+geom_histogram(bins=20,fill='#ffd0b0',colour='black')+xlab("mean number clinicians viewing")
tmp <- sd(historical_data_by_day$mean_views)

historical <- plot_grid(
    historical_plot_ttv+rremove("x.text")+rremove("xlab"),
    historical_variance_ttv+rremove("ylab")+rremove("xlab"),
    historical_plot_unviewed+rremove("x.text")+rremove("xlab"),
    historical_variance_unviewed+rremove("ylab")+rremove("xlab"),
    historical_plot_viewers,
    historical_variance_viewers+rremove("ylab")+rremove("xlab"),
    nrow=3,
    rel_widths=c(2,1),
    align="hv",
    labels = c("A1","A2","B1","B2","C1","C2"))
historical;
save_plot("historicalPub.png", historical,
          base_height = 7.5,
          base_width = 7.5
)

##############
# regressions

tmpOrigin <- median(historical_data_by_day$time_group)
historical_data_by_day <- historical_data_by_day %>% mutate(days =as.numeric(difftime(time_group,tmpOrigin,units="day")))
lm_ttv <- lm(median_ttv~days,historical_data_by_day)
lm_uq_ttv <- lm(upper_quartile_ttv~days,historical_data_by_day)
lm_lq_ttv <- lm(lower_quartile_ttv~days,historical_data_by_day)
lm_mean_views <- lm(mean_views~days,historical_data_by_day)
lm_unviewed <- lm(unviewed_percent~days,historical_data_by_day)

stargazer(
  lm_ttv,
  lm_uq_ttv,
  lm_uq_ttv,
  lm_mean_views,
  lm_unviewed,
  type="html" ,out="table1.html")

# pwr.t2n.test(n1=365*3,n2=91,power=0.8)$h*sigma(tmp)/coef(tmp)["(Intercept)"]
# tmp_d=coef(tmp)["(Intercept)"]*0.10/sigma(tmp)
# power.t.test(power = .80, delta = coef(tmp)["(Intercept)"]*0.05, sd=sigma(tmp), alternative = "one.sided",type="two.sample")
pwr.t2n.test(n1=365*3,n2=91,power=0.8,alternative="greater")$d*sigma(tmp)/coef(tmp)["(Intercept)"]
power.t.test(power = .80, sd=sigma(tmp), n=91, alternative = "one.sided",type="two.sample")$delta/coef(tmp)["(Intercept)"]
effect_diff <- 0.05
pwr.t2n.test(n1=365*3, n2=NULL,power=0.8,alternative="greater", d=coef(tmp)["(Intercept)"]*effect_diff/sigma(tmp))$n2
power.t.test(power = .80, sd=sigma(tmp), alternative = "one.sided",type="two.sample", delta=coef(tmp)["(Intercept)"]*effect_diff)$n

# sd(historical_data_by_day$median_ttv)
# sd(historical_data_by_day$unviewed_percent)
# sd(historical_data_by_day$mean_views)
# library(pwr)
# power.t.test(power = .80, delta = 10, sd=tmp, alternative = "one.sided",type="two.sided")
# power.t.test(power = .80, delta = 10, sd=tmp, alternative = "one.sided",type="two.sample")
# tmp<-sd(historical_data_by_day$median_ttv)
# power.t.test(power = .80, delta = 10, sd=tmp, alternative = "one.sided",type="two.sample")
# power.t.test(n=91,power = NULL, delta = 10, sd=tmp, alternative = "one.sided",type="two.sample")
# power.t.test(n=91,power = NULL, delta = 20, sd=tmp, alternative = "one.sided",type="two.sample")
# power.t.test(n=91,power = NULL, delta = 5, sd=tmp, alternative = "one.sided",type="two.sample")
# power.t.test(n=91,power = 0.8, delta = NULL, sd=tmp, alternative = "one.sided",type="two.sample")
# 88*0.05
# power.t.test(n=NULL,power = 0.8, delta = 4.4, sd=tmp, alternative = "one.sided",type="two.sample")
# power.t.test(n=91,power = 0.8, delta = NULL, sd=tmp, alternative = "one.sided",type="two.sample")
# 5.61/88*100
# tmp <- sd(historical_data_by_day$unviewed_percent)
# power.t.test(n=91,power = 0.8, delta = NULL, sd=tmp, alternative = "one.sided",type="two.sample")
# mean(historical_data_by_day$unviewed_percent)
# 0.006975439/0.06129206
# tmp <- sd(historical_data_by_day$mean_views)
# power.t.test(n=91,power = 0.8, delta = NULL, sd=tmp, alternative = "one.sided",type="two.sample")
# 0.07986374/2.61



syncToDrive()

