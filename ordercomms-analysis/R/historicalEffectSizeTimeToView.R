library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(ggrepel)
library(cowplot)
library(ggpubr)
library(scales)

source_directory <- getSrcDirectory(function(dummy) {dummy});
if (source_directory == "") source_directory="/home/robchallen/Git/working-drafts/src/main/r"
source(paste(source_directory,'/standardAxes.R',sep=""));
source(paste(source_directory,'/utils.R',sep=""));
createDirectory();

# load("~/R/timeToViewData")
# Use the coin package in R which also requires the modeltools and mvtnorm packages. Here is an example

data_2012 <- data %>% filter(
  investigation_abnormal==1
  & discipline_name=='Chem/Haem'
  & as.Date(date) < as.Date("2016-09-01")
  & as.Date(date) >= as.Date("2012-10-01")
);

timespans <- data.frame(
  xmin=c(as.Date("2013-02-01"),as.Date("2014-11-01")),
  #xmax=c(as.Date("2013-04-01"),as.Date("2015-01-01")),
  xmax=c(as.Date("2014-02-01"),as.Date("2015-11-01")),
  label=c("pre","post")
)

pre_timepoints <- seq(timespans$xmin[1], timespans$xmax[1], by = "months")
post_timepoints <- seq(timespans$xmin[2], timespans$xmax[2], by = "months")

tmp_out <- data.frame()
k <- 0
for (i in c(2:length(pre_timepoints))) {
  pre_start <- pre_timepoints[i-1]
  pre_end <- pre_timepoints[i]
  # pre_start <- as.Date("2013-02-01")
  tmp_pre <- data_2012 %>% filter(
    as.Date(date) >= pre_start
    & as.Date(date) < pre_end
  ) 
  for (j in c(2:length(pre_timepoints))) {
    post_start <- post_timepoints[j-1]
    post_end <- post_timepoints[j]
    print(paste0(k,") ",pre_start,"~",pre_end," : ",post_start,"~",post_end))
    # post_start <- as.Date("2015-02-01")
    tmp_post <- data_2012 %>% filter(
      as.Date(date) >= post_start
      & as.Date(date) < post_end
    ) 
    
    tmp_df <- data.frame(
      bind_rows(tmp_pre,tmp_post)
    ) %>% 
    filter(not(is.na(minutes_to_view))) %>%
    mutate(
      timegroup = as.factor(ifelse(as.Date(date) < post_start, "pre", "post"))
    ) %>% select(minutes_to_view, timegroup);
    
    tmp_wt <- wilcox.test(
      minutes_to_view ~ timegroup,
      data=tmp_df ,
      alternative="t", conf.int = TRUE) 
    
    tmp_out <- tmp_out %>% bind_rows(
      data.frame(
        x1=pre_start,
        x2=pre_end,
        y1=post_start,
        y2=post_end,
        p=tmp_wt$p.value,
        W=tmp_wt$statistic["W"],
        estimate=-tmp_wt$estimate,
        min_conf=tmp_wt$conf.int[1],
        max_conf=tmp_wt$conf.int[2]
      )
    )
    k <- k+1
  }
}


h2012_effect_size <- ggplot(tmp_out)+ geom_rect(aes(xmin=x1, xmax=x2,ymin=y1,ymax=y2,fill = estimate),colour = "white") + 
  scale_fill_gradient(high = "black", low = "grey75")+
  coord_fixed(ratio = 1)+
  scale_x_date(labels = date_format("%b %Y"),date_breaks = "1 month")+
  scale_y_date(labels = date_format("%b %Y"),date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

h2012_effect_size+ggtitle("Effect sizes of by month cross comparison pre and post implementation")
ggsave('effectSizeH2012.png',width=5,height=5,units='in')
