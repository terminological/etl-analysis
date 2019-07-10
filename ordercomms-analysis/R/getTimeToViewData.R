library(DBI)
library(odbc)
library(datasets)
library(dplyr)
library(reshape2)

source_directory <- getSrcDirectory(function(dummy) {dummy});
if (source_directory == "") source_directory="/home/robchallen/Git/working-drafts/src/main/r"
source(paste(source_directory,'/standardAxes.R',sep=""));
source(paste(source_directory,'/utils.R',sep=""));
createDirectory();

pwd <- readline(prompt="Enter DB: ");
con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 13 for SQL Server",
                 Server = "10.174.129.118",
                 Database = "RobsDatabase",
                 UID = "RobertSQL",
                 PWD = pwd,
                 Port = 1433);

dbListTables(con);

data <- dbReadTable(con, "aggTimeToView", as.is=c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE));

data <- data %>%
	mutate(
			minutes_to_view = ifelse(minutes_to_view > 31*24*60, NA, minutes_to_view),
			view_type = ifelse(is.na(minutes_to_view), NA, view_type),
			total_views = ifelse(is.na(minutes_to_view), 0, as.integer(total_views)),
			viewed = !is.na(minutes_to_view)
)

data <- data %>%
  mutate(month_cat=format(date, "%Y-%m")) %>%
  mutate(time_of_week=as.numeric(difftime(date,cut(date,"week"),unit="mins"))%%(24*7*60)) %>%
  mutate(time_of_day=time_of_week %% (24*60)) %>%
  mutate(day_time_cat = cut(
				  (time_of_day+16*60)%%(24*60),
					breaks=c(0,4*60,8*60,10*60,25*60),
					labels=c("Morning","Day","Afternoon","Night"),
					ordered_result=FALSE,
					include.lowest=TRUE
				  )) %>%
  filter(as.Date(date) <= as.Date("2017-08-31")) %>%
  filter(ward_name != "Other") %>%
  collect();

data <- data %>% 
  mutate(time_of_week_viewed=as.numeric(difftime(first_viewed_date,cut(first_viewed_date,"week"),unit="mins"))%%(24*7*60)) %>%
  mutate(time_of_day_viewed=time_of_week_viewed %% (24*60)) %>%
  mutate(end_month_cat=format(first_viewed_date, "%Y-%m")) %>%
  collect();

count(data)
median(data$minutes_to_view, na.rm = TRUE)
mean(data$minutes_to_view, na.rm = TRUE)
# percent tests viewed
100*(1-sum(ifelse(data$viewed,1,0))/count(data))
# percent abnormal tests viewed
100*(1-sum(ifelse(data$viewed & data$investigation_abnormal,1,0))/sum(ifelse(data$investigation_abnormal,1,0)))
# percent normal tests viewed 
100*(1-sum(ifelse(data$viewed & !(data$investigation_abnormal),1,0))/sum(ifelse(!data$investigation_abnormal,1,0)))


data_baseline <- data %>% filter(
  investigation_abnormal==1 
  & discipline_name=='Chem/Haem'
  & as.Date(date) < as.Date("2017-09-01")
  & as.Date(date) >= as.Date("2014-09-01")
);

count(data_baseline)
fivenum(data_baseline$minutes_to_view, na.rm = TRUE)
mean(data_baseline$minutes_to_view, na.rm = TRUE)
# percent tests viewed
100*(1-sum(ifelse(data_baseline$viewed,1,0))/count(data_baseline))
# percent abnormal tests viewed
100*(1-sum(ifelse(data_baseline$viewed & data_baseline$investigation_abnormal,1,0))/sum(ifelse(data_baseline$investigation_abnormal,1,0)))
# percent normal tests viewed 
100*(1-sum(ifelse(data_baseline$viewed & !(data_baseline$investigation_abnormal),1,0))/sum(ifelse(!data_baseline$investigation_abnormal,1,0)))

count(data_baseline)/(12*3)
mean(data_baseline$total_views)
sd(data_baseline$total_views)

save.image(file="~/R/timeToViewData");
# cor(data$patient_age, data$minutes_to_view, use="complete.obs")
# cor(data$date, data$minutes_to_view, use="complete.obs")
# https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
# model.lm <- lm(minutes_to_view ~ month_cat, data = data)

###########################################
# characterise data set

data_count <- nrow(data);
viewed_total <- nrow(data %>% filter(viewed))

# mean clinician views


# ttv distribution by hour
data_by_hour_to_view <- data %>%
  filter(minutes_to_view > 0) %>%
  group_by(minutes_to_view) %>%
  summarize(
    count=n()
  ) %>% mutate(
    cumulative=cumsum(count),
    recovered_percent=cumulative / data_count
  )

ttv_distribution_8_hours <- ggplot(data = data_by_hour_to_view %>% filter(minutes_to_view < 60*8),
       aes(x=minutes_to_view,y=count))+
  geom_hline(yintercept = viewed_total/data_count*30000, colour="#808080") +
  geom_bar(stat="identity",width=1, fill="#c0c0ff")+
  geom_line(aes(y=recovered_percent*30000))+
  xlab("minutes since result")+
  scale_x_continuous(breaks=seq(0,60*8,60))+
  ylab("tests viewed per minute")+
  scale_y_continuous(limits=c(0,30000),breaks = seq(0, 30000, 5000),
                     sec.axis=sec_axis(~./300, name="percent viewed")
  )
ttv_distribution_8_hours+ggtitle("Tests viewed in first 8 hours");
ggsave('timeToViewDistributionFirst8Hours.png',width=10,height=5,units='in')
ttv_distribution_8_hours;
ggsave('timeToViewDistributionFirst8HoursNoTitle.png',width=10,height=5,units='in')

ttv_distribution_1_day <- ggplot(data = data_by_hour_to_view %>% filter(minutes_to_view < 60*24),
       aes(x=minutes_to_view,y=count))+
  geom_hline(yintercept = viewed_total/data_count*30000, colour="#ff8080") +
  geom_area(stat="identity",fill="#8080ff")+
  geom_line(stat="identity",width=1, colour="#0000ff")+
  geom_line(aes(y=recovered_percent*30000))+
  xlab("minutes")+
  scale_x_continuous(breaks=seq(0,24*7*60,3*60))+
  ylab("count")+
  scale_y_continuous(limits=c(0,30000),breaks = seq(0, 30000, 5000),
                     sec.axis=sec_axis(~./300, name="percent viewed")
  )
ttv_distribution_1_day+ggtitle("Tests viewed in 24 hours");
ggsave('timeToViewDistribution1days.png',width=10,height=5,units='in')
ttv_distribution_1_day
ggsave('timeToViewDistribution1daysNoTitle.png',width=10,height=5,units='in')

clinician_views_distribution <- ggplot(data, aes(x=total_views)) +
geom_histogram(bins = max(data$total_views),fill="#ffb080",colour="black") +
xlab("no. clinicians")+
scale_x_continuous(breaks=seq(0,50,5))
clinician_views_distribution
ggsave('clinicianViewsDistribution.png',width=10,height=5,units='in')

distribution <- plot_grid(
  ttv_distribution_1_day,
  clinician_views_distribution,
  nrow=1,
  align="v",
  rel_widths=c(2,1),
  labels = c("A","B")
)
distribution

save_plot("distributionPub.png", distribution,base_height = 3,base_width = 10)

#############################

# distribution by month
data_by_month <- data_baseline %>%
  # filter(minutes_to_view < 31*24*60) %>%
  group_by(month_cat) %>%
  summarize(
    count=n(),
    viewed_percent=sum(ifelse(viewed,1,0))/n(),
    mean_views = mean(total_views),
    views_lower_fence=fivenum(total_views, na.rm = TRUE)[1],
    views_lower_quartile=fivenum(total_views, na.rm = TRUE)[2],
    views_median=fivenum(total_views, na.rm = TRUE)[3],
    views_upper_quartile=fivenum(total_views, na.rm = TRUE)[4],
    views_higher_fence=fivenum(total_views, na.rm = TRUE)[5],
    lower_fence=fivenum(minutes_to_view, na.rm = TRUE)[1],
    lower_quartile=fivenum(minutes_to_view, na.rm = TRUE)[2],
    median=fivenum(minutes_to_view, na.rm = TRUE)[3],
    upper_quartile=fivenum(minutes_to_view, na.rm = TRUE)[4],
    higher_fence=fivenum(minutes_to_view, na.rm = TRUE)[5]
  )

baseline_historical_time_to_view <- ggplot(data_by_month, aes(x=month_cat)) +
  geom_boxplot(
    aes(
      lower = lower_quartile,
      upper = upper_quartile,
      middle = median,
      ymin = 0,
      ymax = (upper_quartile+1.5*(upper_quartile-lower_quartile))
    ), 
    stat="identity", fill="#c0c0c0", size=0.3) +
  # theme(axis.text.x = element_text(angle=90,size=9,vjust=1,hjust=1))+
  xlab("month") +
  ylab("minutes") +
  scale_y_continuous(breaks = seq(0, 6000, 60)) +
  theme(axis.text.x = element_text(angle=90,size=9,hjust=1,vjust=1))
baseline_historical_time_to_view+ggtitle("median time to view chemistry and haematology results by calendar month")
ggsave('historicalTimeToViewTimeBoxPlot.png',width=10,height=5,units='in')

baseline_historical_percent_unviewed <- ggplot(data_by_month, aes(x=month_cat)) +
    geom_bar(
      aes(
        y= (1-viewed_percent)*100
      ), stat="identity", fill="#ffc0c0", colour="black", width=0.75, size=0.3) +
    ylab("percent")+
  xlab("month")+
    theme(axis.text.x = element_text(angle=90,size=9,hjust=1,vjust=1))+
    expand_limits(y=c(0,15))
baseline_historical_percent_unviewed+ggtitle("percent of chemistry and haematology results unviewed by calendar month")
  ggsave('historicalUnviewedTests.png',width=10,height=5,units='in')

  
  baseline_historical_clinicians_viewing <- ggplot(data_by_month, aes(x=month_cat)) +
    #geom_boxplot(
    #  aes(
    #    lower = views_lower_quartile,
    #    upper = views_upper_quartile,
    #    middle = views_median,
    #    ymin = views_lower_fence,
    #    ymax = (views_upper_quartile+1.5*(views_upper_quartile-views_lower_quartile))
    #  ), 
    #  stat="identity", fill="#c0c0ff", size=0.3) +
    geom_bar(
      aes(
        y= mean_views
      ), stat="identity", fill="#c0c0ff", colour="black", width=0.75, size=0.3) +
    ylab("clinicians") +
    xlab("month") +
    theme(axis.text.x = element_text(angle=90,size=9,hjust=1,vjust=1)) +
    scale_y_continuous(limits=c(0,5),breaks = seq(0, 5, 1)) 
  baseline_historical_clinicians_viewing+ggtitle("unique clinicians viewing chemistry and haematology results by calendar month")
  ggsave('historicalClinicianViewers.png',width=10,height=5,units='in')

  baseline_historical_time_to_view
  baseline_historical_percent_unviewed
  baseline_historical_clinicians_viewing
  
  baseline <- plot_grid(
    baseline_historical_time_to_view+rremove("x.text")+rremove("xlab"),
    baseline_historical_percent_unviewed+rremove("x.text")+rremove("xlab"),
    baseline_historical_clinicians_viewing,
    nrow=3,
    align="v",
    labels = c("A","B","C")
  )
  
  baseline
  save_plot("baselinePub.png", baseline,base_height = 10,base_width = 10)
  
  ###################################
  ## TODO: more fiddling with enquo
  #########################################
  # Detailed correlations
  # https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
  plot_bubble  <- function(input,grouper,bubble_colour) {
    grouper <- enquo(grouper);
    bubble_colour <- enquo(bubble_colour);
    tmpCorrelations <- input %>%
      group_by(!! grouper, !! bubble_colour)%>%
      dplyr::summarize(
        count=n(),
        median=fivenum(minutes_to_view, na.rm = TRUE)[3],
        viewed_percent=sum(total_views != 0)/n()*100
      );  
    # tmpCorrelations <- mutate(tmpCorrelations,
    #     the_label := !!grouper,
    #     the_colour := !!bubble_colour # the colon before the !!enquo is essential to stop R interpreting this as a 
    #   );
    tmpCorrelations <- tmpCorrelations %>% ungroup() %>% top_n(40,count)
    View(tmpCorrelations)
    return(ggplot(
      tmpCorrelations,
      aes(x=median,y=viewed_percent,label:=!!grouper,size=count))+
        geom_point(shape=21, aes(fill:=!!bubble_colour), colour="black")+
        scale_size_area(max_size=20)+
        geom_text_repel(size=3, segment.colour="#00000080") +
        ylab("percent viewed") +
        xlab("median time to view result (minutes)")+
        expand_limits(x = 0, y=100) +#, y=c(-0.15,0.25))+
        guides(fill=FALSE, size=FALSE)+
        scale_x_continuous(breaks=seq(0,12000,60)))
  }
  
  plot_bubble(data,discipline_name,investigation_abnormal)+ggtitle("Time to view and percent viewed by discipline")+
    scale_x_continuous(limits=c(0,3000),breaks=seq(0,12000,600))
  ggsave('timeToViewAndPercentViewedByDiscipline.png',width=10,height=5,units='in')
  plot_bubble(data_baseline,investigation_name,"#ff808080")+ggtitle("Time to view and percent viewed by test")+
    scale_x_continuous(limits=c(0,300),breaks=seq(0,12000,60))+scale_fill_identity()
  ggsave('timeToViewAndPercentViewedByTest.png',width=10,height=5,units='in')
  plot_bubble(data_baseline,ward_name,dependency_level)+ggtitle("Time to view and percent viewed by ward")+scale_fill_brewer(palette="RdYlBu")
  ggsave('timeToViewAndPercentViewedByWard.png',width=10,height=5,units='in')
  plot_bubble(data_baseline,patient_group,"#8080ff80")+ggtitle("Time to view and percent viewed by patient group")+scale_fill_identity()
  ggsave('timeToViewAndPercentViewedByPatientGroup.png',width=10,height=5,units='in')
  plot_bubble(data_baseline %>%
                mutate(age_group=patient_age %/% 10 * 10),age_group,"#d0d08080")+ggtitle("Time to view and percent viewed by patient decade")
  ggsave('timeToViewAndPercentViewedByPatientDecade.png',width=10,height=5,units='in')
  
  
  ######################################
  
    tmpCorrelations <- data %>%
      group_by(discipline_name, investigation_abnormal) %>%
      dplyr::summarize(
        count=n(),
        median=fivenum(minutes_to_view, na.rm = TRUE)[3],
        viewed_percent=sum(total_views != 0)/n()*100
      ) %>% 
      mutate(
        the_label=paste0(discipline_name,ifelse(investigation_abnormal>0," (a)"," (n)")),
        the_colour=investigation_abnormal
      );
    tmpCorrelations <- tmpCorrelations %>% ungroup() %>% top_n(40,count)
    ggplot(
      tmpCorrelations %>% arrange(desc(the_colour)),
      aes(x=median,y=viewed_percent,label=the_label,size=count))+
        geom_point(shape=21, aes(fill=the_colour), colour="black", alpha=0.5)+
        scale_size_area(max_size=30)+
        geom_text_repel(
          size=3.5,
          nudge_x = 45,
          box.padding = unit(0.1,"inch"),
          point.padding = unit(0.1,"inch"), 
          # Color of the line segments.
          segment.color = '#606060',
          segment.size = 0.5,
          # min.segment.length = unit(0.5,"inch"),
          arrow = arrow(length = unit(0.05, 'inch')),
          force = 1,
        ) +
        ylab("percent viewed") +
        xlab("median time to view result (minutes)")+
        expand_limits(x = 0, y=100) +#, y=c(-0.15,0.25))+
        guides(fill=FALSE, size=FALSE)+
      ggtitle("Time to view and percent viewed by discipline")+
      scale_x_continuous(limits=c(0,3000),breaks=seq(0,12000,600))+
      scale_fill_continuous(low="#4040ff",high="#ff4040")
    ggsave('timeToViewAndPercentViewedByDiscipline.svg',width=5,height=3,units='in')
  
  ##############################
    
    tmpCorrelations <- data_baseline %>%
      group_by(investigation_name) %>%
      dplyr::summarize(
        count=n(),
        median=fivenum(minutes_to_view, na.rm = TRUE)[3],
        viewed_percent=sum(total_views != 0)/n()*100
      ) %>% 
      mutate(
        the_label=investigation_name
      );
    tmpCorrelations <- tmpCorrelations %>% ungroup() %>% top_n(40,count)
    ggplot(
      tmpCorrelations,
      aes(x=median,y=viewed_percent,label=the_label,size=count))+
      geom_point(shape=21, fill="#ff8080", colour="black", alpha=0.5)+
      scale_size_area(max_size=15)+
      geom_text_repel(
        size=2,
        segment.color = '#00000040',
      ) +
      ylab("percent viewed") +
      xlab("median time to view result (minutes)")+
      expand_limits(x = 0, y=100) +#, y=c(-0.15,0.25))+
      guides(fill=FALSE, size=FALSE)+
      ggtitle("Time to view and percent viewed by test")+
      scale_x_continuous(limits=c(0,300),breaks=seq(0,300,60))
    ggsave('timeToViewAndPercentViewedByTest.svg',width=5,height=3,units='in')
    
    ##############################
    
    tmpCorrelations <- data_baseline %>%
      group_by(ward_name,dependency_level) %>%
      dplyr::summarize(
        count=n(),
        median=fivenum(minutes_to_view, na.rm = TRUE)[3],
        viewed_percent=sum(total_views != 0)/n()*100
      ) %>% 
      mutate(
        the_label=ward_name,
        the_colour=dependency_level
      );
    tmpCorrelations <- tmpCorrelations %>% ungroup() %>% top_n(40,count)
    ggplot(
      tmpCorrelations,
      aes(x=median,y=viewed_percent,label=the_label,size=count))+
      geom_point(shape=21, aes(,fill=the_colour), colour="black", alpha=0.5)+
      scale_size_area(max_size=18)+
      geom_text_repel(
        size=2.5,
        segment.color = '#00000040',
        force=1,
        box.padding=0.25
      ) +
      ylab("percent viewed") +
      xlab("median time to view result (minutes)")+
      expand_limits(x = 0, y=100) +#, y=c(-0.15,0.25))+
      guides(fill=FALSE, size=FALSE)+
      ggtitle("Time to view and percent viewed by ward")+
      scale_x_continuous(limits=c(0,200),breaks=seq(0,200,60))
    ggsave('timeToViewAndPercentViewedByWard.svg',width=5,height=3,units='in')