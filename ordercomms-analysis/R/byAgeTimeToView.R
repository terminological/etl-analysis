library(ggplot2)
library(dplyr)
library(reshape2)
library(ggrepel)


data_by_age <- data_baseline %>%
  group_by(patient_age) %>%
  summarize(
    count=n(),
    viewed_percent=sum(total_views != 0)/n(),
    lower_fence_ttv=fivenum(minutes_to_view, na.rm = TRUE)[1],
    lower_quartile_ttv=fivenum(minutes_to_view, na.rm = TRUE)[2],
    median_ttv=fivenum(minutes_to_view, na.rm = TRUE)[3],
    upper_quartile_ttv=fivenum(minutes_to_view, na.rm = TRUE)[4],
    higher_fence_ttv=fivenum(minutes_to_view, na.rm = TRUE)[5]
  )

ttv_box_by_age <- ggplot(data_by_age, aes(x=patient_age)) +
  geom_boxplot(
    aes(
      lower = lower_quartile_ttv,
      upper = upper_quartile_ttv,
      middle = median_ttv,
      ymin = 0,
      ymax = upper_quartile_ttv+1.5*(upper_quartile_ttv-lower_quartile_ttv)
    ), 
    stat="identity", fill="#c0c0c0", size=0.3) +
  ylab("minutes")+
  theme(axis.text.x = element_text(angle=90,size=9))+
  xlab("patient age")
ttv_box_by_age+ggtitle("Median time to view test results by patient age")
ggsave('ttvByPatientAge.png',width=10,height=5,units='in')

unviewed_by_age <- ggplot(data_by_age, aes(x=patient_age)) +
  geom_bar(
    aes(
      y= (1-viewed_percent)*100
    ), stat="identity", fill="#ffc0c0", colour="black", width=0.75, size=0.3) +
  ylab("percent")+
  theme(axis.text.x = element_text(angle=90,size=9))+
  xlab("age")+
  expand_limits(y=c(0,15))
unviewed_by_age
ggsave('unviewedByPatientAge.png',width=10,height=5,units='in')
