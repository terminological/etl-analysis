library(DBI)
library(odbc)
library(datasets)
library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)
library(tidyr)

pwd <- readline(prompt="Enter DB: ");
con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 13 for SQL Server",
                 Server = "10.174.129.118",
                 Database = "RobsDatabase",
                 UID = "RobertSQL",
                 PWD = pwd,
                 Port = 1433);

dbListTables(con);

data <- dbReadTable(con, "appendicitisExample") #, as.is=c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE));

plotdata <- data %>%
	mutate(
			time_cat = cut(days,breaks=c(-Inf,1.5,2.5,4.5,7.5,Inf), labels=c("<1d","1-2d","2-4d","4d-1w", "1w-1m")),
			
  ) %>% separate(
    Normal.Range, c("low_norm","high_norm"),sep="-"
  ) %>%
  group_by(Code) %>%
  mutate(
    low_norm = mean(as.numeric(low_norm)),
    high_norm = mean(as.numeric(high_norm))
  ) %>%
  ungroup() %>%
  group_by(Code,time_cat) %>%
  summarise(
    description = min(Description),
    count = n(),
    avg = mean(Numeric.value),
    sd = sd(Numeric.value),
    low_norm = mean(low_norm),
    high_norm = mean(high_norm)
  ) %>% mutate(
    lower = avg-1.96*(sd/sqrt(count)),
    upper = avg+1.96*(sd/sqrt(count))
  )

plotdata <- plotdata %>% inner_join(plotdata %>% filter(time_cat=="<1d", count>100) %>% select(Code))

p <- ggplot(plotdata, aes(x=time_cat)) + 
  geom_hline(aes(yintercept=low_norm),colour="red") +
  geom_hline(aes(yintercept=high_norm),colour="red") + 
  geom_point(aes(y=avg),colour="#0000ff") + 
  geom_errorbar(aes(ymin=lower,ymax=upper),colour="#8080ff") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + 
  xlab("time before appendicectomy")+ylab("value")
  
p2 <- p + facet_wrap(~description,nrow = 3, ncol=5,scales='free_y')
p2

ggsave("~/Dropbox/Thread/threadAppendicitisDemo.png", p2 ,width=10, height=6, units='in')