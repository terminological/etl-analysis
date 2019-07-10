library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)

load("~/R/timeToViewData")
data = 0
pub_rate = 0



test_select <- data_baseline %>% group_by(investigation_name) %>% summarise(count=n()) %>% top_n(30,count) %>% select(investigation_name)

anovaInput <- data_baseline %>% semi_join(test_select) %>% mutate(
  investigation_name=as.factor(as.character(investigation_name))) %>% select(investigation_name,minutes_to_view)

levels(anovaInput$investigation_name)

aov <- aov(minutes_to_view ~ investigation_name, data = anovaInput)
summary(aov)

TukeyHSD(aov)

kruskal.test(minutes_to_view ~ investigation_name, data = anovaInput)

wilcoxon = pairwise.wilcox.test(anovaInput$minutes_to_view, anovaInput$investigation_name)
test.result <- melt (wilcoxon[[3]],na.rm=T)

ggplot(data = test.result, aes(Var1, Var2, fill = value)) +
  geom_tile(aes(fill=value),color="white") +
  scale_fill_continuous(name="p-Val")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1)) +
  coord_fixed()