library(ROCR)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)

setwd("~/SparkleShare/copyright-content/costOptimisation/R")
clotting <- read.delim("clotting.tsv.INR.class.test.pred.result", header=FALSE)

# pred <- prediction(clotting$V3, clotting$V1)
# perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
# plot(perf, col=rainbow(10))
# abline(a=0, b= 1)
# summary(perf)
# plot(perf, avg="threshold", spread.estimate="boxplot")
# 
# 
# 
# acc.perf = performance(pred, measure = "sens")
# plot(acc.perf)
# plot(performance(pred,measure = "ppv"))
# plot(performance(pred,measure = "rec",x.measure="prec"))
# 
# plot(performance(pred,measure = "cost",cost.fp=2,cost.fn=10))


# plot differential probability densities
plot1 <- ggplot(clotting)+
  geom_density(data=clotting %>% filter(V1==0),aes(x=V3), fill="#ffb0b080")+
  geom_density(data=clotting %>% filter(V1==1),aes(x=V3), fill="#b0b0ff80")+
  xlab("cut off");

# 
tot = nrow(clotting)
cond_pos = sum(clotting$V1)
cond_neg = tot-cond_pos
bin_width = 0.04

dist <- clotting %>% mutate(
    prob_bin = V3 %/% bin_width * bin_width
  ) %>% group_by(prob_bin) %>%
  dplyr::summarize(
      orig_num = n(),
      orig_pos = sum(V1)
  ) 

dist <- dist %>% 
  expand(prob_bin = c(0:(1/bin_width))*bin_width) %>% 
  left_join(.,dist) %>% 
  replace_na(list(orig_num = 0, orig_pos=0))

dist['num'] <- dist['orig_num']
  # stats::filter(dist['orig_num'],filter=rep(1/15,15),method='convolution',sides=2,circular=FALSE)
dist['pos'] <- dist['orig_pos']
  # stats::filter(dist['orig_pos'],filter=rep(1/15,15),method='convolution',sides=2,circular=FALSE)


dist <- dist %>% mutate(
      cum = cumsum(num),
      cum_pos = cumsum(pos)
  ) %>% ungroup() %>% mutate(
      test_neg = cum,
      test_pos = tot - cum,
      fn = cum_pos,
      tn = cum-cum_pos,
      tp = (tot_pos-cum_pos),
      fp = (tot-cum)-(tot_pos-cum_pos)
  ) %>% mutate(
      prob_dist = num / tot / bin_width,
      cum_prob_dist = cum / tot,
      sens = tp / (tp+fn),
      spec = tn / (tn+fp)
  ) %>% mutate(
      delta_sens = (
        ifelse(is.na(lead(sens,1)),sens,lead(sens,1))-
        ifelse(is.na(lag(sens,1)),sens,lag(sens,1))
      )/(
        ifelse(is.na(lead(prob_bin,1)),prob_bin,lead(prob_bin,1))-
        ifelse(is.na(lag(prob_bin,1)),prob_bin,lag(prob_bin,1))
      ),
      f_over_g_prime = prob_dist/delta_sens
  )
  
dist

plot2 <- ggplot(dist)+geom_smooth(aes(x=prob_bin, y=prob_dist),span=0.3)+xlab("cut-off")+ylab("density: f(x)")

plot3 <- ggplot(dist)+geom_smooth(span=0.3,aes(x=prob_bin, y=cum_prob_dist))+xlab("cut-off")+ylab("cumulative probability: F(x)")

plot4 <- ggplot(dist)+
  geom_smooth(span=0.3,aes(x=prob_bin, y=sens),colour="blue")+
  geom_smooth(span=0.3,aes(x=prob_bin, y=spec),colour="red")+xlab("cut-off")+ylab("sensitivity / specificity")

roc <- ggplot(dist)+
  geom_smooth(span=0.3,aes(x=1-spec, y=sens))+
  xlab("1-specificity")+ylab("sensitivity")+
  geom_abline(slope=1, intercept=0,colour="#808080")

plot5 <- ggplot(dist)+geom_smooth(span=0.3,aes(x=prob_bin, y=delta_sens))+xlab("cut-off")+ylab("delta sensitivity: g'(x)")

plot6 <- ggplot(dist)+geom_smooth(span=0.3,aes(x=prob_bin, y=f_over_g_prime))+xlab("cut-off")+ylab("f(x)/g'(x)")

grid <- plot_grid(
  plot1+rremove("x.text")+rremove("xlab"),
  plot2+rremove("x.text")+rremove("xlab"),
  plot3+rremove("x.text")+rremove("xlab"),
  plot4,
  plot5,
  plot6,
  nrow=2,
  align="v",
  labels = c("A","B","C","D","E","F")
)

ggsave("costOptimum.svg", plot=grid,
  height=173.5/25.4*1.25/1.6,
  width=173.5/25.4*1.25
)

ggsave("costOptimum.png", plot=grid,
       height=173.5/25.4*1.25/1.6,
       width=173.5/25.4*1.25
)

ggsave("clottingRoc.svg", plot=roc,
       height=89/25.4*1.25,
       width=89/25.4*1.25
)

ggsave("clottingRoc.png", plot=roc,
       height=89/25.4*1.25,
       width=89/25.4*1.25
)