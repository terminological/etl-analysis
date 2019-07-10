library(dplyr)
library(ggplot2)

ttv <- read.csv("~/Dropbox/minutes_to_view.txt",header=FALSE,col.names=c("minutes"))
bootstrap <- read.csv("~/Dropbox/medians.txt", sep="\t",header = FALSE, col.names = c("samples","sample_median"))
dist_mean <- mean(ttv$minutes);
dist_med <- median(ttv$minutes);
dist_se <- sd(ttv$minutes)/sqrt(nrow(ttv));

bootsummary <- NA
bootsummary <- bootstrap %>% group_by(samples) %>% arrange(sample_median) %>% summarise(
  median = median(sample_median),
  mean = mean(sample_median),
  lower95 = sample_median[250],
  upper95 = sample_median[9750]
) %>% mutate(
    lower95delta = median-lower95,
    upper95delta = upper95-median,
    invSqrtSamples = 1/sqrt(samples)
) 

ci.low <- lm(lower95delta ~ 0+invSqrtSamples, bootsummary)
ci.high <- lm(upper95delta ~ 0+invSqrtSamples, bootsummary)

fit_plot <- ggplot(bootsummary, aes(x=invSqrtSamples))+
  geom_point(aes(y=lower95delta,colour="lower"), alpha=0.5)+
  geom_line(data = fortify(ci.low), aes(x = invSqrtSamples, y = .fitted,colour="lower"))+
  geom_point(aes(y=upper95delta,colour="upper"), alpha=0.5)+
  geom_line(data = fortify(ci.high), aes(x =invSqrtSamples, y = .fitted,colour="upper"))+
  xlab("inverse square root of sample size")+
  ylab("95% confidence limit from median")

predictLowCI <- function(x = vector(), std_dev = NA, sample_size = NA, sample_median = NA) {
  if (length(x)>0) {
    std_dev <- sd(x);
    sample_size <-length(x);
    sample_median <-median(x);
  }
  return( sample_median - as.numeric(ci.low$coefficients[1])/dist_se*abs(std_dev)/abs(sample_size))
}

predictHighCI <- function(x = vector(), std_dev = NA, sample_size = NA, sample_median = NA) {
  if (length(x)>0) {
    std_dev <- sd(x);
    sample_size <-length(x);
    sample_median <-median(x);
  }
  if (sample_size < 0) sample_size = -sample_size
  if (std_dev < 0) std_dev = -std_dev
  return( sample_median + as.numeric(ci.low$coefficients[1])/dist_se*abs(std_dev)/abs(sample_size))
}