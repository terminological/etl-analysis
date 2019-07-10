# TODO: Add comment
# 
# Author: robchallen
###############################################################################
# https://tbrieder.org/epidata/course_e_ex04_task.pdf
# https://stats.stackexchange.com/questions/256148/stratification-in-cox-model
# http://www.sthda.com/english/wiki/cox-proportional-hazards-model

library(survival)
library(survminer)

###
# Time to view cox model


covariates <- c(
		"normalcy",  
		"dependency_level", 
		"age_group", 
		"patient_gender",
		"three_hours_delayed_view",
		"grossly_abnormal",
		"five_or_more_clinicians")

viewed_data2 <- data2 %>% mutate(
  recovered = 1,
  three_hours_delayed_view = ifelse(is.na(minutes_to_view) | minutes_to_view > 180, 1, 0),
  five_or_more_clinicians = (total_views >= 5),
  grossly_abnormal = (degree < -3 | degree > 5)
  )

counts <- lapply( covariates, function(x) {eval(substitute(count(viewed_data2,tmp),list(tmp=as.name(x))))})
sink("characteristics.txt", append=FALSE, split=FALSE)
lapply(counts,function(x){x})
sink();

univ_formulas <- sapply(covariates,
		function(x) as.formula(paste('Surv(minutes_to_resolution, recovered)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = viewed_data2)})
sink("univariate.txt", append=FALSE, split=FALSE)
lapply(univ_models,function(x){ summary(x)})
sink();

# plot(survfit(cox), ylim=c(0, 1), xlab="minutes",
# 		ylab="not viewed")

multiv_formula = as.formula(paste("Surv(minutes_to_resolution, recovered) ~ ",paste(covariates,collapse=" + ")));
multiv_model <- coxph(multiv_formula, data = viewed_data2)
sink("multivariate.txt", append=FALSE, split=FALSE);
summary(multiv_model);
sink();

