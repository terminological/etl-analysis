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
		"discipline_name", 
		"investigation_abnormal",  
		"ward_name", 
		"dependency_level", 
		"patient_group", 
		"day_time_cat")

counts <- lapply( covariates, function(x) {eval(substitute(count(data,tmp),list(tmp=as.name(x))))})

univ_formulas <- sapply(covariates,
		function(x) as.formula(paste('Surv(minutes_to_view, viewed)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = data)})
# Extract data 
univ_results <- lapply(univ_models,
		function(x){ 
			x <- summary(x)
			p.value<-signif(x$wald["pvalue"], digits=2)
			wald.test<-signif(x$wald["test"], digits=2)
			beta<-signif(x$coef[1], digits=2);#coeficient beta
			HR <-signif(x$coef[2], digits=2);#exp(beta)
			HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
			HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
			HR <- paste0(HR, " (", 
					HR.confint.lower, "-", HR.confint.upper, ")")
			res<-c(beta, HR, wald.test, p.value)
			names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
					"p.value")
			return(res)
			#return(exp(cbind(coef(x),confint(x))))
		})
univ_models <- NA
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)



# plot(survfit(cox), ylim=c(0, 1), xlab="minutes",
# 		ylab="not viewed")

multiv_formula = as.formula(paste("Surv(minutes_to_view, viewed) ~ ",paste(covariates,collapse=" + ")));
multiv_model <- coxph(multiv_formula, data = data)

tmp_model <- coxph(Surv(minutes_to_view, viewed) ~ day_time_cat, data = data)
summary(tmp_model)
tmp <- cox.zph(tmp_model)
