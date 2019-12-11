#' TODO: a custom sql translation like:
#' https://github.com/tidyverse/dbplyr/issues/247

#' Helper function to calculate probability from counts in dplyr friendly manner
#'
#' The purpose of this is to calculate the probabilities of given all binary outcomes from
#' a table of marginal frequencies in a dplyr friendly way. 
#' 
#' @param df a dataframe containing one observation per row
#' @param x1y1Var the datatable column containing frequency of cooccurrences of events x1 and y1
#' @param x1Var the datatable column containing frequency of occurrences of event x1
#' @param y1Var the datatable column containing frequency of occurrences of event y1
#' @param totalVar the datatable column containing total number of events
#' @return the datatable with additional columns for all the probabilities associated with each outcome (i.e. a 2x2 confusion matrix)
#' @export
probabilitiesFromCounts = function(df, x1y1Var, x1Var, y1Var, totalVar) {
	x1y1Var = ensym(x1y1Var)
	x1Var = ensym(x1Var)
	y1Var = ensym(y1Var)
	totalVar = ensym(totalVar)
	return(
		df %>% mutate(
			p_x1 = as.double(!!x1Var)/!!totalVar,
			p_x0 = 1-p_x1,
			p_y1 = as.double(!!y1Var)/!!totalVar,
			p_y0 = 1-p_y1,
			p_x1y1 = as.double(!!x1y1Var)/!!totalVar,
			p_x1y0 = p_x1 - p_x1y1,
			p_x0y1 = p_y1 - p_x1y1,
			p_x0y0 = 1.0-p_x1y1-p_x0y1-p_x1y0
		) %>% mutate(
			p_x1 = ifelse(p_x1 < 0.0 | p_x1 > 1.0, NA, p_x1),
			p_x0 = ifelse(p_x0 < 0.0 | p_x0 > 1.0, NA, p_x0),
			p_y1 = ifelse(p_y1 < 0.0 | p_y1 > 1.0, NA, p_y1),
			p_y0 = ifelse(p_y0 < 0.0 | p_y0 > 1.0, NA, p_y0),
			p_x1y1 = ifelse(p_x1y1 < 0.0 | p_x1y1 > 1.0, NA, p_x1y1),
			p_x0y1 = ifelse(p_x0y1 < 0.0 | p_x0y1 > 1.0, NA, p_x0y1),
			p_x1y0 = ifelse(p_x1y0 < 0.0 | p_x1y0 > 1.0, NA, p_x1y0),
			p_x0y0 = ifelse(p_x0y0 < 0.0 | p_x0y0 > 1.0, NA, p_x0y0)
		) 
	)
}

#' Helper function to calculate probability from confusion matrix stats in dplyr friendly manner
#'
#' The purpose of this is to calculate the probabilities of binary outcomes from
#' a table of true pos, true neg, false pos and false neg trials 
#' 
#' @param df a dataframe containing one observation per row
#' @param tpVar the datatable column containing frequency of cooccurrence of true positives
#' @param fpVar the datatable column containing frequency of occurrence of false positives
#' @param fnVar the datatable column containing frequency of occurrence of false negatives
#' @param tnVar the datatable column containing frequency of occurrence of true negatives
#' @return the datatable with additional columns for all the probabilities associated with each outcome (i.e. a 2x2 confusion matrix)
#' @export
probabilitiesFromConfusionMatrix = function(df, tpVar, fpVar, fnVar, tnVar) {
	tpVar = ensym(tpVar)
	fpVar = ensym(fpVar)
	fnVar = ensym(fnVar)
	tnVar = ensym(tnVar)
	return(
		df %>% mutate(
			total = !!tpVar+!!fpVar+!!fnVar+!!tnVar,
			p_x1y1 = as.double(!!tpVar)/total,
			p_x1y0 = as.double(!!fnVar)/total,
			p_x0y1 = as.double(!!fpVar)/total,
			p_x0y0 = as.double(!!tnVar)/total,
			p_x1 = p_x1y1+px1y0,
			p_x0 = p_x0y1+px0y0,
			p_y1 = p_x1y1+px0y1,
			p_y0 = p_x1y0+px0y0
		)
	)
}

#' A calculate multiple 2 class mutual information scores from confusion matrix probabilities in dplyr friendly manner
#'
#' The purpose of this is to make it possible to calculate MI in a DBPLYR sql table
#' 
#' @param df a dataframe containing one observation per row & p_x1y1, p_x0y1, p_x1y0, and p_x0y0 columns (see probabilitiesFromCounts)
#' @return the datatable with additional columns for MI; PMI0 and PMI1
#' @export
calculateBinaryMI = function(df) {
	return(
		df %>% mutate(
			pmi_x1y1 = ifelse( p_x1y1==0, ifelse(p_x1==0 | p_y1==0, 0, -Inf), log(p_x1y1/(p_x1*p_y1)) ),
			pmi_x0y1 = ifelse( p_x0y1==0, ifelse(p_x0==0 | p_y1==0, 0, -Inf), log(p_x0y1/(p_x0*p_y1)) ),
			pmi_x1y0 = ifelse( p_x1y0==0, ifelse(p_x1==0 | p_y0==0, 0, -Inf), log(p_x1y0/(p_x1*p_y0)) ),
			pmi_x0y0 = ifelse( p_x0y0==0, ifelse(p_x0==0 | p_y0==0, 0, -Inf), log(p_x0y0/(p_x0*p_y0)) ),
			mi_binary = (
				ifelse(p_x1y1==0|p_x1==0|p_y1==0, 0, p_x1y1*pmi_x1y1)+
				ifelse(p_x0y1==0|p_x0==0|p_y1==0, 0, p_x0y1*pmi_x0y1)+
				ifelse(p_x1y0==0|p_x1==0|p_y0==0, 0, p_x1y0*pmi_x1y0)+
				ifelse(p_x0y0==0|p_x0==0|p_y0==0, 0, p_x0y0*pmi_x0y0)
			),
			npmi_x1y1 = ifelse( p_x1y1==0, ifelse(p_x1==0 | p_y1==0, 0, -1), pmi_x1y1 / (-log(p_x1y1)) ),
			npmi_x0y1 = ifelse( p_x0y1==0, ifelse(p_x0==0 | p_y1==0, 0, -1), pmi_x0y1 / (-log(p_x0y1)) ),
			npmi_x1y0 = ifelse( p_x1y0==0, ifelse(p_x1==0 | p_y0==0, 0, -1), pmi_x1y0 / (-log(p_x1y0)) ),
			npmi_x0y0 = ifelse( p_x0y0==0, ifelse(p_x0==0 | p_y0==0, 0, -1), pmi_x0y0 / (-log(p_x0y0)) )
		)
	)
}

#' Ccalculate multiple confusion matrix stats from mariginal probabilities in dplyr friendly manner
#'
#' The purpose of this is to make it possible to calculate accuracy stats in a DBPLYR sql table
#' 
#' @param df a dataframe containing one observation per row & p_x1y1, p_x0y1, p_x1y0, and p_x0y0 columns (see probabilitiesFromConfusionMatrix)
#' @return the datatable with additional columns for true_pos_rate / true_neg_rate / etc...
#' @export
calculateConfusionMatrixStats = function(df) {
	return(df %>% mutate(
		
		true_pos_rate = p_x1y1/p_x1,
		true_neg_rate = p_x0y0/p_x0,
		false_pos_rate = p_x0y1/p_x0,
		false_neg_rate = p_x1y0/p_x1,
		
		neg_pred_value = p_x0y0/p_y0,
		pos_pred_value = p_x1y1/p_y1,
		
		specificity = true_neg_rate,
		sensitivity = true_pos_rate,
				
		precision = pos_pred_value,
		recall = true_pos_rate,
		
		accuracy = p_x1y1+p_x0y0,
		f1 = 2*precision*recall/(precision+recall),
		mcc = (p_x1y1*p_x0y0 - p_x0y1*p_x1y0) / sqrt(p_x1*p_x0*p_y1*py0),
		informedness = true_pos_rate+true_neg_rate-1,
		youdens_j = informedness

	))
}

#' calculate single mutual information score from multiclass groups in dplyr friendly manner
#'
#' The purpose of this is to make it possible to calculate MI in a DBPLYR sql table
#' 
#' @param df a dataframe containing one observation per row & minimally p_x1y1, p_x1, p_y1 columns (see probabilitiesFromCounts)
#' @return the datatable with additional columns for MI
#' @export
calculateMultiClassMI = function(df) {
	return(
		df %>% mutate(
			pmi_x1y1 = ifelse( p_x1y1==0, ifelse(p_x1==0 | p_y1==0, 0, -Inf), log(p_x1y1/(p_x1*p_y1)) ),
			# h_x1y1 = -log(p_x1y1),
			# npmi_x1y1 = ifelse( p_x1y1==0, ifelse(p_x1==0 | p_y1==0, 0, -1), pmi_x1y1 / h_x1y1 ),
			mi_component = ifelse(p_x1y1==0|p_x1==0|p_y1==0, 0, p_x1y1*pmi_x1y1)
		) %>% summarise(
			mi = sum(mi_component)
		)
	)
}

#' Helper function to calculate probability from grouped data in a tidy friendly manner
#'
#' The purpose of this is to calculate the probabilities of all binary outcomes from
#' a table of marginal frequencies using tidy syntax
#' 
#' @param df a dataframe containing 2 columns defining class of event X and class of event Y and either one row per event, 
#' or a count of observations, for each class combination. 
#' df may also be grouped and in which case the grouping is preserved in the result 
#' @param groupXVar the datatable column defining the class of event X
#' @param groupXVar the datatable column defining the class of event Y
#' @param countVar the datatable column containing the observed frequency combination of event XY. If this is missing the row count will be used instead
#' @return A new datatable with all possible combinations of X&Y and the probabilities associated with each outcome (i.e. a 2x2 confusion matrix for each XY)
#' @export
probabilitiesFromGroups = function(df, groupXVar, groupYVar, countVar=NA) {
	grps = df %>% groups()
	groupXVar = ensym(groupXVar)
	groupYVar = ensym(groupYVar)
	if (identical(grps,NULL)) {
		grpsList = NULL
	} else {
		grpsList = sapply(grps,as.character)
	}
	joinList = c(grpsList,"join")
	if (is.na(countVar)) {
		df = df %>% group_by(!!!grps, !!groupXVar, !!groupYVar) %>% summarise(f_xy = n())
	} else {
		countVar = ensym(countVar)
		df = df %>% group_by(!!!grps, !!groupXVar, !!groupYVar) %>% summarise(f_xy = sum(!!countVar))
	}
	N = df %>% group_by(!!!grps) %>% summarise(N=sum(f_xy)) %>% mutate(join=1)
	X = df %>% group_by(!!!grps,!!groupXVar) %>% summarise(f_x = sum(f_xy)) %>% mutate(join=1) # grouping
	Y = df %>% group_by(!!!grps,!!groupYVar) %>% summarise(f_y = sum(f_xy)) %>% mutate(join=1) # grouping
	XY = (X %>% inner_join(Y, by=joinList) %>% inner_join(N, by=joinList)) %>% select(-join)
	joinAll = c(grpsList,as.character(groupXVar),as.character(groupYVar))
	XY = XY %>% 
			left_join(df, by=joinAll) %>% 
			mutate( f_xy = ifelse(is.na(f_xy),0,f_xy))
	return( XY %>% group_by(!!!grps) %>% probabilitiesFromCounts(f_xy,f_x,f_y,N) )
}

#' calculate mutual information between a categorical value (X) and a continuous value (Y)
#' 
#' @param df - may be grouped, in which case the value is interpreted as different types of continuous variable
#' @param groupXVar - the column of the categorical value (X)
#' @param valueYVar - the column of the continuous value (Y)
calculateMultiClassContinuousMI_SG = function(df, groupXVar, valueYVar, k_05=10) {
	#df = contMI %>% group_by(test)
	grps = df %>% groups()
	groupXVar = ensym(groupXVar)
	valueYVar = ensym(valueYVar)
	#groupXVar = as.symbol("outcome")
	#valueYVar = as.symbol("value")
	
	if (identical(grps,NULL)) {
		grpsList = NULL
	} else {
		grpsList = sapply(grps,as.character)
	}
	joinList = c(grpsList, as.character(groupXVar))
	
	# this is confusing because groups mean 2 things here - the 
	# different types of Y (grps) which should be preserved and the categorical X 
	# NX has group counts (N) and subgroup counts (N_X) 
	N_X = df %>% group_by(!!!grps,!!groupXVar) %>% summarise(N_X = n()) %>% group_by(!!!grps) %>% mutate(N = sum(N_X))
	
	# sort by valueYVar
	# assign ungrouped rank (rank)
	# apply grouping groupXVar and sort by valueYVar within group
	# rank within outcome group (groupRank)
	# View(tmp %>% filter(outcome=="low" & test=="K")) # verify collisions get a random rank
		
	tmp = df %>% inner_join(N_X, by=joinList) %>% mutate(
			y=!!valueYVar,
			x=!!groupXVar)
	# construct 
	
	# get the group wise pdfs (i.e. p_xy)
	# process with X grouped and ordered by Y within groups
	# value of Y wrt rank of Y is an inverse CDF function. This is evenly spaced on rank of Y
	# spacing of rank depends on the number of observations (and actually we don't need the value)
	# gradient of this is inverse of PDF. This needs heavy smoothing to be useable subsequently.
	tmp2 = tmp %>%  group_by(!!!grps,x) %>% arrange(y) %>% group_modify(
		function(d,...) {
			data.frame(
					N_X=d$N_X,
					N = d$N,
					y = d$y,
					d_xy_d_r = signal::sgolayfilt(d$y,p=3,n=k_05*2+1,m=1,ts=1/(mean(d$N_X)))
			) %>% mutate(
					p_y_given_x = 1/d_xy_d_r
		#	) %>% mutate(
		#			corr = (p_y_given_x+lag(p_y_given_x,1,default=0))*(y-lag(y))/2
		#	) %>% mutate(
		#			p_y_given_x = p_y_given_x/sum(corr, na.rm=TRUE)
		#	) %>% select(
		#			-corr,
			)
		}
	)
	
	# get the overall pdf for the combination of all x's (gives us p_y)	
	tmp2 = tmp2 %>%  group_by(!!!grps) %>% arrange(y) %>% group_modify(
		function(d,...) {
			data.frame(
					x=d$x,
					N_X=d$N_X,
					N = d$N,
					y = d$y,
					p_y_given_x = d$p_y_given_x,
					d_xy_d_r = d$d_xy_d_r,
					d_y_d_r = signal::sgolayfilt(d$y,p=3,n=k_05*2+1,m=1,ts=1/(mean(d$N)))
			) %>% mutate(
					p_y = 1/d_y_d_r,
					p_x = N_X/N
			) %>% mutate(
		#			corr_p_y = (p_y+lag(p_y,1,default=0))*(y-lag(y))/2
		#	) %>% mutate(
		#			p_y = p_y/sum(corr_p_y, na.rm=TRUE),
					pmi_xy = ifelse(
							p_y_given_x > 0 & p_y > 0,
							p_x*p_y_given_x*log(p_y_given_x/p_y),
							0	
					)
			)
		}
	)

	# do the integration
	# points are not evenly spaced in y so piecewise integration required
	tmp3 = tmp2 %>% group_by(!!!grps,x) %>% arrange(y) %>% mutate(
			# integrate over dy ( well does the trapeziod part of the integration
			d_I_d_xy = (pmi_xy+lag(pmi_xy,1,default=0))*(y-lag(y))/2
		) %>% group_by(!!!grps) %>% summarise (
			#sum over x (& sums trapeziums over y at the same time)
			I = sum(d_I_d_xy,na.rm=TRUE)
		)

	return(tmp3)
}
	# apply sgolay filter (cubic, window = 5) to groups
#	tmp = tmp %>% group_by(!!!grps) %>% arrange(!!!grps,!!valueYVar) %>% mutate(
#			v = !!valueYVar,
#			v_n1 = lag(!!valueYVar,1),
#			v_p1 = lead(!!valueYVar,1),
#			v_n2 = lag(!!valueYVar,2),
#			v_p2 = lead(!!valueYVar,2)
#	) %>% mutate(
#		inv_pdf = 1/(10*N)*(-2*v_n2-1*v_n1+1*v_p1+2*v_p2),
#		pdf = 1/inv_pdf,
#	)

	# apply sgolay filter (cubic, window = 5) to groups 
#	tmp = tmp %>% group_by(!!!grps,!!groupXVar) %>% arrange(!!!grps,!!groupXVar,!!valueYVar) %>% mutate(
#				v_x = !!valueYVar,
#				v_x_n1 = lag(!!valueYVar,1),
#				v_x_p1 = lead(!!valueYVar,1),
#				v_x_n2 = lag(!!valueYVar,2),
#				v_x_p2 = lead(!!valueYVar,2)
#	) %>% mutate(
#				inv_pdf_x = 1/(10*N_X)*(-2*v_x_n2-1*v_x_n1+1*v_x_p1+2*v_x_p2),
#				pdf_x = 1/inv_pdf_x,
#	)

#	KNN method:
#	not working
#	tmp = tmp %>% group_by(!!!grps,!!groupXVar) %>% arrange(!!valueYVar) %>% mutate(groupRank = row_number())
#	tmp = tmp %>% group_by(!!!grps,!!groupXVar) %>% arrange(!!!grps,!!groupXVar,!!valueYVar) %>% mutate(
#			knnLag = as.integer(k_05),
#			knnLead = as.integer(k_05)
#	) 
#	for (i in c(1,k_05)) {
#		leadColName = paste0("knnLead",i)
#		lagColName = paste0("knnLead",i)
#		tmp = tmp %>% mutate(
#				knnLagDeltaVal = !!valueYVar-lag(!!valueYVar,n=knnLag),
#				knnLeadDeltaVal = lag(!!valueYVar,n=knnLead)-!!valueYVar,
#				knnLagPlusDeltaVal = !!valueYVar-lag(!!valueYVar,n=knnLag+1),
#				knnLeadPlusDeltaVal = lag(!!valueYVar,n=knnLead+1)-!!valueYVar,
#				knnLag = ifelse(knnLagPlusDeltaVal < knnLeadDeltaVal, knnLag+1, ifelse(knnLeadPlusDeltaVal < knnLagDeltaVal, knnLag-1, knnLag)),
#				knnLead = ifelse(knnLeadPlusDeltaVal < knnLagDeltaVal, knnLead+1, ifelse(knnLagPlusDeltaVal < knnLeadDeltaVal, knnLead-1, knnLead)),
#		)
#	}


#' calculate mutual information between a categorical value (X) and a continuous value (Y)
#' 
#' @param df - may be grouped, in which case the value is interpreted as different types of continuous variable
#' @param groupXVar - the column of the categorical value (X)
#' @param valueYVar - the column of the continuous value (Y)
calculateMultiClassContinuousMI_KNN = function(df, groupXVar, valueYVar, k_05=2) {
	#df = contMI %>% group_by(test)
	grps = df %>% groups()
	groupXVar = ensym(groupXVar)
	valueYVar = ensym(valueYVar)
	#groupXVar = as.symbol("outcome")
	#valueYVar = as.symbol("value")
	
	if (identical(grps,NULL)) {
		grpsList = NULL
	} else {
		grpsList = sapply(grps,as.character)
	}
	joinList = c(grpsList, as.character(groupXVar))
	
	# this is confusing because groups mean 2 things here - the 
	# different types of Y (grps) which should be preserved and the categorical X 
	# NX has group counts (N) and subgroup counts (N_X) 
	N_X = df %>% group_by(!!!grps,!!groupXVar) %>% summarise(N_X = n()) %>% group_by(!!!grps) %>% mutate(N = sum(N_X))
	
	# sort by valueYVar
	# assign ungrouped rank (rank)
	# apply grouping groupXVar and sort by valueYVar within group
	# rank within outcome group (groupRank)
	# View(tmp %>% filter(outcome=="low" & test=="K")) # verify collisions get a random rank
	
	tmp = df %>% inner_join(N_X, by=joinList) %>% mutate(
			y=!!valueYVar,
			x=!!groupXVar)
	
	
	# the knn approach without using neighbours - i.e. a k wide sliding window
	tmp4 = tmp %>% group_by(!!!grps) %>% arrange(y) %>% mutate(rank = row_number())
	tmp4 = tmp4 %>% group_by(!!!grps,x) %>% arrange(y) %>% mutate(
		m_i = lead(rank,as.integer(k_05))-lag(rank,as.integer(k_05))
	) %>% mutate(
		I_i = digamma(N)-digamma(N_X)+0.99*digamma(k_05*2)-digamma(m_i)
	) %>% group_by(!!!grps) %>% summarize(
		I = mean(I_i,na.rm=TRUE)
	)
	return(tmp4)
	 
}






#' calculate mutual information from counts
#'
#' @param x1y1 cooccurrence of x1 and y1 in corpus.
#' @param x1 occurrence of x1 in corpus
#' @param y1 occurrence of y1 in corpus
#' @param total total size of corpus
#' @return the mutual information
#' @export  
miFromCounts = function(x1y1, x1, y1, total) {
	notValid = (x1y1 > x1 | x1y1 > y1 | x1 > total | y1 > total);
	x1y0 = x1-x1y1;
	x0y1 = y1-x1y1;
	p_x1y1 = (as.double(x1y1))/total;
	p_x0y1 = (as.double(x0y1))/total;
	p_x1y0 = (as.double(x1y0))/total;
	return(ifelse(notValid,NaN,
					mi(p_x1y1,p_x0y1,p_x1y0)));
}

#' A calculate pointwise mutual information from counts
#'
#' @param x1y1 cooccurrence of x1 and y1 in corpus.
#' @param x1 occurrence of x1 in corpus
#' @param y1 occurrence of y1 in corpus
#' @param total total size of corpus
#' @return the pointwise mutual information
#' @export
pmiFromCounts = function(x1y1, x1, y1, total) {
    notValid = (x1y1 > x1 | x1y1 > y1 | x1 > total | y1 > total);
    return(ifelse(notValid,NA,pmi(
      (as.double(x1y1))/total,
      (as.double(x1))/total,
      (as.double(y1))/total)));
}

#' A calculate pointwise mutual information from counts
#'
#' @param x1y1 cooccurrence of x1 and y1 in corpus.
#' @param x1 occurrence of x1 in corpus
#' @param y1 occurrence of y1 in corpus
#' @param total total size of corpus
#' @return the normalised pointwise mutual information
#' @export  
npmiFromCounts = function(x1y1, x1, y1, total) {
    notValid = (x1y1 > x1 | x1y1 > y1 | x1 > total | y1 > total);
    return(ifelse(notValid,NA,npmi(
      (as.double(x1y1))/total,
      (as.double(x1))/total,
      (as.double(y1))/total)));
  }

#' A calculate pointwise mutual information from probabilities
#'
#' @param p_xy probability of cooccurrence of x and y.
#' @param p_x probability of occurrence of x
#' @param p_y probability of occurrence of y
#' @return the pointwise mutual information
#' @export 
pmi = function(p_xy, p_x, p_y) {
    notValid = (p_xy < 0.0 | p_xy > 1.0 | p_x < 0.0 | p_x > 1.0 | p_y < 0.0 | p_y > 1.0 | p_xy > p_x + 0.00001 | (p_xy > p_y + 0.00001 ) );
    returnZero = (p_x==0 | p_y==0); 
    returnNegInf = (p_xy == 0 & p_x > 0.0 & p_y > 0.0);
    return(
      ifelse(notValid, NaN,
        ifelse(returnZero,0,
          ifelse(returnNegInf, -Inf,
            log(p_xy/(p_x*p_y))
    ))));
  }

#' A calculate normalised pointwise mutual information from probabilities
#'
#' @param p_xy probability of cooccurrence of x and y.
#' @param p_x probability of occurrence of x
#' @param p_y probability of occurrence of y
#' @return the normalised pointwise mutual information
#' @export 
npmi = function(p_xy, p_x, p_y) {
    notValid = (p_xy < 0.0 | p_xy > 1.0 | p_x < 0.0 | p_x > 1.0 | p_y < 0.0 | p_y > 1.0 | p_xy > p_x + 0.00001 | p_xy > p_y + 0.00001 )
    returnNeg1 = (p_xy == 0.0 & p_x > 0.0 & p_y > 0.0)
    return(
      ifelse(notValid,NaN,
        ifelse(returnNeg1,-1,
          pmi(p_xy,p_x,p_y)/(-log(p_xy))
      )));
  }



#' calculate mutual information from probabilities
#'
#' @param p_x1y1 probability of cooccurrence of x and y.
#' @param p_x0y1 probability of occurrence of y without x
#' @param p_x1y0 probability of occurrence of x without y
#' @return the mutual information
#' @export 
mi = function(p_x1y1, p_x0y1, p_x1y0) {
    notValid = (p_x1y1 < 0.0 | p_x1y1 > 1.0 | p_x0y1 < 0.0 | p_x0y1 > 1.0 | p_x1y0 < 0.0 | p_x1y0 > 1.0 );
    p_x0y0 = 1.0-(p_x1y1+p_x0y1+p_x1y0);
    p_x1 = p_x1y0+p_x1y1;
    p_x0 = 1.0-p_x1;
    p_y1 = p_x1y1+p_x0y1;
    p_y0 = 1.0-p_y1;
    return(ifelse(notValid,NaN,
      (
        ifelse(p_x1y1==0,0,p_x1y1*pmi(p_x1y1,p_x1,p_y1))+
        ifelse(p_x0y1==0,0,p_x0y1*pmi(p_x0y1,p_x0,p_y1))+
        ifelse(p_x1y0==0,0,p_x1y0*pmi(p_x1y0,p_x1,p_y0))+
        ifelse(p_x0y0==0,0,p_x0y0*pmi(p_x0y0,p_x0,p_y0))
      )
    ))
}
