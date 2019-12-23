#' calculate mutual information between a categorical value (X) and a continuous value (Y)
#' 
#' @param df - may be grouped, in which case the value is interpreted as different types of continuous variable
#' @param groupXVar - the column of the categorical value (X)
#' @param valueYVar - the column of the continuous value (Y)
#' @param method - the method employed - valid options are "KWindow", "KNN","SGolay","Discrete". At the moment "SGolay" not supported for dbplyr tables
#' @param ... - the other parameters are passed onto the implementations
#' @return a dataframe containing the disctinct values of the groups of df, and for each group a mutual information column (I). If df was not grouped this will be a single entry
calculateDiscreteContinuousMI = function(df, groupXVar, valueYVar, method="KWindow", ...) {
  switch (method,
    KWindow = calculateDiscreteContinuousMI_KWindow(df, {{groupXVar}}, {{valueYVar}}, ...),
    KNN = calculateDiscreteContinuousMI_KNN(df, {{groupXVar}}, {{valueYVar}}, ...),
    SGolay = calculateDiscreteContinuousMI_SGolay(df, {{groupXVar}}, {{valueYVar}}, ...),
    Discrete = calculateDiscreteContinuousMI_Discretise(df, {{groupXVar}}, {{valueYVar}}, ...),
  )
}

#' calculate mutual information between a categorical value (X) and a continuous value (Y)
#' 
#' @param df - may be grouped, in which case the value is interpreted as different types of continuous variable
#' @param groupVar - the column of the categorical value (X)
#' @param valueVar - the column of the continuous value (Y)
#' @param k_05 - the half window width of the SG filter that smooths the data. This is dependent on data but typically not less that 10.
#' @return a dataframe containing the disctinct values of the groups of df, and for each group a mutual information column (I). If df was not grouped this will be a single entry
#' @import dplyr
calculateDiscreteContinuousMI_SGolay = function(df, discreteVar, continuousVar, k_05=10, minSize=30) {
  grps = df %>% groups()
  discreteVar = ensym(discreteVar)
  continuousVar = ensym(continuousVar)

  if (length(grps)==0) {
    grpsList = NULL
  } else {
    grpsList = sapply(grps,as.character)
  }
  joinList = c(grpsList, as.character(discreteVar))
  
  # N.B. this whole bit of code is confusing because groups mean 2 things here - the 
  # different types of Y (grps) which should be preserved and the discrete 
  # NX has group counts (N) and subgroup counts (N_X).
  groupSize = df %>% group_by(!!!grps,!!discreteVar) %>% summarise(N_X = n()) %>% group_by(!!!grps) %>% mutate(N = sum(N_X))
  
  # if (min(groupSize$N_X) < k_05*2+1) {
  #   return(df %>% group_by(!!!grps) %>% summarise(I = NA))
  # }
  
  # sort by valueYVar
  # assign ungrouped rank (rank)
  # apply grouping groupXVar and sort by valueYVar within group
  # rank within outcome group (groupRank)
  # View(tmp %>% filter(outcome=="low" & test=="K")) # verify collisions get a random rank
  
  tmp = df %>% inner_join(groupSize, by=joinList) %>% mutate(
    y_continuous=!!continuousVar,
    x_discrete=!!discreteVar)
  # get the group wise pdfs (i.e. p_y_given_x) at every observation point in class X
  # process with X grouped and ordered by Y within groups
  # value of Y wrt rank of Y is an inverse CDF function. This is evenly spaced on rank of Y
  # spacing of rank depends on the number of observations (and actually we don't need the value)
  # gradient of this is inverse of PDF. This needs heavy smoothing to be useable subsequently_
  
  
  tmp2 = tmp %>%  group_by(!!!grps,x_discrete) %>% arrange(y_continuous) %>% group_modify(
    function(d,...) {
      k = k_05*2+1
      samples = min(d$N_X)
      #if (k >= samples) k = samples-samples%%2-1
      if (k < samples && samples > minSize) {
        return(
          tibble(
            N_X = d$N_X,
            N = d$N,
            y_continuous = d$y_continuous,
            d_xy_d_r = signal::sgolayfilt(d$y_continuous, p=2, n=k, m=1, ts=1/samples)
          ) %>% mutate(
            p_y_given_x = 1/d_xy_d_r
          )
        )
      } else {
        return(
          tibble(
            N_X = d$N_X,
            N = d$N,
            y_continuous = d$y_continuous,
            d_xy_d_r = rep(NA,length(d$y_continuous)), 
            p_y_given_x = rep(NA,length(d$y_continuous)) 
          )
        )
      }
    }
  )
  
  # get the overall pdf for the combination of all x's (gives us p_y)	at all observation points
  tmp2 = tmp2 %>%  group_by(!!!grps) %>% arrange(y_continuous) %>% group_modify(
    function(d,...) {
      k = k_05*2+1
      samples = min(d$N)
      # if (k >= samples) k = samples-samples%%2-1
      if (k < samples && samples > minSize) {
        return(
          tibble(
            N_X=d$N_X,
            N = d$N,
            x_discrete = d$x_discrete,
            y_continuous = d$y_continuous,
            p_y_given_x = d$p_y_given_x,
            d_xy_d_r = d$d_xy_d_r,
            d_y_d_r = signal::sgolayfilt(d$y_continuous, p=2, n=k, m=1, ts=1/samples)
          ) %>% mutate(
            p_y = 1/d_y_d_r,
            p_x = N_X/N
          ) %>% mutate(
            pmi_xy = p_x*p_y_given_x*log(p_y_given_x/p_y)
          )
        )
      } else {
        return(
          tibble(
            N_X=d$N_X,
            N = d$N,
            x_discrete = d$x_discrete,
            y_continuous = d$y_continuous,
            p_y_given_x = d$p_y_given_x,
            d_xy_d_r = d$d_xy_d_r,
            d_y_d_r = rep(NA,length(d$y_continuous)),
            p_y = rep(NA,length(d$y_continuous)),
            p_x = N_X/N,
            pmi_xy = rep(NA,length(d$y_continuous)) 
          )
        )
      }
    }
  )
  
  # do the integration
  # points are not evenly spaced in y dimension so piecewise integration
  tmp3 = tmp2 %>% group_by(!!!grps,x_discrete) %>% arrange(y_continuous) %>% mutate(
    # integrate over dy ( well does the trapeziod part of the integration
    d_I_d_xy = (pmi_xy+lag(pmi_xy,1,default=0))*(y_continuous-lag(y_continuous))/2
  ) %>% group_by(!!!grps) %>% summarise (
    min_N_X = min(N_X),
    I = sum(d_I_d_xy,na.rm=TRUE),
  ) %>% mutate(
    I = ifelse(min_N_X < minSize,NA,I)
  )
  
  
  
  return(tmp3)
}

#### this bit of code defunct but here to remind me how to implement SGolay filter in 
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

# coefficients: signal::sgolay(p=2,n=5,m=1)[3,]


#' calculate mutual information between a categorical value (X) and a continuous value (Y) using a sliding window and local entropy measure
#' 
#' This is based on the technique described here:
#' B. C. Ross, “Mutual information between discrete and continuous data sets,” PLoS One, vol. 9, no. 2, p. e87357, Feb. 2014 [Online]. Available: http://dx_doi.org/10.1371/journal.pone.0087357
#' but with the important simplification of using the sliding window K elements wide rather than the k nearest neighbours. This is empirically shown to have little difference on larger datasets
#' and makes this algorithm simple to implement in dbplyr tables.
#' 
#' @param df - may be grouped, in which case the value is interpreted as different types of continuous variable
#' @param groupXVar - the column of the categorical value (X)
#' @param valueYVar - the column of the continuous value (Y)
#' @param k_05 - half the sliding window width - this should be a small number like 1,2,3.
#' @param a - adjustment parameter 1 (adjusts k digamma term - constant)
#' @return a dataframe containing the disctinct values of the groups of df, and for each group a mutual information column (I). If df was not grouped this will be a single entry
#' @import dplyr
calculateDiscreteContinuousMI_KWindow = function(df, discreteVar, continuousVar, k_05=4, a=1) { #a=0.992, b=1) {
  k_05 = as.integer(k_05)
  if (k_05<2) k_05=2
  grps = df %>% groups()
  discreteVar = ensym(discreteVar)
  continuousVar = ensym(continuousVar)
  
  if (length(grps)==0) {
    grpsList = NULL
  } else {
    grpsList = sapply(grps,as.character)
  }
  joinList = c(grpsList, as.character(discreteVar))
  
  df = df %>% select(!!!grps,!!discreteVar,!!continuousVar)
  # this is confusing because groups mean 2 things here - the 
  # different types of Y (grps) which should be preserved and the categorical X 
  # NX has group counts (N) and subgroup counts (N_X) 
  grpCounts = df %>% group_by(!!!grps,!!discreteVar) %>% summarise(N_X = n()) %>% group_by(!!!grps) %>% mutate(N = sum(N_X)) %>% compute()
  
  tmp = df %>% inner_join(grpCounts, by=joinList) %>% mutate(
    x_discrete=!!discreteVar,
    y_continuous=!!continuousVar)
  
  # the knn approach without using neighbours - i.e. a k wide sliding window
  tmp4 = tmp %>% group_by(!!!grps) %>% arrange(y_continuous) %>% mutate(rank = row_number())
  tmp4 = tmp4 %>% group_by(!!!grps,x_discrete) %>% arrange(y_continuous) %>% mutate(
    
    # correct k for tails of distributions exclusive
    # kRank = row_number(),
    # m_i = lead(rank,n=k_05,default=max(N))-lag(rank,n=k_05,default=1)+1L,
    # k = lead(kRank,n=k_05,default=max(N_X))-lag(kRank,n=k_05,default=1)+1L
    
    # correct k for tails of distributions inclusive
    # kRank = row_number(),
    # m_i = lead(rank,n=k_05,default=max(N))-lag(rank,n=k_05,default=1),
    # k = lead(kRank,n=k_05,default=max(N_X))-lag(kRank,n=k_05,default=1)
    
    # dont correct k & exclude tails
    k = k_05*2,
    m_i = lead(rank,n=k_05)-lag(rank,n=k_05)
    
    # average m_i over 3 window sizes
    # k = k_05*2,
    # m_i = floor((
	  #				lead(rank,n=local(k_05))-lag(rank,n=local(k_05))+
	  #				lead(rank,n=local(k_05+1L))-lag(rank,n=local(k_05+1L))+
	  #				lead(rank,n=local(k_05-1L))-lag(rank,n=local(k_05-1L))
	  #				)/3*2)/2
  )  %>% compute()
  if ("tbl_sql" %in% class(tmp4)) {
    # estimate digamma in SQL
    # for large n digamma(n) is approx ln(n-1)+1/(2*(n-1))
    digammaTbl = tibble(n = c(1:1000), digamma = digamma(c(1:1000)))
	  tmp4 = tmp4 %>% 
      left_join(digammaTbl %>% rename(k = n, digammak = digamma), by="k",copy=TRUE) %>%
      left_join(digammaTbl %>% rename(N = n, digammaN = digamma), by="N",copy=TRUE) %>%
      left_join(digammaTbl %>% rename(N_X = n, digammaN_X = digamma), by="N_X",copy=TRUE) %>%
      left_join(digammaTbl %>% rename(m_i = n, digammam_i = digamma), by="m_i",copy=TRUE) %>%
      mutate(
        digammaN = ifelse(is.na(digammaN), log(N-1)+1/(2*(N-1)),digammaN),
        digammaN_X = ifelse(is.na(digammaN_X), log(N_X-1)+1/(2*(N_X-1)),digammaN_X)
      ) %>%
      mutate(
        I_i = digammaN-digammaN_X+local(a)*digammak-digammam_i
      )
  } else {
    tmp4 = tmp4 %>% mutate(
      I_i = digamma(N)-digamma(N_X)+a*digamma(k)-digamma(m_i)
    )
  }
  tmp4 = tmp4 %>% filter(!is.na(I_i)) %>% group_by(!!!grps) %>% summarize(
      I = mean(I_i)
  )
  return(tmp4)
}

#' calculate mutual information between a categorical value (X) and a continuous value (Y) using a discretisation and infotheo
#' 
#' @param df - may be grouped, in which case the value is interpreted as different types of continuous variable
#' @param groupXVar - the column of the categorical value (X)
#' @param valueYVar - the column of the continuous value (Y)
#' @param bins - the number of bins (see infotheo::discretize)
#' @return a dataframe containing the disctinct values of the groups of df, and for each group a mutual information column (I). If df was not grouped this will be a single entry
#' @import dplyr
calculateDiscreteContinuousMI_Discretise = function(df, discreteVar, continuousVar, bins=nrow(df)^(1/3)) {
  discreteVar = ensym(discreteVar)
  continuousVar = ensym(continuousVar)
  grps = df %>% groups()
  
  df = df %>% group_by(!!!grps) %>% arrange(!!continuousVar) %>% mutate(y_discrete = ntile(n=bins), x_discrete = !!discreteVar)
  return(df %>% probabilitiesFromGroups(x_discrete, y_discrete) %>% calculateMultiClassMI())
  
  #return(
  #  df %>% group_modify(function(d,...) {
  #      d = d %>% mutate(x_discrete = as.integer(as.factor(!!discreteVar))) %>% select(x_discrete, y_continuous = !!continuousVar) %>% infotheo::discretize(n=bins)
  #     return(data.frame(I=infotheo::mutinformation(d$x_discrete, d$y_continuous)))
  # })
  #)
}

digammaTable = function(con) {
	return(tibble(n = c(1:300), digamma = digamma(c(1:300)) ))
  #if(con %>% db_has_table("digamma") || con %>% db_has_table("##digamma")) return(tbl(con,"digamma"))
  #digammaTbl = tibble(n = c(1:1000), digamma = digamma(c(1:1000)) )
  #return(con %>% copy_to(digammaTbl,name="digamma", overwrite=TRUE))
}

#### ----

#' calculate mutual information between a categorical value (X) and a continuous value (Y) using a sliding window and local entropy measure
#' 
#' This is based on the technique described here:
#' B. C. Ross, “Mutual information between discrete and continuous data sets,” PLoS One, vol. 9, no. 2, p. e87357, Feb. 2014 [Online]. Available: http://dx_doi.org/10.1371/journal.pone.0087357
#' but with the important simplification of using the sliding window K elements wide rather than the k nearest neighbours. This is empirically shown to have little difference on larger datasets
#' and makes this algorithm simple to implement in dbplyr tables.
#' 
#' @param df - may be grouped, in which case the value is interpreted as different types of continuous variable
#' @param groupXVar - the column of the categorical value (X)
#' @param valueYVar - the column of the continuous value (Y)
#' @param k_05 - half the sliding window width - this should be a small number like 1,2,3.
#' @param a - adjustment parameter 1 (adjusts k digamma term - constant)
#' @return a dataframe containing the disctinct values of the groups of df, and for each group a mutual information column (I). If df was not grouped this will be a single entry
#' @import dplyr
calculateDiscreteContinuousMI_KNN = function(df, discreteVar, continuousVar, k_05=4, a=1) { #a=0.992, b=1) {
  k_05 = as.integer(k_05)
  if (k_05<2) k_05=2
  grps = df %>% groups()
  discreteVar = ensym(discreteVar)
  continuousVar = ensym(continuousVar)
  
  if (length(grps)==0) {
    grpsList = NULL
  } else {
    grpsList = sapply(grps,as.character)
  }
  joinList = c(grpsList, as.character(discreteVar))
  
  df = df %>% select(!!!grps,!!discreteVar,!!continuousVar)
  # this is confusing because groups mean 2 things here - the 
  # different types of Y (grps) which should be preserved and the categorical X 
  # NX has group counts (N) and subgroup counts (N_X) 
  grpCounts = df %>% group_by(!!!grps,!!discreteVar) %>% summarise(N_X = n()) %>% group_by(!!!grps) %>% mutate(N = sum(N_X)) %>% compute()
  
  # totalN = df %>% group_by(!!!grps) %>% count() %>% rename(totalN = n)
  
  #if (min(grpCounts$N_X) < k_05*2+1) {
  #  return(df %>% group_by(!!!grps) %>% summarise(I = NA))
  #}
  
  tmp = df %>% inner_join(grpCounts, by=joinList) %>% mutate(
    x_discrete=!!discreteVar,
    y_continuous=!!continuousVar)
  
  tmp = tmp %>% group_by(!!!grps) %>% arrange(y_continuous) %>% mutate(rank = row_number()) %>% 
    group_by(!!!grps,x_discrete) %>% arrange(y_continuous) %>% mutate(groupRank = row_number()) %>% compute()
  
  join2List = c(grpsList, "join")
  
  tmp_join = tmp %>% mutate(join=1) %>% rename(
    y_continuous_knn = y_continuous, 
    x_discrete_knn = x_discrete) %>%
    select(!!!grps,join,y_continuous_knn,x_discrete_knn) %>% compute()
  
  # TODO: this is unuseably inefficient
  
  tmp4 = tmp %>% mutate(join=1) %>% inner_join(tmp_join, by=join2List) %>% 
    mutate(y_diff = abs(y_continuous - y_continuous_knn)) %>% 
    group_by(!!!grps,rank) %>% 
    arrange(y_diff) %>% 
    mutate(sameGroup=ifelse(x_discrete_knn==x_discrete,1L,0L)) %>% #, differentGroup=ifelse(x_discrete_knn==x_discrete,0,1)) %>%
    mutate(kDist = cumsum(sameGroup), m_i = row_number()) %>%
    filter(kDist == local(k_05*2L+1L) & sameGroup==1) %>%
    rename(k = kDist) %>% compute()
  
  
  if ("tbl_sql" %in% class(tmp4)) {
    # estimate digamma in SQL
    # for large n digamma(n) is approx ln(n-1)+1/(2*(n-1))
    digammaTbl = tibble(n = c(1:1000), digamma = digamma(c(1:1000)))
    # TODO: how do we cache this properly?    digammaTbl = digammaTable(tmp4$src$con)
    tmp4 = tmp4 %>% 
      left_join(digammaTbl %>% rename(k = n, digammak = digamma), by="k",copy=TRUE) %>%
      left_join(digammaTbl %>% rename(N = n, digammaN = digamma), by="N",copy=TRUE) %>%
      left_join(digammaTbl %>% rename(N_X = n, digammaN_X = digamma), by="N_X",copy=TRUE) %>%
      left_join(digammaTbl %>% rename(m_i = n, digammam_i = digamma), by="m_i",copy=TRUE) %>%
      mutate(
        digammaN = ifelse(is.na(digammaN), log(N-1)+1/(2*(N-1)),digammaN),
        digammaN_X = ifelse(is.na(digammaN_X), log(N_X-1)+1/(2*(N_X-1)),digammaN_X)
      ) %>%
      mutate(
        I_i = digammaN-digammaN_X+local(a)*digammak-digammam_i
      )
  } else {
    tmp4 = tmp4 %>% mutate(
      I_i = digamma(N)-digamma(N_X)+a*digamma(k)-digamma(m_i)
    )
  }
  tmp4 = tmp4 %>% filter(!is.na(I_i)) %>% group_by(!!!grps) %>% summarize(
    # I = mean(I_i)
    I = mean(I_i)
  )
  return(tmp4)
}


# harmonicTable = function(con) {
#   if(con %>% db_has_table("harmonic")) return(tbl(con,"harmonic"))
#   harmonicTbl = tibble(n = c(1:10000), harmonic=c(0,1/c(1:9999))) %>% mutate(harmonic = cumsum(harmonic))
#   return(con %>% copy_to(digammaTbl,name="digamma"))
# }
