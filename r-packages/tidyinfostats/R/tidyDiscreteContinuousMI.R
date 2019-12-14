#' calculate mutual information between a categorical value (X) and a continuous value (Y)
#' 
#' @param df - may be grouped, in which case the value is interpreted as different types of continuous variable
#' @param groupXVar - the column of the categorical value (X)
#' @param valueYVar - the column of the continuous value (Y)
#' @param method - the method employed - valid options are "KWindow","SGolay","Discrete". At the moment only "kwindow" supported for dbplyr tables
#' @param ... - the other parameters are passed onto the implementations
#' @return a dataframe containing the disctinct values of the groups of df, and for each group a mutual information column (I). If df was not grouped this will be a single entry
calculateDiscreteContinuousMI = function(df, groupXVar, valueYVar, method="KWindow", ...) {
  switch (method,
    KWindow = calculateDiscreteContinuousMI_KWindow(df, {{groupXVar}}, {{valueYVar}}, ...),
    SGolay = calculateDiscreteContinuousMI_SGolay(df, {{groupXVar}}, {{valueYVar}}, ...),
    Discrete = calculateDiscreteContinuousMI_Discretise(df, {{groupXVar}}, {{valueYVar}}, ...),
  )
}

#' calculate mutual information between a categorical value (X) and a continuous value (Y)
#' 
#' @param df - may be grouped, in which case the value is interpreted as different types of continuous variable
#' @param groupXVar - the column of the categorical value (X)
#' @param valueYVar - the column of the continuous value (Y)
#' @param k_05 - the half window width of the SG filter that smooths the data. This is dependent on data but typically not less that 10.
#' @return a dataframe containing the disctinct values of the groups of df, and for each group a mutual information column (I). If df was not grouped this will be a single entry
#' @import dplyr
calculateDiscreteContinuousMI_SGolay = function(df, groupXVar, valueYVar, k_05=10) {
  grps = df %>% groups()
  groupXVar = ensym(groupXVar)
  valueYVar = ensym(valueYVar)

  if (identical(grps,NULL)) {
    grpsList = NULL
  } else {
    grpsList = sapply(grps,as.character)
  }
  joinList = c(grpsList, as.character(groupXVar))
  
  # N.B. this whole bit of code is confusing because groups mean 2 things here - the 
  # different types of Y (grps) which should be preserved and the categorical X 
  # NX has group counts (N) and subgroup counts (N_X).
  N_X = df %>% group_by(!!!grps,!!groupXVar) %>% summarise(N_X = n()) %>% group_by(!!!grps) %>% mutate(N = sum(N_X))
  
  # sort by valueYVar
  # assign ungrouped rank (rank)
  # apply grouping groupXVar and sort by valueYVar within group
  # rank within outcome group (groupRank)
  # View(tmp %>% filter(outcome=="low" & test=="K")) # verify collisions get a random rank
  
  tmp = df %>% inner_join(N_X, by=joinList) %>% mutate(
    y=!!valueYVar,
    x=!!groupXVar)
  # get the group wise pdfs (i.e. p_y_given_x) at every observation point in class X
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
        # N.B. correction factor seems unnecessary
        #	) %>% mutate(
        #			corr = (p_y_given_x+lag(p_y_given_x,1,default=0))*(y-lag(y))/2
        #	) %>% mutate(
        #			p_y_given_x = p_y_given_x/sum(corr, na.rm=TRUE)
        #	) %>% select(
        #			-corr,
      )
    }
  )
  
  # get the overall pdf for the combination of all x's (gives us p_y)	at all observation points
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
        # N.B. correction factor seems unnecessary
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
  # points are not evenly spaced in y dimension so piecewise integration
  tmp3 = tmp2 %>% group_by(!!!grps,x) %>% arrange(y) %>% mutate(
    # integrate over dy ( well does the trapeziod part of the integration
    d_I_d_xy = (pmi_xy+lag(pmi_xy,1,default=0))*(y-lag(y))/2
  ) %>% group_by(!!!grps) %>% summarise (
    I = sum(d_I_d_xy,na.rm=TRUE)
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
#' B. C. Ross, “Mutual information between discrete and continuous data sets,” PLoS One, vol. 9, no. 2, p. e87357, Feb. 2014 [Online]. Available: http://dx.doi.org/10.1371/journal.pone.0087357
#' but with the important simplification of using the sliding window K elements wide rather than the k nearest neighbours. This is empirically shown to have little difference on larger datasets
#' and makes this algorithm simple to implement in dbplyr tables.
#' 
#' @param df - may be grouped, in which case the value is interpreted as different types of continuous variable
#' @param groupXVar - the column of the categorical value (X)
#' @param valueYVar - the column of the continuous value (Y)
#' @param k_05 - half the sliding window width - this should be a small number like 1,2,3.
#' @param a - adjustment parameter 1 (adjusts k digamma term - constant)
#' @param b - adjustment parameter 2 (adjusts m_i digamma term - varies with MI)
#' @return a dataframe containing the disctinct values of the groups of df, and for each group a mutual information column (I). If df was not grouped this will be a single entry
#' @import dplyr
calculateDiscreteContinuousMI_KWindow = function(df, groupXVar, valueYVar, k_05=2,a=1,b=1) {
  grps = df %>% groups()
  groupXVar = ensym(groupXVar)
  valueYVar = ensym(valueYVar)
  
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
  
  tmp = df %>% inner_join(N_X, by=joinList) %>% mutate(
    y=!!valueYVar,
    x=!!groupXVar)
  
  # the knn approach without using neighbours - i.e. a k wide sliding window
  tmp4 = tmp %>% group_by(!!!grps) %>% arrange(y) %>% mutate(rank = row_number())
  tmp4 = tmp4 %>% group_by(!!!grps,x) %>% arrange(y) %>% mutate(
    m_i = lead(rank,as.integer(k_05))-lag(rank,as.integer(k_05))
  ) 
  if ("tbl_sql" %in% class(tmp4)) {
    # estimate digamma in SQL
    # for large n digamma(n) is approx ln(n-1)+1/(2*(n-1))
    digammaTbl = digammaTable(tmp4$src$con)
    tmp4 = tmp4 %>% 
      left_join(digammaTbl %>% rename(N = n, digammaN = digamma), by="N") %>%
      left_join(digammaTbl %>% rename(N_X = n, digammaN_X = digamma), by="N_X") %>%
      left_join(digammaTbl %>% rename(m_i = n, digammam_i = digamma), by="m_i") %>%
      mutate(
        digammaN = ifelse(is.na(digammaN), log(N-1)+1/(2*(N-1)),digammaN),
        digammaN_X = ifelse(is.na(digammaN_X), log(N_X-1)+1/(2*(N_X-1)),digammaN_X)
      ) %>%
      mutate(
        I_i = digammaN-digammaN_X+local(a*digamma(k_05*2))-local(b)*digammam_i
      )
  } else {
    tmp4 = tmp4 %>% mutate(
      I_i = digamma(N)-digamma(N_X)+a*digamma(k_05*2)-b*digamma(m_i)
    )
  }
  tmp4 = tmp4 %>% group_by(!!!grps) %>% summarize(
    I = mean(I_i,na.rm=TRUE)
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
calculateDiscreteContinuousMI_Discretise = function(df, groupXVar, valueYVar, bins=nrow(df)^(1/3)) {
  groupXVar = ensym(groupXVar)
  valueYVar = ensym(valueYVar)
  return(
    df %>% group_modify(function(d,...) {
        d = d %>% mutate(x = as.integer(as.factor(!!groupXVar))) %>% select(x,y = !!valueYVar) %>% infotheo::discretize(n=bins)
        return(data.frame(I=infotheo::mutinformation(d$x, d$y)))
    })
  )
}

digammaTable = function(con) {
  if(con %>% db_has_table("digamma")) return(tbl(con,"digamma"))
  digammaTbl = tibble(n = c(1:10000),
    digamma = digamma(c(1:10000))
  )
  return(con %>% copy_to(digammaTbl,name="digamma"))
}

harmonicTable = function(con) {
  if(con %>% db_has_table("harmonic")) return(tbl(con,"harmonic"))
  harmonicTbl = tibble(n = c(1:10000), harmonic=c(0,1/c(1:9999))) %>% mutate(harmonic = cumsum(harmonic))
  return(con %>% copy_to(digammaTbl,name="digamma"))
}
