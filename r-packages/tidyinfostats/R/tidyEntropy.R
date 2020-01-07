#' calculate entropy of a sequence of 
#' 
#' @param df - may be grouped, in which case the value is interpreted as different types of continuous variable
#' @param groupVars - the columns that define the discrete subgroups of the data quoted by vars().
#' @param method - the method employed - valid options are "MontgomerySmith", "Histogram", "InfoTheo", "Compression"
#' @param ... - the other parameters are passed onto the implementations
#' @return a single value for the entropy of the vector
calculateEntropy = function(df, groupVars, method, ...) {
  switch (method,
          MontgomerySmith = calculateEntropy_MontgomerySmith(df, {{groupVars}}, ...),
          Histogram = calculateEntropy_Histogram(df, {{groupVars}}, ...),
          InfoTheo = calculateEntropy_InfoTheo(df, {{groupVars}}, ...),
          Compression = calculateEntropy_Compression(df, {{groupVars}}, ...),
          {stop(paste0(method," not a valid option"))}
  )
}

# TODO:
# Entropy of continuous data - e.g. SGolay approach / discretisation
# If there a method involving horizontal visibility?



#' calculate entropy of an optionally ordered discrete value (X) using estimates of entropy from method 2 in
#' 
#' S. Montgomery-Smith and T. Schürmann, “Unbiased Estimators for Entropy and Class Number,” arXiv, 18-Oct-2014. Available: http://arxiv.org/abs/1410.5002
#' 
#' @param df - may be grouped, in which case the grouping is interpreted as different types of discrete variable
#' @param groupVars - the column of the discrete value (X)
#' @param orderingVar - (optional) the column of an ordering variable (e.g. time) - if missing assumes df order,
#' @param j - the width of the interval considered for the estimator. A small number << the smallest sample size
#' @return a dataframe containing the disctinct values of the groups of df, and for each group an entropy value (H). If df was not grouped this will be a single entry
calculateEntropy_MontgomerySmith = function(df, groupVars, orderingVar = NULL, ...) {
  
  # Euler–Mascheroni constant (lambda)
  lambda = 0.577215664901532
  
  grps = df %>% groups()
  # groupVars = ensyms(groupVars)
  
  if (is.null(orderingVar)) {
    # seq here is to fool dbplyr queries into a valid sql syntax as row_number window queries are non-deterministic unless there is some intrinsic ordering.
    tmp = df %>% mutate(seq = 1)
  } else {
    orderingVar = ensym(orderingVar)
    tmp = df %>% rename(seq = !!orderingVar)
  }
  
  # define rank and top level counts
  tmp = tmp %>% group_by(!!!grps) %>% arrange(seq) %>% mutate(
    rank = row_number(),
    N = n(),
    C_x = n_distinct(!!!groupVars)
  )
  
  # define groupwise rank delta and count
  # tmp = tmp %>% group_by(!!!grps, !!!groupVars) %>% arrange(seq) %>% mutate( # would also probably work
  # browser()
  tmp = tmp %>% group_by(!!!grps, !!!groupVars) %>% arrange(rank) %>% mutate(
    k = lead(rank,1,NA)-rank, # this is the N in the paper - j is as mentioned in the paper,
    # TODO: they discuss a esimator be redone for different values of lead and averaged.
    # I don't understand how this doesn't just increase the estimate as the large j is the larger digamma k is.
    #k2 = lead(rank,2,NA)-rank, would need digamma calc & average
    #k3 = lead(rank,3,NA)-rank,
    NX = n()
  ) #%>% filter(!is.na(k))
  
  # digamma(n) = Harmonic(n-1)-lambda
  # Entropy estimated by Harmonic(n-1) = digamma(n)+lambda
  
  # SQL freindly calculation of the digamma function
  digammaTbl = tibble(k = c(1:50), digammak = digamma(c(1:50)))
  tmp = tmp %>% left_join(digammaTbl, by="k",copy=TRUE) %>%
    # left_join(digammaTbl %>% rename(NX=k,digammaNX=digammak), by="NX",copy=TRUE) %>%
    # left_join(digammaTbl %>% rename(C_x=k,digammaC_x=digammak), by="C_x",copy=TRUE) %>%
    mutate(digammak = ifelse(is.na(digammak), log(k-1) + 1/(2*(k-1) - 1/(12*(k-1)^2) ),digammak)) # %>%
    # mutate(digammaNX = ifelse(is.na(digammaNX), log(NX-1) + 1/(2*(NX-1) - 1/(12*(NX-1)^2) ),digammaNX)) %>%
    # mutate(digammaC_x = ifelse(is.na(digammaC_x), log(C_x-1) + 1/(2*(C_x-1) - 1/(12*(C_x-1)^2) ),digammaC_x))
  
  tmp2 = tmp %>% group_by(!!!grps) %>% summarise(
    H = (mean(digammak, na.rm=TRUE) + lambda), #-digammaC_x+log(C_x), na.rm=TRUE) + lambda),
    H_sd = sd(digammak, na.rm=TRUE)/sqrt(max(N)) #-digammaC_x+log(C_x), na.rm=TRUE)
  )  # %>% mutate(
  #  H = H/log(C_x),
  #  H_sd = H_sd/log(C_x) #TODO: making this up
  #)
  # browser()
  return(tmp2)
  
}

#' calculate entropy of an optionally discrete value (X) using a histogram approach
#' 
#' @param df - may be grouped, in which case the grouping is interpreted as different types of discrete variable
#' @param groupVars - the column of the discrete value (X)
#' @param mm - Apply a miller-madow adjustment to the result
#' @return a dataframe containing the disctinct values of the groups of df, and for each group an entropy value (H). If df was not grouped this will be a single entry
calculateEntropy_Histogram = function(df, groupVars, mm=TRUE, ...) {
  grps = df %>% groups()
  # groupVars = ensyms(groupVars)
  
  tmp = df %>% ungroup() %>% group_by(!!!grps) %>% mutate(
    N = n(),
    C_x = n_distinct(!!!groupVars)
  )
  
  tmp2 = tmp %>% ungroup() %>% group_by(!!!grps, !!!groupVars, N, C_x) %>% summarise(
    NX = n()
  ) %>% mutate(
    p_x = NX/N,
    mmAdj = (C_x-1)/(2*N),
    H_x = -p_x*log(p_x) # /log(C_x)
  )
  
  tmp3 = tmp2 %>% ungroup() %>% group_by(!!!grps) %>% summarise(
    H = sum(H_x,na.rm = TRUE)+ifelse(mm,max(mmAdj),0),
    H_sd = NA
  )
  
  return(tmp3 %>% ungroup())
}
 
#' calculate entropy of an optionally discrete value (X) using a infotheo library
#' 
#' @param df - may be grouped, in which case the grouping is interpreted as different types of discrete variable
#' @param groupVars - the column of the discrete value (X)
#' @return a dataframe containing the disctinct values of the groups of df, and for each group an entropy value (H). If df was not grouped this will be a single entry
calculateEntropy_InfoTheo = function(df, groupVars, infoTheoMethod="mm", ...) {
  grps = df %>% groups()
  # groupVars = ensyms(groupVars)
  
  groupsJoin = as.vector(sapply(groupVars,as_label))
  groupIds = df %>% ungroup() %>% select(!!!groupVars) %>% distinct() %>% arrange(!!!groupVars) %>% mutate(
    # raw_x = TODO convert groupVars to integer... Doesn't matter how - ignores groups even, although maybe shouldn't
    x_int = row_number()
  )
  
  tmp = df %>% ungroup() %>% group_by(!!!grps) %>% #mutate(
    #N = n(),
    #C_x = n_distinct(!!!groupVars),
    # raw_x = TODO convert groupVars to integer... Doesn't matter how - ignores groups even, although maybe shouldn't
  #) %>% 
    left_join(groupIds, by = groupsJoin)
  
  tmp2 = tmp %>% ungroup() %>% group_by(!!!grps) %>% group_modify(function(d,...) {
    tibble(
      H = infotheo::entropy(d$x_int, method=infoTheoMethod),
      H_sd = NA
    )
  })
  
  return(tmp2)
}

#' calculate entropy of an optionally discrete value (X) using a infotheo library
#' 
#' @param df - may be grouped, in which case the grouping is interpreted as different types of discrete variable
#' @param groupVars - the column of the discrete value (X)
#' @return a dataframe containing the disctinct values of the groups of df, and for each group an entropy value (H). If df was not grouped this will be a single entry
calculateEntropy_Compression = function(df, groupVars, orderingVar = NULL, ...) {
  grps = df %>% groups()
  # groupVars = ensyms(groupVars)
  if (length(grps)==0) {
    grpsList = NULL
  } else {
    grpsList = sapply(grps,as.character)
  }
  groupsJoin = c(grpsList,as.vector(sapply(groupVars,as_label)))
  
  if (is.null(orderingVar)) {
    # seq here is to fool dbplyr queries into a valid sql syntax as row_number window queries are non-deterministic unless there is some intrinsic ordering.
    tmp = df %>% mutate(tmp_seq=1)
  } else {
    orderingVar = ensym(orderingVar)
    tmp = df %>% mutate(tmp_seq=!!orderingVar)
  }
  
  # convert discrete values of x (defined as combination of groupVars) to integers.
  groupIds = df %>% ungroup() %>% select(!!!grps, !!!groupVars) %>% distinct() %>% group_by(!!!grps) %>% arrange(!!!groupVars) %>% mutate(
    x_int = row_number()
  )
  
  if(max(groupIds$x_int) > 256) stop(paste0("Compression cannot be used on discrete data with more than 256 levels"))
  
  groupIds = groupIds %>% group_by(!!!grps) %>% mutate(
    x_raw = as.raw(x_int-1),
    C_x = n() # the number of non zero classes
  )
  
  tmp = df %>% ungroup() %>% group_by(!!!grps) %>% mutate(
      N = n()
    ) %>% left_join(groupIds, by = groupsJoin)
  
  tmp2 = tmp %>% group_by(!!!grps, C_x, N) %>% group_modify(function(d,g,...) {
    C0 = length(memCompress(as.raw(rep(0,g$N))))
    C1 = C0+g$N/log(g$C_x) #max(sapply(c(1:10), function(i) length(memCompress(as.raw(sample.int(g$C_x,size=g$N,replace=TRUE)-1)))))
    C = length(memCompress(as.vector(d$x_raw)))
    if (C > C1) C=C1 # prevent entropy exceeding theoretical maximum
    H = (C-C0)/(C1-C0) * log(g$C_x) # original paper includes a degrees of freedom parameter here. with this setup this can only be one...?
    # browser()
    return(tibble(
      H = H,
      H_sd = NA
    ))
  })
  
  return(tmp2 %>% ungroup() %>% select(-c(C_x,N)))
}
