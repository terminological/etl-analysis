#' Get an of the omop databse analyser
#'
#' @keywords omop
#' @import dplyr
#' @export
Analyser = R6::R6Class("Analyser", public=list(

  #### Fields (TODO: make private?) ----
  #' @field omop the omop connection
  omop = NULL,
  #' @field groupedDf a grouped dataframe including a grouping representing the documents, distinct concepts in the groups and counts of concepts in document
  groupedDf = NULL,
  #' @field grps the original grouping of the data - corresponds to a document
  grps = list(),
  #' @field nodes a dataframe including unique concept_ids, overall counts and count of documents containing
  nodes = NULL,
  #' @field edges a dataframe of shortest ancestor edges between nodes
  edges = NULL,
  #' @field totalDocuments a count of all the docuemnt level groupings in the analyser
  totalDocuments = NULL,
  

  #### Methods ----
  #' @description create a concept analyser - typically not designed to be called directly. See Analyser$fromSearcher and Analyser$fromDataframe
  #' @param omop an R6 Omop object
  #' @param field the field name of the concept_id
  #' @param expanded has the data already been expanded to include ancestors
  #' @param groupedDf a data frame including at least a concept_id plus the groupings defining documents, and ideally counts
  initialize = function( omop, groupedDf, field  ) {
    self$omop = omop;
    # create a count column if none exists in our input groupedDf
    if (!"count" %in% colnames(groupedDf)) {  groupedDf = groupedDf %>% mutate(count= 1L)   }
    if (!"isOrigin" %in% colnames(groupedDf)) {  groupedDf = groupedDf %>% mutate(isOrigin = as.logical(1L)) }
    tmpfield = ensym(field)
    grps = groupedDf %>% groups()
    self$grps = grps
    # get the distinct concept_ids and counts in each group of the groupedDf 
    # this may have already happened upstream
    self$groupedDf = groupedDf %>% mutate(concept_id = !!tmpfield) %>% group_by(!!!grps,concept_id) %>% summarise(count = sum(count, na.rm=TRUE)) %>% compute()
    self$totalDocuments = groupedDf %>% select(!!!grps) %>% distinct() %>% ungroup() %>% summarise(N=n()) %>% pull(N)
  },

  #' @description creates the nodes field containing IDF information
  #' 
  #' TODO: save and restore IDFs based on different corpuses
  #' TODO: calculate IDFs of ancestor frequencies based on child frequencies (if possible)
  calculateIdfNodes = function() {
    # nodes are the unique concepts accross all groups - these are counted to give us:
    # count = number of occurrences in corpus
    # documents = number of documents that each concept appears in (useful for idf)
    self$nodes = self$groupedDf %>% ungroup() %>% 
      group_by(concept_id) %>% #,isOrigin) %>% 
      # f_t - is the number of instances of a given term in the corpus
      # N_t - is number of documents containing given term in the corpus 
      summarise(
		concept_name = first(concept_name),
        f_t=sum(count, na.rm=TRUE), 
        N_t=n()
      ) %>% ungroup() %>%
      mutate(
        id = rank(concept_id)
      ) %>%
      # max_N_t = max number of documents containing any given single term in corpus (term that appears in most documents)
      # max_f_t = max number of any given term occurrences in in corpus (term that appears in most documents)
      # N = total number of documents in the corpus
      mutate(
        max_N_t = max(N_t, na.rm=TRUE),
        max_f_t = max(f_t, na.rm=TRUE),
        N = local(self$totalDocuments)
      ) %>% compute(unique_index="concept_id")
    invisible(self)
  },
  
  #' @description build the nodes and links using the ancestor table and shortest paths
  calculateGraph = function() {
    if(is.null(self$nodes)) { self$calculateIdfNodes() }
    nodes = self$nodes
    omop = self$omop
    self$edges = omop$concept_ancestor %>%
      inner_join(nodes %>% select(descendant_concept_id=concept_id,from=id), by="descendant_concept_id") %>%
      inner_join(nodes %>% select(ancestor_concept_id=concept_id,to=id), by="ancestor_concept_id") %>%
      # group_by(ancestor_concept_id,descendant_concept_id) %>%
      filter( # equiv of having clause - shortest paths
        # min_levels_of_separation == min(min_levels_of_separation, na.rm=TRUE)
        # & min_levels_of_separation > 0
        min_levels_of_separation == 1
      ) %>% compute()
    invisible(self)
  },
  
  #' @description define document aggregation grouping post hoc. usually better to group data before importing
  #' @return the analyser 
  redefineGroups = function(...) {
    s = ensyms(...)
    s = s[as.character(s) %in% colnames(self$groupedDf)]
    self$grps = s
    self$groupedDf %>% group_by(!!!s)
    self$totalDocuments = self$groupedDf %>% select(!!!grps) %>% distinct() %>% ungroup() %>% summarise(N=n()) %>% pull(N)
    self$omop$tidyTempTable(self$nodes)
    self$nodes = NULL
    self$omop$tidyTempTable(self$edges)
    self$edges = NULL
    invisible(self)
  },
  # N.B. this doesn't work as groupedDf is modified on import by aggregating on groups. Counts will be incorrect.
  
  #' @description get ancestors for a grouped set of concepts preserving grouping and count information
  #' @return the analyser with expanded groupedDf including extra concepts
  calculateAncestorConcepts = function() {
    omop = self$omop
    groupedDf = self$groupedDf
    grps = self$grps
    # first get the concepts by group in groupedDf - i.e. concepts in each document and sum counts
    # this gives us the freqency of a concept_id in a document
    tmp = groupedDf %>% rename(descendant_concept_id=concept_id) %>% compute()
    # expand to the ancestor table.
    tmp2 = omop$concept_ancestor %>%
      inner_join(tmp, by="descendant_concept_id", copy=TRUE) %>%
      rename(concept_id=ancestor_concept_id) %>%
      # reapply the grouping as the columns have changed name
      # this will include the original concepts (as they are 1:1 mapped in the ancestor table)
      # as well as probably multiple copies of all the ancestors to root
      ungroup %>% group_by(!!!grps,concept_id) %>%
      # when more than one concept within a document matches the ancestor we sum the count
      summarise(
        count = sum(count, na.rm = TRUE) #, isOrigin not working here and seems to introduce duplicates
        # isOrigin = ifelse(min(min_levels_of_separation, na.rm=TRUE)==0,as.logical(1L),as.logical(0L)) # was this part of the orinal groupedDf?
      ) %>% compute()
    #the searcher here gives us a standard filtered list of concepts
    search = Searcher$new(omop)
    self$groupedDf = search$toDataframe() %>% select(concept_id,concept_name) %>% inner_join(tmp2, by="concept_id") %>% compute()
    omop$tidyTempTable(self$nodes)
    self$nodes = NULL
    omop$tidyTempTable(self$edges)
    self$edges = NULL
    omop$tidyTempTable(groupedDf)
    omop$tidyTempTable(tmp)
    omop$tidyTempTable(tmp2)
    invisible(self)
  },
  
  #' @description caluculates a mutual information table for the outcomes given in outcomeVar
  #' @details
  #' The grouped dataframe here acts as a "Document" from the perpective of the TFIDF calculation but might be a person
  #' 
  #' @param outcomeVar the outcome 
  #' @return an analysis object with the nodes expanded to include a MI score for each concept assuming a binary outcome
  #' and an expanded groupedDf which includes the normalised pointwise mutual information stats for each concept in each outcome
  calculateOutcomeMI = function(outcomeVar) {
    if(is.null(self$nodes)) { self$calculateIdfNodes() }
    outcomeVar = ensym(outcomeVar)
    grps = self$grps
    docGrps = grps[grps != outcomeVar]
    
    groupedDf = self$groupedDf %>% ungroup() 
    
    # count documents by outcome
    byDocsAndOutcome = groupedDf %>% select(!!!docGrps,!!outcomeVar) %>% distinct()
    byOutcome = byDocsAndOutcome %>% group_by(!!outcomeVar) %>% summarise(docsWithOutcome = n())
    
    # count documents by concept 
    byDocsAndConcept = groupedDf %>% select(!!!docGrps,concept_id, concept_name) %>% distinct()
    byConcept = byDocsAndConcept %>% group_by(concept_id, concept_name) %>% summarise(docsWithTerm = n())
    
    # count documents by intersection of outcome and concept
    byDocsAndConceptAndOutcome = groupedDf %>% select(!!!docGrps,!!outcomeVar,concept_id) %>% distinct()
    byConceptAndOutcome = byDocsAndConceptAndOutcome %>% group_by(!!outcomeVar,concept_id) %>% summarise(docsWithTermAndOutcome = n())
    
    # TODO: In theory this can be done in SQL with a SELECT * FROM byConcept,byOutcome 
    # May need to do this if performance an issue, however little point unless we can convert PMI calculation to SQL
    
    # untested code to generate full combination set in database:
    # result = byOutcome %>% mutate(j=1) %>% 
    #   inner_join(byConcept %>% mutate(j=1), by="j") %>% 
    #   select(-j) %>%
    #   outer_join(byConceptAndOutcome, by=c("concept_id",as.character(outcomeVar))) %>%
    #   mutate(
    #     docsWithTermAndOutcome = ifelse(is.na(docsWithTermAndOutcome),0,docsWithTermAndOutcome),
    #     allDocs = self$totalDocuments
    #   )
    
    # fill in missing combinations of concept and outcome
    # In 2 class outcomes actually we can do this with just one of the combinations as MI is symmetrical but npmi is not
    # e.g. byConceptAndOutcome %>% left_join(byConcept) %>% left_join(byOutcome) %>% group_by(concept_id) %>% filter(first i.e. any one) 
    # TODO: mi calculation in SQL is not trivial - hence in R
    tmp = byConceptAndOutcome %>% collect() %>% ungroup()
    expandedByConceptAndOutcome = tmp %>% tidyr::complete(!!outcomeVar,concept_id, fill=list(docsWithTermAndOutcome=0))
    
    # join the by concept and by outcome data
    result = expandedByConceptAndOutcome %>% 
      inner_join(byConcept %>% collect(), by = "concept_id") %>%
      inner_join(byOutcome %>% collect(), by = as.character(outcomeVar)) %>%
      mutate(allDocs = self$totalDocuments)
    
    result = result %>% mutate(
      npmi = omopUtils::npmiFromCounts(docsWithTermAndOutcome, docsWithTerm, docsWithOutcome, allDocs),
      # pmi = pmiFromCounts(docsWithTermAndOutcome, docsWithTerm, docsWithOutcome, allDocs),
      mi = omopUtils::miFromCounts(docsWithTermAndOutcome, docsWithTerm, docsWithOutcome, allDocs)
    )
    
    # add into groupedDf concept and outcome npmi and mi
    self$groupedDf = self$groupedDf %>% inner_join(
      result %>% select(!!outcomeVar,concept_id,npmi,mi), 
      copy=TRUE, by=c("concept_id",as.character(outcomeVar)))
    
    # add into node the combination with the maximum value of npmi - i.e. the outcome class which is posistively associated with
    # this term, and the degree of that association.
    self$nodes = self$nodes %>% inner_join(
      result %>% group_by(concept_id) %>% select(!!outcomeVar,concept_id,mi) %>% summarise(mi = mean(mi,na.rm = TRUE)), 
      copy=TRUE, by = "concept_id")
    
    self$nodes = self$nodes %>% left_join(
      result %>% group_by(concept_id) %>% 
        select(!!outcomeVar,concept_id,npmi) %>% 
        filter(abs(npmi) == max(abs(npmi),na.rm = TRUE)) %>%
        filter(1 == n()), 
      copy=TRUE, by = "concept_id")
    
    invisible(self)
  },
  
  
  #' @description get an inverse document frequency for concepts associated with a grouping
  #' @details
  #' The grouped datafram here acts as a "Document" from the perpective of the TFIDF calculation but might be a person
  #' 
  #' @param k1 default 1.2 - okapi BM25
  #' @param b default 0.95 - okapi BM25
  #' @return an analysis object with the groupedDf expanded to include tfidf stats for each concept in each group
  calculateTfidf = function(k1 = 1.2, b = 0.95) {
    # this currently works on the original grouping of the corpus
    # If this includes a grouping for outcome (as initially intended)
    # documents will get a different tfidf, then the outcome must be a super set
    # e.g. group_by(outcome,document_id) must be the same as group_by(document_id)
    
    if(is.null(self$grps)) {
      print("Original data had no grouping information to define 'documents'")
      return()
    }
    if(self$totalDocuments == 1) {
      print("Only one document in collection")
      return()
    }
    
    if(is.null(self$nodes)) {
      self$calculateIdfNodes()
    }
    
    # grps = c(groupedDf %>% groups(),as.symbol("concept_id"))
    groupedDf = self$groupedDf
    grps = self$grps
    nodes = self$nodes
    N = self$totalDocuments
    
    # get the count of codes/terms - n
    # totalCodes = nodes %>% summarise(n=sum(count)) %>% pull(n) %>% collect()
    # f = totalCodes 
    # n.b. this is never used
    
    # make sure grouping OK
    groupedDf = groupedDf %>% ungroup() %>% group_by(!!!grps)
    
    # in groupedDf we are counting cooncepts / terms
    # the tf statistic
    
    # Doucment level grouping
    # count is number of occurences of given concept(term - t) in each document(d) this is n_td
    # f_td frequency of a given term in a given document (also referred to a f_td)
    # f_d is the total n of codes in a given d
    # max_f_d maximum number of terms in each document
    # max_f_td greatest number of any term in a given document (also referred to a max_f_td)
    groupedDf = groupedDf %>% mutate(
      f_td = count,
      f_d = sum(count, na.rm=TRUE),
      max_f_d=max(f_d, na.rm=TRUE),
      max_f_td = max(count, na.rm=TRUE)
    )
    
    # Corpus level stats
    # avg_n_d average number of terms in all documents - is the same as avg_dl - document length (in terms of codes) on Nr in D
    groupedDf = groupedDf %>% ungroup() %>% mutate(
      avg_f_d=mean(as.double(f_d),na.rm=TRUE),
    )
    
    # G. Paltoglou and M. Thelwall, “A Study of Information Retrieval Weighting Schemes for Sentiment Analysis,” in Proceedings of the 48th Annual Meeting of the Association for Computational Linguistics, Uppsala, Sweden, 2010, pp. 1386–1395 [Online]. Available: http://dl.acm.org/citation.cfm?id=1858681.1858822
    # TODO: Other tfidf metrics
    # TODO: how do we validate this?
    # TODO: split out idf generation so we can do for whole corpus
    
    tmp = groupedDf %>% left_join(
        nodes %>% select(concept_id, f_t, N_t, max_N_t, max_f_t, N),
        by="concept_id"
      ) %>% mutate(
        length_normalised_tfidf = as.double(f_td) / f_d * log( as.double(N) / N_t),
        o_bm25 = ((k1+1)*as.double(f_td))/(k1*((1-b)+b*as.double(f_d)/avg_f_d)+f_td),
        k_bm25 = log(as.double(N-N_t)+0.5)/(N_t+0.5)
      ) %>% mutate (
        okapi_bm25 = o_bm25*k_bm25
      ) %>% mutate(
        sum_sq = sqrt(sum(okapi_bm25^2, na.rm=TRUE))
      ) %>% mutate(
        norm_okapi_bm25 = okapi_bm25/sum_sq
      ) %>% compute()
    
    self$omop$tidyTempTable(self$groupedDf)
    self$groupedDf = tmp
    
    return(self)
  },
  
  #' @description plot the currently loaded graph.
  #' TODO: facet plot on different scoreVars
  #' TODO: make layout fixed for multiple graphs: https://github.com/thomasp85/ggraph/issues/130
  #' TODO: split this into own class?
  #' TODO: aggregatre tfidf measures
  #' TODO: mutual information measures
  #' @param scoreVar a node feature the the graph will use as e.g. f_t, N_t, idf measures...
  #' @param labelScore a percentage of score that will trigger a node being labelled
  #' @import tidygraph
  #' @return the networkAnalysis object with a graph loaded
  #' @examples
  #' omop = Omop$new(...)
  #' s = Searcher$new(omop)
  #' df = s.search("%myoc%inf%") 
  #' a = Analyser$from...
  #' a$plotConceptNetwork()
  plotConceptNetwork = function(scoreVar = "f_t", labelScore = 1) {
    scoreVar = ensym(scoreVar)
    
    if (is.null(self$edges)) { self$calculateGraph()  }
    tmpNodes = self$nodes %>% arrange(id) %>% collect() %>% mutate(id = as.integer(id))
    tmpEdges = self$edges %>% collect() %>% mutate(from = as.integer(from), to=as.integer(to))
    
    if (as.character(scoreVar) %in% colnames(self$nodes)) {
        tmpNodes = tmpNodes %>% mutate(score = (as.double(!!scoreVar)/max(!!scoreVar,na.rm=TRUE)))
    } else {
        tmpNodes = tmpNodes %>% mutate(score = 1)
    }
    
    graph = tidygraph::tbl_graph(nodes = tmpNodes, edges = tmpEdges, directed = TRUE)
    # TODO: implement some kind of more generic filtering here
    graph = graph %>% activate(nodes) %>% mutate(isRoot = node_is_sink())
    graph = graph %>% activate(nodes) %>% mutate(isLabelled = (score>=labelScore))
    graph = graph %>% activate(edges) %>% mutate(fromScore = .N()$score[from], toScore = .N()$score[to])
    
    set.seed(101)
    p = ggraph::ggraph(graph, layout="nicely")+
      ggraph::geom_edge_diagonal(aes(
          # colour=hsv(0,0,0.75*(1-toScore)), 
          # alpha=stat(index)*(toScore-fromScore)+fromScore,
          # alpha=0.25+0.75*fromScore,
          # alpha=stat(index)*0.75*(fromScore-toScore)+0.75*toScore+0.25,
          edge_width=0.25+0.75*sqrt(fromScore)
        ), alpha=0.1, flipped=TRUE)+
      ggraph::geom_node_point(aes(
          size=0.25+0.75*sqrt(score),
          colour=ifelse(isLabelled | isRoot, ifelse(isRoot,"#ff0000","#0000ff"), "#000000"),
          #alpha = ifelse(isLabelled | isRoot,1,0.25+0.75*score),
          alpha = ifelse(isLabelled | isRoot,1,0.5) 
        ),stroke=0)+
      ggraph::geom_node_label(aes(
          label=ifelse(score>labelScore | isRoot,concept_name,NA),
          colour=ifelse(isRoot,"#ff0000","#0000ff")
        ),
          alpha=0.8,
          repel=TRUE, 
          size=(10/ggplot2:::.pt/(96/72))
        ) + 
      scale_fill_identity() + scale_color_identity() + scale_edge_color_identity() + scale_alpha_identity() +
      ggraph::theme_graph() + theme(legend.position = "none")
    return(p)
  },
  
  #' @description plot the currently loaded graph as sankey diagram.
  #' TODO: figure out how the links get a weight that is meaningful in this context
  #' This would be good for visualising a classifier network
  #' @param linkWeightVar the score on the link that will define the width of the network
  plotSankeyD3 = function(linkWeightVar = "weight") {
    linkWeightVar = ensym(linkWeightVar)
    
    if (is.null(self$edges)) { self$calculateGraph()  }
    tmpNodes = tmpNodes %>% arrange(id) %>% collect() %>% mutate(id = as.integer(id-1))
    tmpEdges = self$edges %>% collect() %>% mutate(from = as.integer(from-1), to=as.integer(to-1))
    
    if (as.character(linkWeightVar) %in% colnames(tmpEdges)) {
      tmpEdges = tmpEdges %>% mutate(linkWeight = !!linkWeightVar)
    } else {
      tmpEdges = tmpEdges %>% mutate(linkWeight = 1)
    }
    
    networkD3::sankeyNetwork(Links = tmpEdges, Nodes = tmpNodes, Source = "from",
                  Target = "to", Value = "linkWeight", NodeID = "concept_name",
                  fontSize = 10, nodeWidth = 30,sinksRight=TRUE)
    invisible(self)
  },
  
  #TODO: use visNetwork for final visuals? need to be convinced we can export to pdf
  # ? via webshot
  
  # TODO
  # a function to calculate the mutual information between a score and an outcome (one of the groupings)
  # also could be a score (e.g. tfidf in the groupedDf) and an outcome (one of the groupings)
  # will need to specify the outcome variable & score variable
  # may need some way to discretize one or both
  # https://rdrr.io/cran/infotheo/man/mutinformation.html
  # or estimate an earth mover distance between pdfs of score in each outcome groups
  # or chiSq on pdfs
  # can they be assumed to be normally distiributed? The Two Sample Behrens-Fisher Problem (Neyman-Pearson)
  # https://en.wikipedia.org/wiki/Feature_selection#Information_Theory_Based_Feature_Selection_Mechanisms
  
  # TODO: part C:
  # With NPMI scores for outcome and co-occurences we could look at a distance metric
  # maximise absolute value of NPMI code outcome whilst minimise NPMI between different concept codes. 
  # some sort of repulsion model may then identify clusters that are dissimilar and predictive of the 
  # outcome
  # alternatively filter concepts with high NPI with outcome and 
  # detect communities of concepts on concept/concept NPMI and calculate cluster NPMI to 
  
  #' @description calculate a co-occurence matrix for the most 
  #' @param filteredNodes - a list of 
  #' @param limit the max number of terms co-occurrences that will be used for cooccurrence or NA for no limit
  #' @param ascending order the factor as ascending
  #' @return the co-occurrence matrix as a dataframe
  calculateCooccurrenceMI = function(filteredNodes) {
    
	filteredNodes = filteredNodes %>% compute()
    # Generate the "full" matrix of cooccurences
    # actually not a full matrix - limited to top 300 by default and then only one side of that
    # is actually calculated as MI is symmetrical
    # created on a join using a constant value j to give all possible cooccurrences.
    fullMatrix = filteredNodes %>% select(concept_id1 = concept_id) %>% mutate(j=1) %>% inner_join(
      filteredNodes %>% select(concept_id2 = concept_id) %>% mutate(j=1), by="j"
    ) %>% select(-j)
    
    # add corpus level frequency information from each of the 2 sides of the co-occurrence matrix
    fullMatrix = fullMatrix %>% 
      inner_join(filteredNodes %>% select(concept_id, concept_name1 = concept_name, N_t1 = N_t), by=c("concept_id1"="concept_id")) %>%
      inner_join(filteredNodes %>% select(concept_id, concept_name2 = concept_name, N_t2 = N_t, N), by=c("concept_id2"="concept_id"))
    
    fullMatrix = fullMatrix %>% compute()
    
    # create the co-occurence information from the document level information
    # create a join specfication based on the "document" groupings as a named vector
    joins = sapply(self$grps, as.character)
    names(joins) = lapply(self$grps, as.character)
    # apply the join on the document leve info
    tmp = self$groupedDf %>% semi_join(filteredNodes, by="concept_id", copy=TRUE) %>% select(!!!self$grps,concept_id1 = concept_id) %>% left_join(
      self$groupedDf %>% semi_join(filteredNodes, by="concept_id", copy=TRUE) %>% select(!!!self$grps,concept_id2 = concept_id), by=joins) %>%
      filter(!is.na(concept_id2))
    # count the number of co-occurences
    tmp = tmp %>% ungroup() %>% group_by(concept_id1,concept_id2) %>% summarise(N_t1t2 = n()) %>% 
      compute()
    
    # add document level co-occurrnce frequency info to the matrix
    fullMatrix = fullMatrix %>% left_join(tmp, by=c("concept_id1","concept_id2"), copy=TRUE) %>% mutate(N_t1t2 = ifelse(is.na(N_t1t2),0,N_t1t2))
    # self$omop$tidyTempTable(tmp)
    
    # calculate the mutual information (locally in R for the time being - then copy it back to the database)
    fullMatrixLocal = fullMatrix %>% collect() %>% mutate(
      npmi = npmiFromCounts(N_t1t2, N_t1, N_t1, N),
      mi = miFromCounts(N_t1t2, N_t1, N_t2, N)
    )
    # self$omop$tidyTempTable(fullMatrix) - not required as never computed
    # self$cooccurrence = self$omop$con %>% copy_to(fullMatrixLocal,overwrite=TRUE)
    
    return(fullMatrixLocal)
  },  
  
  
  
  
  #' @description print the analyser
  print = function() {
    print("R6 network analyser class")
    invisible(self)
  }

))

#### static ----
#' @name Analyser_fromSearcher
#' @title Create an analyser from a searcher
#' @usage Analyser$fromSearcher(searcher)
#' @description creates a network analyser from a searcher
#' @param searcher the name of the file - initial part of path - no .vocab.rds extension
#' @param expanded has the searcher already had ancestors expanded?
#' @return an analyser with idf and graph counts precomputed.
NULL
Analyser$fromSearcher = function(searcher, expanded = FALSE) {
  a = Analyser$new(searcher$omop, searcher$result, as.symbol("concept_id")) 
  if (!expanded) {  a$countAncestorConcepts() }
  return(a)
}

#' @name Analyser_fromDataframe
#' @title Create an analyser from a data frame
#' @usage Analyser$fromSearcher(searcher)
#' @description creates a network analyser from a searcher
#' @param omop an instance of the Omop class
#' @param groupedDf a data frame grouped to define the document level groupings in the data
#' @param field the field that holds the concept_id 
#' @param expanded has the searcher already had ancestors expanded?
#' @return an analyser with idf and graph counts precomputed.
NULL
Analyser$fromDataframe = function(omop, groupedDf, field = concept_id, expanded = FALSE) {
  field = ensym(field)
  a = Analyser$new(omop, groupedDf, !!field)
  if (!expanded) {  a$calculateAncestorConcepts() }
  return(a)
}
