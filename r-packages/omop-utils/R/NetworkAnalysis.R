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
  

  #### Methods ----
  #' @description create a concept analyser - typically not designed to be called directly. See Analyser$fromSearcher and Analyser$fromDataframe
  #' @param omop an R6 Omop object
  #' @param field the field name of the concept_id
  #' @param expanded has the data already been expanded to include ancestors
  #' @param groupedDf a data frame including at least a concept_id plus the groupings defining documents, and ideally counts
  initialize = function(
        omop, 
        field = "concept_id",
        expanded = FALSE,
        groupedDf
  ) {
    self$omop = omop;
    # create a count column if none exists in our input groupedDf
    if (!"count" %in% colnames(groupedDf)) {  groupedDf = groupedDf %>% mutate(count=1)   }
    field = ensym(field)
    grps = groupedDf %>% groups()
    self$grps = grps
    # get the distinct concept_ids and counts in each group of the groupedDf 
    # this may have already happened upstream
    self$groupedDf = groupedDf %>% mutate(concept_id = !!field) %>% group_by(!!!grps,concept_id) %>% summarise(count = sum(count))
    if (!expanded) {
      # call the ancestor lookup function
      self$countAncestorConcepts()
    }

    # nodes are the unique concepts accross all groups - these are counted to give us:
    # count = number of occurrences in corpus
    # documents = number of documents that each concept appears in (useful for idf)
    self$nodes = self$groupedDf %>% ungroup() %>% 
      group_by(concept_id) %>% 
      summarise(count=sum(count), documents=n()) %>% 
      # order_by(local(1)) %>%
      order_by(concept_id) %>%  
      mutate(id = row_number()) %>%
      compute()    
    
    # build the links using the ancestor table and shortest paths
    self$edges = self$omop$concept_ancestor %>% #filter(min_levels_of_separation == 1) %>%
      inner_join(self$nodes %>% select(descendant_concept_id=concept_id,from=id), by="descendant_concept_id") %>%
      inner_join(self$nodes %>% select(ancestor_concept_id=concept_id,to=id), by="ancestor_concept_id") %>%
      group_by(ancestor_concept_id,descendant_concept_id) %>%
      filter(min_levels_of_separation == min(min_levels_of_separation)) %>% # equiv of having clause - shortest paths
      compute()
      # collect()

  },

  
  # TODO: make private?
  #' @description get ancestors for a grouped set of concepts preserving grouping and count information
  #' @return the analyser with expanded
  countAncestorConcepts = function() {
    # first get the concepts by group in groupedDf - i.e. concepts in each document and sum counts
    # this gives us the freqency of a concept_id in a document
    tmp = self$groupedDf %>% group_by(!!!self$grps,concept_id) %>% summarise(count=sum(count)) %>%
      rename(descendant_concept_id=concept_id) %>% compute()
    # expand to the ancestor table.
      tmp2 = self$omop$concept_ancestor %>%
        inner_join(tmp, by="descendant_concept_id", copy=TRUE) %>%
        rename(concept_id=ancestor_concept_id) %>%
        # reapply the grouping as the columns have changed name
        # this will include the original concepts (as they are 1:1 mapped in the ancestor table)
        # as well as probably multiple copies of all the ancestors to root
        ungroup %>% group_by(!!!self$grps,concept_id) %>%
        # when more than one concept within a document matches the ancestor we sum the count
        summarise(count = sum(count, na.rm = TRUE)) %>%
        compute()
    self$groupedDf = 
      #the searcher here gives us a standard filtered list of concepts
      Searcher$new(self$omop)$get() %>% select(concept_id,concept_name) %>% 
      inner_join(tmp2, by="concept_id")
      #TODO: should we be trying to map to standard concepts as well at this point....
    return(self)
  },
  
  #' @description get an inverse document frequency for concepts associated with a grouping
  #' @details
  #' The grouped datafram here acts as a "Document" from the perpective of the TFIDF calculation but might be a person
  #' 
  #' a dataframe with a concept_id field and at least one group_by active defining the "document"
  #' @return an analysis object with the groupedDf expanded to include tfidf stats for each concept in each group
  #' @examples
  #' omop = Omop$new(...)
  #' a = Analyser$new(omop)
  #' df = ... get all concepts from a set of documents ...
  calculateTfidf = function() {
    # grps <- c(groupedDf %>% groups(),as.symbol("concept_id"))
    groupedDf = self$groupedDf
    grps = self$grps
    nodes = self$nodes
    
    # get the count of documents - N
    totalDocuments = groupedDf %>% select(!!!grps) %>% distinct() %>% ungroup() %>% summarise(N=n()) %>% pull(N) %>% collect()
    # N = totalDocuments
    
    # get the count of codes/terms - n
    # totalCodes = nodes %>% summarise(n=sum(count)) %>% pull(n) %>% collect()
    # n = totalCodes 
    # n.b. this is never used
    
    # make sure grouping OK
    groupedDf = groupedDf %>% ungroup() %>% group_by(!!!grps)
    
    # in groupedDf we are counting cooncepts / terms
    # the tf statistic
    # n_td frequency of a given term in a given document (also referred to a f_td)
    # n_d number of any terms in a given document
    # max_n_td greatest number of any term in a given document (also referred to a max_f_td)
    # avg_n_d average number of terms in all documents
    
    # count is number of occurences of given concept(term - t) in each document(d) this is n_td
    # n_d is the total n of codes in each d
    groupedDf = groupedDf %>% mutate(
      n_td = count,
      n_d = sum(count),
      max_n_td = max(count)
    )
    
    # avg_n_d is the same as avg_dl - document length (in terms of codes) on Nr in D
    groupedDf = groupedDf %>% ungroup() %>% mutate(
      avg_n_d=mean(as.double(n_d),na.rm=TRUE),
      max_n_d=max(n_d)
    )
    
    # in nodes
    # count is the number of instances of each code n_t
    # documents is number of documents containing given term N_t
    # max_N_t = max number of documents containing any given single term
    nodes = nodes %>% ungroup() %>% mutate(
        n_t = count,
        N_t = documents,
        max_N_t = max(N_t),
        max_n_t = max(n_t),
        N = local(totalDocuments)
    )
    
    self$nodes = nodes
    
    k1 = 1.2
    b = 0.95
    # G. Paltoglou and M. Thelwall, “A Study of Information Retrieval Weighting Schemes for Sentiment Analysis,” in Proceedings of the 48th Annual Meeting of the Association for Computational Linguistics, Uppsala, Sweden, 2010, pp. 1386–1395 [Online]. Available: http://dl.acm.org/citation.cfm?id=1858681.1858822
    
    # TODO: Other tfidf metrics
    # TODO: how do we validate this?
    # TODO: split out idf generation so we can do for whole corpus
    
    self$groupedDf = groupedDf %>% left_join(
        nodes %>% select(-count), #remove count for naming collision
        by="concept_id"
      ) %>% mutate(
        length_normalised_tfidf = n_td / n_d * log(N / n_t),
        o_bm25 = ((k1+1)*n_td)/(k1*((1-b)+b*n_d/avg_n_d)+n_td),
        k_bm25 = log(N-n_t+0.5)/(n_t+0.5),
        okapi_bm25 = o_bm25*k_bm25
      ) %>% mutate(
        sum_sq = sqrt(sum(okapi_bm25^2))
      ) %>% mutate(
        norm_okapi_bm25 = okapi_bm25/sum_sq
      )
    
    return(self)
  },
  
  #' @description plot the currently loaded graph.
  #' TODO: facet plot on different scoreVars
  #' TODO: make layout fixed for multiple graphs: https://github.com/thomasp85/ggraph/issues/130
  #' @param scoreVar a node feature the the graph will use as e.g.count, norm_okapi_bm25,
  #' @param labelScore a percentage of score that will trigger a node being labelled
  #' @import tidygraph
  #' @return the networkAnalysis object with a graph loaded
  #' @examples
  #' omop = Omop$new(...)
  #' s = Searcher$new(omop)
  #' df = s.search("%myoc%inf%") 
  #' a = Analyser$from...
  #' a$plotConceptNetwork()
  plotConceptNetwork = function(scoreVar = "count", labelScore = 1) {
    scoreVar = ensym(scoreVar)
    
    if (as.character(scoreVar) %in% colnames(self$nodes)) {
        tmp = self$nodes %>% mutate(score = (as.double(!!scoreVar)/max(!!scoreVar)))
    } else {
        tmp = self$nodes %>% mutate(score = 1)
    }
    
    graph = tidygraph::tbl_graph(nodes = tmp, edges = self$edges, directed = TRUE)
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
        ), alpha=0.1)+
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
  #' TODO: facet plot on different scoreVars
  #' TODO: make layout fixed for multiple graphs: https://github.com/thomasp85/ggraph/issues/130
  #' @param linkWeightVar the score on the link that will define the width of the network
  plotSankeyD3 = function(linkWeightVar = "weight") {
    linkWeightVar = ensym(linkWeightVar)
    
    tmpNodes = self$nodes %>% mutate(id = id-1)
    tmpEdges = self$edges %>% mutate(from = from-1, to=to-1)
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
  
  #TODO:
  # a function to calculate the mutual information between a score (e.g. tfidf in the groupedDf)
  # and an outcome (one of the groupings)
  # will need to specify the outcome variable & score variable
  # may need some way to discretize one or both
  # https://rdrr.io/cran/infotheo/man/mutinformation.html
  # or estimate an earth mover distance between pdfs of score in each outcome groups
  # or chiSq on pdfs
  
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
  a = Analyser$new(omop, field = "concept_id",expanded=expanded,searcher=searcher) 
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
Analyser$fromDataframe = function(omop, groupedDf, field = "concept_id", expanded = FALSE) {
  a = Analyser$new(omop, field = field, expanded = expanded, groupedDf = groupedDf)
  return(a)
}
