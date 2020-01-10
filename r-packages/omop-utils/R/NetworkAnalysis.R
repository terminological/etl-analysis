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
