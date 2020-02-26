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
  nodes = NULL,
  #' @field edges a dataframe of shortest ancestor edges between nodes
  edges = NULL,
  #' @field idMap mapping concept_ids to sequential ids
  idMap = NULL,
  

  #### Methods ----
  #' @description create a concept analyser - typically not designed to be called directly. See Analyser$fromSearcher and Analyser$fromDataframe
  #' @param omop an R6 Omop object
  #' @param nodesDf a dataframe containing a dataframe with minimally a list of node concept_ids
  #' @param field the field name of the concept_id
  initialize = function( omop, nodesDf, conceptIdVar = "concept_id", countVar = "count") {
    
    conceptIdVar = ensym(conceptIdVar)
    countVar = ensym(countVar)
    self$omop = omop;
    nodesDf = nodesDf %>% rename(concept_id = !!conceptIdVar) %>% ungroup();
    # create a count column if none exists in our input groupedDf
    if (!as.character(countVar) %in% colnames(nodesDf)) {  
      nodesDf = nodesDf %>% mutate(count = 1L)   
    } else {
      nodesDf = nodesDf %>% rename(count = !!countVar)
    }
    
    self$idMap = nodesDf %>% select(concept_id) %>% distinct() %>% arrange(concept_id) %>% mutate(id = row_number()) %>% compute()
    self$nodes = nodesDf %>% left_join(self$idMap, by="concept_id") %>% self$omop$getConceptNames() %>% compute()
    self$edges = nodesDf %>% omop$getSpanningGraphEdges(concept_id) %>% 
      inner_join(self$idMap %>% rename(descendant_concept_id=concept_id, from=id), by="descendant_concept_id") %>%
      inner_join(self$idMap %>% rename(ancestor_concept_id=concept_id, to=id), by="ancestor_concept_id") %>% compute()
    
  },

  #' @description as tidygraph.
  #' @import tidygraph
  #' @return a tidygraph::tbl_graph
  toTidygraph = function() {
    tmpNodes = self$nodes %>% arrange(id) %>% collect() %>% mutate(id = as.integer(id))
    tmpEdges = self$edges %>% collect() %>% mutate(from = as.integer(from), to=as.integer(to))
    graph = tidygraph::tbl_graph(nodes = tmpNodes, edges = tmpEdges, directed = TRUE)
    return(graph)
  },
  
  #' @description as igraph.
  #' @import igraph
  #' @return an igraph::graph
  toIgraph = function() {
    ## A simple example with a couple of actors
    ## The typical case is that these tables are read in from files....
    tmpNodes <- self$nodes %>% select(id,everything())
    tmpEdges <- self$edges %>% select(from,to,everything())
    g = igraph::graph_from_data_frame(tmpEdges, directed=TRUE, vertices=tmpNodes)
    return(g)
  },
  
  #' @description conver the network to a DiagrammeR::graph
  #' @return the DiagrammeR::graph
  toDiagrammeR = function() {
    tmpNodes = self$nodes %>% mutate(
      nodes=id,
      type="concept",
      label=concept_name
      # fixedsize = as.logical(0)
    ) %>% select(nodes,label,everything()) %>% collect()
    tmpEdges = self$edges %>% mutate(rel="is_a") %>% select(from,to,rel,everything()) %>% collect()
    graph = 
      DiagrammeR::create_graph(
        nodes_df = tmpNodes,
        edges_df = tmpEdges,
        attr_theme = "bt") %>% 
      standardPrintOutput::defaultGraphLayout()
  },
  
  #### Graph filtering ----
  
  #' @description removes indirect paths between nodes leaving only the longest paths between nodes
  #' @param distance (defaults to one - the minimum level of path distances that are removed) larger values leaves more of the graph intact
  #' @return the analyser with edges filtered to include only those with most direct ancestral connections
  longestPaths = function(distance = 1) {
    twoEdges = self$edges %>% filter(min_levels_of_separation >= distance) %>% rename(common_concept_id = ancestor_concept_id) %>% 
      inner_join(self$edges %>% filter(min_levels_of_separation >= distance) %>% rename(common_concept_id = descendant_concept_id), by="common_concept_id")
    self$edges = self$edges %>% anti_join(twoEdges, by=c("ancestor_concept_id","descendant_concept_id")) %>% compute()
    invisible(self)
  },
  
  #' @description select nodes which have a neighbourhood local maximum score value
  #' @return a list of nodes with maximum scoreVar in the neighbourhood
  nodesWithMaxScores = function(scoreVar) {
    scoreVar = ensym(scoreVar)
    lhs = self$nodes %>% rename(lhsScore = !!scoreVar, from = id) %>% select(from,lhsScore)
    rhs = self$nodes %>% rename(rhsScore = !!scoreVar, to = id) %>% select(to,rhsScore)
    comp = self$edges %>% inner_join(lhs, by="from") %>% inner_join(rhs, by="to")
    removeLhs = comp %>% filter(lhsScore < rhsScore) %>% rename(id = from) %>% select(id)
    removeRhs = comp %>% filter(lhsScore > rhsScore) %>% rename(id = to) %>% select(id)
    out = self$nodes %>% anti_join(removeLhs, by="id") %>% anti_join(removeRhs, by="id")
    return(out)
  },
  
  #' @description mutate nodes
  #' @param ... mutate operation
  mutateNodes = function(...) {
    self$nodes = self$nodes %>% mutate(...)
    invisible(self)
  },
  
  #' @description mutate edges
  #' @param ... mutate operation
  mutateEdges = function(...) {
    self$edges = self$edges %>% mutate(...)
    invisible(self)
  },
  
  #### Graph visualisation ----
  
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
  plotConceptNetwork = function() {
    graph = self$toTidygraph()
    # TODO: implement some kind of more generic filtering here
    # graph = graph %>% activate(nodes) %>% mutate(isRoot = node_is_sink())
    # graph = graph %>% activate(nodes) %>% mutate(isLabelled = (score>=labelScore))
    # graph = graph %>% activate(edges) %>% mutate(fromScore = .N()$score[from], toScore = .N()$score[to])
    
    set.seed(101)
    p = ggraph::ggraph(graph, layout="nicely")+
      ggraph::geom_edge_diagonal(#aes(
          # colour=hsv(0,0,0.75*(1-toScore)), 
          # alpha=stat(index)*(toScore-fromScore)+fromScore,
          # alpha=0.25+0.75*fromScore,
          # alpha=stat(index)*0.75*(fromScore-toScore)+0.75*toScore+0.25,
          # edge_width=0.25+0.75*sqrt(fromScore)
        #), 
        alpha=0.1, flipped=TRUE)+
      ggraph::geom_node_point(#aes(
          # size=0.25+0.75*sqrt(score),
          # colour=ifelse(isLabelled | isRoot, ifelse(isRoot,"#ff0000","#0000ff"), "#000000"),
          # alpha = ifelse(isLabelled | isRoot,1,0.25+0.75*score),
          # alpha = ifelse(isLabelled | isRoot,1,0.5) 
        #),
        stroke=0)+
      ggraph::geom_node_label(aes(
          label=concept_name #ifelse(score>labelScore | isRoot,concept_name,NA),
          #colour=ifelse(isRoot,"#ff0000","#0000ff")
        ),
          alpha=0.8,
          repel=TRUE, 
          size=(10/ggplot2:::.pt/(96/72))
        ) + 
      scale_fill_identity() + scale_color_identity() + ggraph::scale_edge_color_identity() + scale_alpha_identity() +
      ggraph::theme_graph() + theme(legend.position = "none")
    return(p)
  },
  
  #' @description plot the currently loaded graph as sankey diagram.
  #' TODO: figure out how the links get a weight that is meaningful in this context
  #' This would be good for visualising a classifier network
  #' @param linkWeightVar the score on the link that will define the width of the network
  plotSankeyD3 = function(linkWeightVar = "weight") {
    linkWeightVar = ensym(linkWeightVar)
    
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
Analyser$fromSearcher = function(searcher) {
  a = Analyser$new(searcher$omop, searcher$result, "concept_id") 
  return(a)
}


#### graph vis ----

#' @description plot the currently loaded graph.
#' @param scoreVar a node feature the the graph will use as e.g. f_t, N_t, idf measures...
#' @param labelScore a percentage of score that will trigger a node being labelled
#' @import tidygraph
#' @return the networkAnalysis object with a graph loaded
#' @examples
plotConcepts = function(tidygraph) {
  
  # graph = graph %>% activate(nodes) %>% mutate(isRoot = node_is_sink())
  # graph = graph %>% activate(nodes) %>% mutate(isLabelled = (score>=labelScore))
  # graph = graph %>% activate(edges) %>% mutate(fromScore = .N()$score[from], toScore = .N()$score[to])
  
  set.seed(101)
  p = ggraph::ggraph(tidygraph, layout="dendrogram")+
    ggraph::geom_edge_diagonal(#aes(
      # colour=hsv(0,0,0.75*(1-toScore)), 
      # alpha=stat(index)*(toScore-fromScore)+fromScore,
      # alpha=0.25+0.75*fromScore,
      # alpha=stat(index)*0.75*(fromScore-toScore)+0.75*toScore+0.25,
      # edge_width=0.25+0.75*sqrt(fromScore)
      #), 
      alpha=0.1, flipped=TRUE)+
    ggraph::geom_node_point(#aes(
      # size=0.25+0.75*sqrt(score),
      # colour=ifelse(isLabelled | isRoot, ifelse(isRoot,"#ff0000","#0000ff"), "#000000"),
      # alpha = ifelse(isLabelled | isRoot,1,0.25+0.75*score),
      # alpha = ifelse(isLabelled | isRoot,1,0.5) 
      #),
      stroke=0)+
    ggraph::geom_node_label(aes(
      label=concept_name #ifelse(score>labelScore | isRoot,concept_name,NA),
      #colour=ifelse(isRoot,"#ff0000","#0000ff")
    ),
    alpha=0.8,
    repel=TRUE, 
    size=(10/ggplot2:::.pt/(96/72))
    ) + 
    scale_fill_identity() + scale_color_identity() + scale_edge_color_identity() + scale_alpha_identity() +
    ggraph::theme_graph() + theme(legend.position = "none")
  return(p)
}
