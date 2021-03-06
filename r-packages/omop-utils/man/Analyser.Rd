% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NetworkAnalysis.R
\name{Analyser}
\alias{Analyser}
\title{Get an of the omop databse analyser}
\description{
Get an of the omop databse analyser

Get an of the omop databse analyser
}
\examples{

## ------------------------------------------------
## Method `Analyser$plotConceptNetwork`
## ------------------------------------------------

omop = Omop$new(...)
s = Searcher$new(omop)
df = s.search("\%myoc\%inf\%") 
a = Analyser$from...
a$plotConceptNetwork()
}
\keyword{omop}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{omop}}{the omop connection}

\item{\code{groupedDf}}{a grouped dataframe including a grouping representing the documents, distinct concepts in the groups and counts of concepts in document}

\item{\code{edges}}{a dataframe of shortest ancestor edges between nodes}

\item{\code{idMap}}{mapping concept_ids to sequential ids}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{groupedDf}}{a grouped dataframe including a grouping representing the documents, distinct concepts in the groups and counts of concepts in document}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Analyser$new()}}
\item \href{#method-toTidygraph}{\code{Analyser$toTidygraph()}}
\item \href{#method-toIgraph}{\code{Analyser$toIgraph()}}
\item \href{#method-toDiagrammeR}{\code{Analyser$toDiagrammeR()}}
\item \href{#method-longestPaths}{\code{Analyser$longestPaths()}}
\item \href{#method-nodesWithMaxScores}{\code{Analyser$nodesWithMaxScores()}}
\item \href{#method-mutateNodes}{\code{Analyser$mutateNodes()}}
\item \href{#method-mutateEdges}{\code{Analyser$mutateEdges()}}
\item \href{#method-plotConceptNetwork}{\code{Analyser$plotConceptNetwork()}}
\item \href{#method-plotSankeyD3}{\code{Analyser$plotSankeyD3()}}
\item \href{#method-print}{\code{Analyser$print()}}
\item \href{#method-clone}{\code{Analyser$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
create a concept analyser - typically not designed to be called directly. See Analyser$fromSearcher and Analyser$fromDataframe
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Analyser$new(omop, nodesDf, conceptIdVar = "concept_id", countVar = "count")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{omop}}{an R6 Omop object}

\item{\code{nodesDf}}{a dataframe containing a dataframe with minimally a list of node concept_ids}

\item{\code{field}}{the field name of the concept_id}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toTidygraph"></a>}}
\subsection{Method \code{toTidygraph()}}{
as tidygraph.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Analyser$toTidygraph()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a tidygraph::tbl_graph
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toIgraph"></a>}}
\subsection{Method \code{toIgraph()}}{
as igraph.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Analyser$toIgraph()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
an igraph::graph
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toDiagrammeR"></a>}}
\subsection{Method \code{toDiagrammeR()}}{
conver the network to a DiagrammeR::graph
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Analyser$toDiagrammeR()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
the DiagrammeR::graph
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-longestPaths"></a>}}
\subsection{Method \code{longestPaths()}}{
removes indirect paths between nodes leaving only the longest paths between nodes
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Analyser$longestPaths(distance = 1)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{distance}}{(defaults to one - the minimum level of path distances that are removed) larger values leaves more of the graph intact}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
the analyser with edges filtered to include only those with most direct ancestral connections
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-nodesWithMaxScores"></a>}}
\subsection{Method \code{nodesWithMaxScores()}}{
select nodes which have a neighbourhood local maximum score value
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Analyser$nodesWithMaxScores(scoreVar)}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a list of nodes with maximum scoreVar in the neighbourhood
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-mutateNodes"></a>}}
\subsection{Method \code{mutateNodes()}}{
mutate nodes
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Analyser$mutateNodes(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{mutate operation}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-mutateEdges"></a>}}
\subsection{Method \code{mutateEdges()}}{
mutate edges
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Analyser$mutateEdges(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{mutate operation}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-plotConceptNetwork"></a>}}
\subsection{Method \code{plotConceptNetwork()}}{
plot the currently loaded graph.
TODO: facet plot on different scoreVars
TODO: make layout fixed for multiple graphs: https://github.com/thomasp85/ggraph/issues/130
TODO: split this into own class?
TODO: aggregatre tfidf measures
TODO: mutual information measures
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Analyser$plotConceptNetwork()}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{scoreVar}}{a node feature the the graph will use as e.g. f_t, N_t, idf measures...}

\item{\code{labelScore}}{a percentage of score that will trigger a node being labelled}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
the networkAnalysis object with a graph loaded
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{omop = Omop$new(...)
s = Searcher$new(omop)
df = s.search("\%myoc\%inf\%") 
a = Analyser$from...
a$plotConceptNetwork()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-plotSankeyD3"></a>}}
\subsection{Method \code{plotSankeyD3()}}{
plot the currently loaded graph as sankey diagram.
TODO: figure out how the links get a weight that is meaningful in this context
This would be good for visualising a classifier network
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Analyser$plotSankeyD3(linkWeightVar = "weight")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{linkWeightVar}}{the score on the link that will define the width of the network}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\subsection{Method \code{print()}}{
print the analyser
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Analyser$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Analyser$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
