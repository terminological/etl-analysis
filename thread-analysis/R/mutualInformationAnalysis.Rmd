library(ggplot2)
library(ggdendro)
library(ggraph)
library(standardPrintOutput)

omop$finalize()
rm(list = ls(all.names = TRUE))

devtools::load_all("~/Git/etl-analysis/r-packages/omop-utils/")

omop = Omop$new()

groupedDf = 
  omop$note %>% filter(note_class_concept_id == 3032918L) %>% head(50) %>% union(
    omop$note %>% filter(note_class_concept_id == 3031179L) %>% head(50)
  ) %>%
  left_join(omop$note_nlp, by="note_id") %>%
  select(note_class_concept_id, note_id,lexical_variant, concept_id=note_nlp_concept_id) %>%
  omop$getConceptNames() %>% group_by(note_class_concept_id, note_id) %>% compute()

a = Analyser$fromDataframe(omop,groupedDf)
a$calculateTfidf()
a$calculateOutcomeMI(note_class_concept_id)
cooccurMI = a$nodes %>% ungroup() %>% arrange(desc(mi)) %>% head(50) %>% a$calculateCooccurrenceMI()
cooccurCount = a$nodes %>% ungroup() %>% arrange(desc(f_t)) %>% head(50) %>% a$calculateCooccurrenceMI()

plotClustering = function(df) {
	#df = cooccurMI
	tmp = df %>% select(concept_name1, concept_name2, mi) %>% collect()
	simMat = stats::xtabs(mi ~ concept_name1 + concept_name2,tmp, na.action = na.pass)
	simMat = ifelse(simMat==0,t(simMat),simMat)
	distMat = 1-simMat/max(simMat,na.rm=TRUE)
	clust = hclust(as.dist(t(distMat)))
	dhc = as.dendrogram(clust)
	ddata <- dendro_data(dhc, type = "rectangle")
	p = ggplot(segment(ddata)) + 
			geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
			coord_flip() + 
			scale_y_reverse() +
			expand_limits(x = c(0, 1), y = c(-1.5, 0))+
			geom_text(data = label(ddata), 
					aes(x = x, y = y, label = label), 
					vjust = 0.5, hjust=0,nudge_y = 0.04,
					#nudge_x = -0.1,# 
					size=(6/ggplot2:::.pt/(96/72))) +
			theme_dendro()
	return(p)
	# https://www.r-graph-gallery.com/339-circular-dendrogram-with-ggraph.html
	# ggdendro::ggdendrogram(clust, rotate=TRUE, size=(6/ggplot2:::.pt/(96/72))))
	# https://www.data-to-viz.com/graph/edge_bundling.html
}

plotClustering(cooccurMI)
standardPrintOutput::saveHalfPageFigure(filename="~/Dropbox/threadAnalysis/vocab/MIDendrogram")

plotClustering(cooccurCount)
standardPrintOutput::saveHalfPageFigure(filename="~/Dropbox/threadAnalysis/vocab/CountDendrogram")

#### Table of concepts by max nPMI and MI ----
nodes = a$nodes %>% omop$getConceptNames() %>% collect()
tmp = nodes %>% ungroup() %>% arrange(desc(mi)) %>% head(50) %>% select(
		Class=note_class_concept_name,
		Concept=concept_name,
		MI = mi,
		nPMI = npmi
		) %>% group_by(Class) 
tmp %>% standardPrintOutput::saveTable(filename="~/Dropbox/threadAnalysis/vocab/TopTermsByMI")

#### 
# a$plotConceptNetwork(scoreVar=mi,labelScore=0.8)
# standardPrintOutput::saveFullPageFigureLandscape(filename="~/Dropbox/threadAnalysis/vocab/OrthRheumVocab")

scoreVar = as.symbol("mi")
a$calculateGraph()
tmpNodes = a$nodes %>% arrange(id) %>% collect() %>% mutate(id = as.integer(id))
tmpEdges = a$edges %>% collect() %>% mutate(from = as.integer(from), to=as.integer(to))

if (as.character(scoreVar) %in% colnames(tmpNodes)) {
	tmpNodes = tmpNodes %>% mutate(score = (as.double(!!scoreVar)/max(!!scoreVar,na.rm=TRUE)))
} else {
	tmpNodes = tmpNodes %>% mutate(score = 1)
}

labelScore = 0.8
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
		ggraph::geom_node_point(aes(size=0.25+0.75*sqrt(score)),
						colour="#000000",
						alpha = 0.5,
						stroke=0)+
		ggraph::geom_node_point(aes(size=0.25+0.75*sqrt(score), filter=isRoot),
						colour="#ff0000",
						alpha = 1,
						stroke=0)+
		ggraph::geom_node_point(aes(size=0.25+0.75*sqrt(score), filter=isLabelled),
						colour="#0000ff",
						alpha = 1,
						stroke=0)+
		ggraph::geom_node_text(aes(
						label=ifelse(score>labelScore | isRoot,concept_name,NA),
						colour=ifelse(isRoot,"#ff0000","#0000ff")
				),
				alpha=0.8,
				repel=TRUE, 
				size=(6/ggplot2:::.pt/(96/72)) #standardPrintOutput::labelInPoints(6)
		) + scale_fill_identity() + scale_color_identity() + scale_edge_color_identity() + scale_alpha_identity() +
		ggraph::theme_graph() + theme(legend.position = "none")

p %>% standardPrintOutput::saveFullPageFigureLandscape(filename="~/Dropbox/threadAnalysis/vocab/OrthRheumVocab")