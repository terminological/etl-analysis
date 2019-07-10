# install.packages("metR")
library(DBI)
library(odbc)
library(datasets)
library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)
library(tidyr)
library(stringr)
library(huxtable)
library(metR)

pwd <- readline(prompt="Enter DB: ");

con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 13 for SQL Server",
                 Server = "10.174.129.118",
                 Database = "RobsDatabase",
                 UID = "RobertSQL",
                 PWD = pwd,
                 Port = 1433,
                 bigint = "integer");
mapping <- dbReadTable(con,"mappingComparison")
mappingGoldFull <- dbReadTable(con,"mappingGoldStandardOPCS4")
mappingGold <- mappingGoldFull %>% 
  mutate(mapped_concept_id=target_concept_id) %>%
  group_by(source_concept_id,mapped_concept_id) %>% 
  summarise(
    source_description_name = paste0(source_description_name, collapse = "\n"),
    mapped_description_name = paste0(target_description_name, collapse = "\n")
  ) %>%
  distinct()
mappingDetail <- dbReadTable(con,"mappingTestDetailOPCS4")

mappingIndex <- dbReadTable(con,"mappingIndexInput")
mappingSearch <- dbReadTable(con,"mappingSearchInput")

# one description per concept - a feature of OPCS4.

# mappingDetail %>% select(source_concept_id,source_desc_id) %>% distinct() %>% group_by(source_concept_id) %>% summarise(n=n()) %>% filter(n>1)

sources <- mapping %>% select(source_concept_id) %>% distinct()
targets <- mapping %>% select(target_concept_id) %>% distinct()
mappeds <- mapping %>% select(mapped_concept_id) %>% distinct()



mapping %>% select(target_concept_id) %>% distinct() %>% count()

mapping %>% filter(!is.na(target_concept_id)) %>% select(source_concept_id) %>% distinct() %>% count()

# OLD ----

mapping %>% select(source_concept_id) %>% distinct() %>% count()

fg_concepts <- tmp %>% filter(mapped == 1) %>% select(source_concept_id)
bg_concepts <- tmp %>% filter(mapped == 0) %>% select(source_concept_id)

fg_concepts %>% count() + bg_concepts %>% count()



# cut off is the value at which woudl have first predicted the correct answer or the most similar value in the case of no prediction
rocForRank <- function(rank) { 
  
  matches <- 
    mapping %>% mutate(
      rankOrder = ifelse(is.na(rankOrder),100,as.integer(rankOrder)),
      similarity = ifelse(is.na(similarity),0,similarity)
    ) %>% filter(rankOrder>0) %>% arrange(source_concept_id,rankOrder) %>%
    group_by(source_concept_id) %>%
    select(source_concept_id,similarity,rankOrder,mapped_concept_id) %>%
    mutate(found = ifelse(is.na(mapped_concept_id),0,1))
  
  tmp <- matches %>% filter(rankOrder<=rank) %>% group_by(source_concept_id) %>%
    summarise(
      found_similarity = max(similarity * found),
      not_found_similarity = max(similarity * (1-found))
    ) %>% 
    mutate(prediction = ifelse(found_similarity==0,not_found_similarity,found_similarity)) %>%
    select(source_concept_id, prediction)
  
  matches %>% count()
  
  fg <- fg_concepts %>% left_join(tmp) %>% mutate(prediction = ifelse(is.na(prediction),0.5,prediction)) %>% select(prediction)
  bg <- bg_concepts %>% left_join(tmp) %>% mutate(prediction = ifelse(is.na(prediction),0.5,prediction)) %>% select(prediction)
  
  tmpRoc <- roc.curve(scores.class0 = fg$prediction, scores.class1 = bg$prediction, curve=TRUE)
  return(tmpRoc)
}

plot(rocForRank(1), color=1)
plot(rocForRank(5), color=2, add=TRUE)
plot(rocForRank(10), color=3, add=TRUE)


precisionForRank <- function(rank) {
  tmp <- mapping %>%  mutate(
    rankOrder = ifelse(is.na(rankOrder),100,as.integer(rankOrder)),
    similarity = ifelse(is.na(similarity),0,similarity)
  ) %>% filter(rankOrder>0) %>% filter(rankOrder <= rank)
  
  return (tmp %>% filter(!is.na(mapped_concept_id)) %>% count() / tmp %>% count())
}

recallForRank <- function(rank) {
  tmp <- mapping %>%  mutate(
    rankOrder = ifelse(is.na(rankOrder),100,as.integer(rankOrder)),
    similarity = ifelse(is.na(similarity),0,similarity)
  ) %>% filter(rankOrder>0) %>% filter(rankOrder <= rank)
  
  relevant <- mapping %>% filter(!is.na(mapped_concept_id)) %>% distinct() %>% count()
  
  return (tmp %>% filter(!is.na(mapped_concept_id)) %>% count() / relevant)
}

prRank <- as.data.frame(list(rank=seq(1,20)))
prRank$precision <- unlist(sapply(prRank$rank, precisionForRank))
prRank$recall <- unlist(sapply(prRank$rank, recallForRank))
prRank <- prRank %>% merge(tibble(beta=c(1,2,5,10)), by=NULL)
prRank <- prRank %>% mutate( 
  fValue = (1+beta^2)*precision*recall/((beta^2)*precision+recall),
  beta = as.factor(beta)
)



# ggplot(prRank,aes(x=recall,y=precision))+geom_line()+xlim(c(0,1))+ylim(c(0,1))

ggplot(prRank %>% filter(beta=='1'),aes(x=rank))+
  geom_histogram(aes(y=precision), stat='identity', color='blue', fill='white')+ylim(c(0,1))

ggplot(prRank %>% filter(beta=='1'),aes(x=rank))+
  geom_histogram(aes(y=recall), stat='identity', color='red', fill='white')+ylim(c(0,1))

ggplot(prRank,aes(x=rank, y=fValue, color = beta))+
  geom_line()+geom_point()
# OLD 2 precision for cutoff ----

precisionForSimilarity <- function(sim) {
  tmp <- mapping %>%  mutate(
    rankOrder = ifelse(is.na(rankOrder),100,as.integer(rankOrder)),
    similarity = ifelse(is.na(similarity),0,similarity)
  ) %>% filter(rankOrder>0) %>% filter(similarity >= sim)
  
  return (tmp %>% filter(!is.na(mapped_concept_id)) %>% count() / tmp %>% count())
}

recallForSimilarity <- function(sim) {
  tmp <- mapping %>%  mutate(
    rankOrder = ifelse(is.na(rankOrder),100,as.integer(rankOrder)),
    similarity = ifelse(is.na(similarity),0,similarity)
  ) %>% filter(rankOrder>0) %>% filter(similarity >= sim)
  
  relevant <- mapping %>% filter(!is.na(mapped_concept_id)) %>% distinct() %>% count()
  
  return (tmp %>% filter(!is.na(mapped_concept_id)) %>% count() / relevant)
}

pr <- as.data.frame(list(sim=seq(1,0.5,-0.01)))
pr$precision <- unlist(sapply(pr$sim, precisionForSimilarity))
pr$recall <- unlist(sapply(pr$sim, recallForSimilarity))
pr <- pr %>% merge(tibble(beta=c(0.1,0.2,0.33,0.5,1,2,3,5,10)), by=NULL)
# pr <- pr %>% merge(tibble(beta=seq(1,10,0.5)), by=NULL)

pr <- pr %>% mutate( 
  fValue = (1+beta^2)*precision*recall/((beta^2)*precision+recall),
  beta = as.factor(beta)
)

ggplot(pr %>% filter(beta=='1'),aes(x=sim))+
  geom_line(aes(y=precision), color='blue')+ylim(c(0,1))

ggplot(pr %>% filter(beta=='1'),aes(x=sim))+
  geom_line(aes(y=recall), color='red')+ylim(c(0,1))

ggplot(pr,aes(x=sim, y=fValue, color = beta))+
  geom_line()

# Confusion matrix ----

# confMatrixGenerator <- function(sim, rank) {
#   # rank = 5; sim = 0.75
#   tmp <- mapping %>% 
#     mutate(
#       source_concept_id,
#       target_concept_id = ifelse(is.na(target_concept_id) | (rankOrder <= rank & similarity >= sim),target_concept_id,NA),
#       mapped_concept_id,
#       similarity = ifelse(is.na(similarity) | (rankOrder <= rank & similarity >= sim),similarity,NA),
#       rankOrder = ifelse(is.na(rankOrder) | (rankOrder <= rank & similarity >= sim),rankOrder,NA)
#     ) %>% 
#     filter(!(is.na(mapped_concept_id) & is.na(target_concept_id))) %>%
#     select(source_concept_id,target_concept_id,mapped_concept_id,similarity,rankOrder) %>% distinct()
#   tmp <- sources %>% left_join(tmp)
#   tp = (tmp %>% filter(!is.na(target_concept_id) & !is.na(mapped_concept_id)) %>% select(source_concept_id) %>% distinct() %>% count())$n
#   fp = (tmp %>% filter(!is.na(target_concept_id) & is.na(mapped_concept_id))  %>% select(source_concept_id,target_concept_id) %>% distinct() %>% count())$n
#   tn = (tmp %>% filter(is.na(target_concept_id) & is.na(mapped_concept_id))  %>% select(source_concept_id) %>% distinct() %>% count())$n
#   fn = (tmp %>% filter(is.na(target_concept_id) & !is.na(mapped_concept_id))  %>% select(source_concept_id) %>% distinct() %>% count())$n
#   return(lst(sim=sim, rank=rank, tp=tp, tn=tn, fp=fp, fn=fn))
# }

filteredResult <- mappingDetail %>% group_by(source_concept_id) %>% mutate(
  rank=dense_rank(desc(similarity))
  # target_concept_id = ifelse(is.na(target_concept_id),-1,target_concept_id)
) %>%
  group_by(source_concept_id,target_concept_id) %>% # best hit
  summarise(rank = min(rank), similarity=max(similarity)) %>% # best hit
  select(source_concept_id,target_concept_id,rank,similarity) %>%
distinct()

confMatrixGenerator <- function(minSim, maxRank) {
  # minSim <- 0.5
  # maxRank <- 5
  tmp <- mappingGold %>% ungroup() %>% full_join(
    filteredResult %>% ungroup() %>% filter(rank<=maxRank & similarity>= minSim)
  ) %>% mutate(
    tn = is.na(mapped_concept_id) & is.na(target_concept_id),
    fn = !is.na(mapped_concept_id) & is.na(target_concept_id),
    fp = is.na(mapped_concept_id) & !is.na(target_concept_id) | (!is.na(mapped_concept_id) & !is.na(target_concept_id) & mapped_concept_id!=target_concept_id),
    tp = (!is.na(mapped_concept_id) & !is.na(target_concept_id) & mapped_concept_id==target_concept_id)
  )
  return(
    tmp %>% summarise(tp = sum(tp), tn=sum(tn), fp=sum(fp), fn=sum(fn)) %>% mutate(sim = minSim, rank=maxRank)
  )
}

tmp2 <- confMatrixGenerator(0.5,20)
tmp2
tmp2$tp+tmp2$tn+tmp2$fn

# confMatrixInput <- tibble(rank=1:20) %>% merge(tibble(sim=), by=NULL)
# confMatrixInput <- tibble(rank=1:2) %>% merge(tibble(sim=seq(0.5,1,0.1)), by=NULL)
confMatrix = tibble()

for (sim in seq(0,1,0.02)) {
  for (rank in 1:20) {
    confMatrix <- confMatrix %>% bind_rows(confMatrixGenerator(sim,rank))
  }
}

fValue <- function(beta,tp,tn,fp,fn) { 
  return((1+beta^2)*tp / as.double((1+beta^2)*tp+(beta^2)*fn+fp))
}

confMatrix <- confMatrix %>% mutate(
  sources = tp+tn+fn,
  mcc = (tp*tn-fp*fn)/sqrt(as.double(tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)),
  sens = tp/as.double(tp+fn),
  spec = tn/as.double(tn+fp),
  precision = tp/as.double(tp+fp),
  recall = sens,
  acc = (tp+tn)/as.double(tp+tn+fp+fn),
  bmi = sens+spec-1,
  tpr = tp/as.double(tp+tn+fp+fn),
  fpr = fp/as.double(tp+tn+fp+fn),
  tnr = tn/as.double(tp+tn+fp+fn),
  fnr = fn/as.double(tp+tn+fp+fn),
  f1 = fValue(1,tp,tn,fp,fn),
  f2 = fValue(2,tp,tn,fp,fn),
  f4 = fValue(4,tp,tn,fp,fn),
  f8 = fValue(8,tp,tn,fp,fn)
)


# Plots ----
setwd("~/Dropbox/ngramTextMatching")

totalPlot <- ggplot(confMatrix, aes(x=rank,y=sim,fill=tp+fn, z=tp+fn))+geom_tile(show.legend=FALSE)+geom_contour2(colour='black')+
  geom_text_contour(stroke=0.1)+scale_fill_gradient(high="grey72", low="grey25")+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('total.png',width=7,height=5,units='in')
ggsave('total.svg',width=7,height=5,units='in')


tpPlot <- ggplot(confMatrix, aes(x=rank,y=sim,fill=tp, z=tp))+geom_tile(show.legend=FALSE)+geom_contour2(colour='black')+
  geom_text_contour(stroke=0.1)+scale_fill_gradient(high="green", low="grey75")+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('tp.png',width=7,height=5,units='in')
ggsave('tp.svg',width=7,height=5,units='in')

tnPlot <- ggplot(confMatrix, aes(x=rank,y=sim,fill=tn, z=tn))+geom_tile(show.legend=FALSE)+geom_contour2(colour='black')+
  geom_text_contour(stroke=0.1)+scale_fill_gradient(high="green", low="grey75")+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('tn.png',width=7,height=5,units='in')
ggsave('tn.svg',width=7,height=5,units='in')

fpPlot <- ggplot(confMatrix, aes(x=rank,y=sim,fill=fp, z=fp))+geom_tile(show.legend=FALSE)+geom_contour2(colour='black')+
  geom_text_contour(stroke=0.1)+scale_fill_gradient(high="red", low="grey75")+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('fp.png',width=7,height=5,units='in')
ggsave('fp.svg',width=7,height=5,units='in')

fnPlot <- ggplot(confMatrix, aes(x=rank,y=sim,fill=fn, z=fn))+geom_tile(show.legend=FALSE)+geom_contour2(colour='black')+
  geom_text_contour(stroke=0.1)+scale_fill_gradient(high="red", low="grey75")+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('fn.png',width=7,height=5,units='in')
ggsave('fn.svg',width=7,height=5,units='in')



ggplot(confMatrix, aes(x=rank,y=sim,fill=mcc, z=mcc))+geom_tile(show.legend=FALSE)+geom_contour(colour='black')+
  geom_text_contour(stroke=0.1)+scale_fill_gradient2(high="green", low="red", mid="grey75",midpoint=0, limits=c(-1,1))+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('matthewsCorrelationCoefficient.png',width=7,height=5,units='in')
ggsave('matthewsCorrelationCoefficient.svg',width=7,height=5,units='in')

ggplot(confMatrix, aes(x=rank,y=sim,fill=recall, z=recall))+geom_tile(show.legend=FALSE)+
  geom_contour2(breaks=seq(0.65,0.95,0.05),colour='black')+
  geom_text_contour(breaks=seq(0.65,0.95,0.05),stroke=0.1)+
  scale_fill_gradient(high="orange", low="grey75", limits=c(0.45,1))+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('recall.png',width=7,height=5,units='in')
ggsave('recall.svg',width=7,height=5,units='in')

ggplot(confMatrix, aes(x=rank,y=sim,fill=acc, z=acc))+geom_tile(show.legend=FALSE)+
  geom_contour2(#breaks=seq(0.65,0.95,0.05),
    colour='black')+
  geom_text_contour(#breaks=seq(0.65,0.95,0.05),
    stroke=0.1)+
  scale_fill_gradient(high="purple", low="grey75")+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('precision.png',width=7,height=5,units='in')
ggsave('precision.svg',width=7,height=5,units='in')

ggplot(confMatrix, aes(x=rank,y=sim,fill=f1, z=f1))+geom_tile(show.legend=FALSE)+geom_contour2(#breaks=seq(0.5,0.68,0.02),
  colour='black')+
  geom_text_contour(#breaks=seq(0.5,0.68,0.02),
    stroke=0.1)+scale_fill_gradient(high="blue", low="grey75")+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('f1.png',width=7,height=5,units='in')
ggsave('f1.svg',width=7,height=5,units='in')

ggplot(confMatrix, aes(x=rank,y=sim,fill=f2, z=f2))+geom_tile(show.legend=FALSE)+geom_contour2(#breaks=seq(0.5,0.68,0.02),
  colour='black')+
  geom_text_contour(#breaks=seq(0.5,0.68,0.02),
    stroke=0.1)+scale_fill_gradient(high="blue", low="grey75")+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('f2.png',width=7,height=5,units='in')
ggsave('f2.svg',width=7,height=5,units='in')

ggplot(confMatrix, aes(x=rank,y=sim,fill=f4, z=f4))+geom_tile(show.legend=FALSE)+geom_contour2(#breaks=seq(0.5,0.68,0.02),
  colour='black')+
  geom_text_contour(breaks=seq(0.65,0.95,0.05),
    stroke=0.1)+scale_fill_gradient(high="blue", low="grey75")+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('f4.png',width=7,height=5,units='in')
ggsave('f4.svg',width=7,height=5,units='in')

ggplot(confMatrix, aes(x=rank,y=sim,fill=f8, z=f8))+geom_tile(show.legend=FALSE)+geom_contour2(breaks=seq(0.75,0.99,0.02),
  colour='black')+
  geom_text_contour(breaks=seq(0.87,0.99,0.02),
    stroke=0.1)+scale_fill_gradient(high="blue", low="grey75")+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('f8.png',width=7,height=5,units='in')
ggsave('f8.svg',width=7,height=5,units='in')

# Unused ones ----

ggplot(confMatrix, aes(x=rank,y=sim,fill=bmi, z=bmi))+geom_tile(show.legend=FALSE)+geom_contour(colour='black')+
  geom_text_contour(stroke=0.1)+scale_fill_gradient2(high="green", low="red", mid="grey75",midpoint=0, limits=c(-1,1))+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('bookmakersInformedness.png',width=7,height=5,units='in')

ggplot(confMatrix %>% mutate(col = rgb(precision*63+192,192,(recall-0.5)/0.5*63+192,255,NULL,255)), aes(x=rank,y=sim))+
  geom_tile(aes(fill=col),show.legend=FALSE)+
  geom_contour2(aes(z=recall),breaks=seq(0.65,0.95,0.05),
                colour='blue')+
  geom_text_contour(aes(z=recall),breaks=seq(0.65,0.95,0.05),
                    stroke=0.1,colour='blue')+
  geom_contour(aes(z=precision),colour='red')+
  geom_text_contour(aes(z=precision),stroke=0.1,colour='red')+scale_fill_identity()+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('precisionRecall.png',width=7,height=5,units='in')

ggplot(confMatrix %>% mutate(col = rgb(192,spec*63+192,(recall-0.5)/0.5*63+192,255,NULL,255)), aes(x=rank,y=sim))+
  geom_tile(aes(fill=col),show.legend=FALSE)+
  geom_contour2(aes(z=recall),breaks=seq(0.65,0.95,0.05),colour='blue')+
  geom_text_contour(aes(z=recall),breaks=seq(0.65,0.95,0.05),stroke=0.1,colour='blue')+
  geom_contour(aes(z=spec),colour='green')+
  geom_text_contour(aes(z=spec),stroke=0.1,colour='green')+scale_fill_identity()+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('sensSpec.png',width=7,height=5,units='in')

ggplot(confMatrix, aes(x=rank,y=sim))+
  geom_tile(aes(fill=rgb(recall*0.25+0.75,0.75,mcc*0.25+0.75,1)),show.legend=FALSE)+
  geom_contour2(aes(z=recall),breaks=seq(0.65,0.95,0.05),colour='blue')+
  geom_text_contour(aes(z=recall),breaks=seq(0.65,0.95,0.05),stroke=0.1,colour='blue')+
  geom_contour(aes(z=mcc),colour='red')+
  geom_text_contour(aes(z=mcc),stroke=0.1,colour='red')+scale_fill_identity()+
  xlab('results rank')+ylab("similarity cutoff")
ggsave('recallMatthewsCorrelationCoefficent.png',width=7,height=5,units='in')

ggplot(confMatrix %>% filter(sim==0.5), aes(x=rank))+
  geom_line(aes(y=recall))

ggplot(confMatrix %>% filter(rank==20) %>% gather(key='metric',value='value',recall,precision,acc,mcc,bmi,spec), aes(x=sim))+
  geom_line(aes(y=value,colour=metric))
ggsave('metricsBySimilarityForRank20.png',width=7,height=5,units='in')

ggplot(confMatrix %>% filter(rank==2) %>% gather(key='metric',value='value',recall,precision,acc,mcc,bmi,spec), aes(x=sim))+
  geom_line(aes(y=value,colour=metric))
ggsave('metricsBySimilarityForRank2.png',width=7,height=5,units='in')

ggplot(confMatrix %>% filter(sim==0.5) %>% gather(key='metric',value='value',recall,precision,acc,mcc,bmi,spec), aes(x=rank))+
  geom_line(aes(y=value,colour=metric))
ggsave('metricsByRankForSimilarity05.png',width=7,height=5,units='in')

ggplot(confMatrix %>% filter(sim==0.3) %>% gather(key='metric',value='value',recall,precision,acc,mcc,bmi,spec), aes(x=rank))+
  geom_line(aes(y=value,colour=metric))
ggsave('metricsByRankForSimilarity03.png',width=7,height=5,units='in')

ggplot(confMatrix %>% filter(sim==0.5) %>% gather(key='metric',value='value',tp,fp,tn,fn), aes(x=rank))+
  geom_line(aes(y=value,colour=metric))
ggsave('rawByRankForSimilarity05.png',width=7,height=5,units='in')

ggplot(confMatrix %>% filter(sim==0.8) %>% gather(key='metric',value='value',tp,fp,tn,fn), aes(x=rank))+
  geom_line(aes(y=value,colour=metric))
ggsave('rawByRankForSimilarity07.png',width=7,height=5,units='in')

ggplot(confMatrix %>% filter(rank==20) %>% gather(key='metric',value='value',tp,fp,tn,fn), aes(x=sim))+
  geom_line(aes(y=value,colour=metric))
ggsave('rawBySimilarityForRank20.png',width=7,height=5,units='in')



# Qualitative ----

defaultLayout = function(...) {
  return( theme_article(...) %>% 
            set_width("400pt") %>%
            set_wrap(TRUE) %>%
            set_all_padding(everywhere,everywhere,2) %>%
            set_valign(everywhere,everywhere,"top") )
}
# TODO: source_concept_name = source_description_name
best_suggestion <- mappingDetail %>% 
  group_by(source_concept_id,source_term) %>%
  mutate(suggestions=n()) %>%
  top_n(5,desc(similarity)) %>%
  arrange(desc(similarity)) %>%
  summarize(
    suggestions=max(suggestions),
    bestSim=max(similarity),
    top5Sim=mean(similarity),
    best=first(target_term,order_by=desc(similarity)),
    top5=paste0(target_term,collapse="; ")
  ) %>% arrange(desc(top5Sim))

not_found_in_mapping <- 
  best_suggestion %>% anti_join(mappingGold %>% filter(!is.na(mapped_concept_id)), on='source_concept_id')


# ggplot(
# missed_by_count <- 
# defaultLayout(huxtable(
#   not_found_in_mapping %>%
#     mutate(suggestions = cut(suggestions,breaks=c(-Inf,0.5,1.5,2.5,3.5,4.5,Inf),labels=c(0,1,2,3,4,'>5')))
#     %>% group_by(suggestions) %>% summarise( count = n()) %>%
#     select("Number candidates"=suggestions, "Original search terms"=count),
#   add_colnames = TRUE
# ))
# quick_html(defaultLayout(missed_by_count),file='missedByCount.html',open=FALSE)
# quick_latex(defaultLayout(missed_by_count),file='missedByCount.tex',open=FALSE)

# ,
#aes(x=suggestions,y=count)
  #) + geom_histogram(stat = 'identity')

missed_with_suggestions <- defaultLayout(huxtable(
  not_found_in_mapping %>% ungroup() %>%
    top_n(5,bestSim) %>%
    arrange(desc(bestSim)) %>%
    select(
      "Original"=source_term,
      "Search hits"=suggestions,
      "Best 5 search result"=top5) ,
  add_colnames = TRUE
))
quick_html(missed_with_suggestions,file='missedWithSuggestions.html',open=FALSE)
quick_latex(missed_with_suggestions,file='missedWithSuggestions.tex',open=FALSE)

# missed_with_one_suggestion <- huxtable(
#   not_found_in_mapping %>%
#     filter(suggestions==1) %>%
#     top_n(-10,source_concept_name) %>%
#     arrange(source_concept_name) %>%
#     select(
#       "Original"=source_concept_name,
#       "Gold standard"=mapped_concept_name,
#       "Search hits"=suggestions,
#       "Best search result"=best),
#   add_colnames = TRUE
# )
# quick_html(defaultLayout(missed_with_one_suggestion),file='missedWithOneSuggestion.html',open=FALSE)
# quick_latex(defaultLayout(missed_with_one_suggestion),file='missedWithOneSuggestion.tex',open=FALSE)

# there are no items that are not mapped and have no suggestions (because similarity is so low.)
# mappingGold %>% filter(is.na(mapped_concept_id)) %>% anti_join(mappingDetail)
# 
# missed_without_suggestions <- huxtable(
#   not_found_in_mapping %>%
#     filter(suggestions==0) %>%
#     top_n(-10,source_concept_name) %>%
#     arrange(source_concept_name) %>%
#     select(
#       "Original"=source_concept_name,
#       "Gold standard"=mapped_concept_name,
#       "Search hits"=suggestions
#     ),
#   add_colnames = TRUE
# )
# quick_html(defaultLayout(missed_without_suggestions),file='missedWithoutSuggestions.html',open=FALSE)
# quick_latex(defaultLayout(missed_without_suggestions),file='missedWithoutSuggestions.tex',open=FALSE)
# 
